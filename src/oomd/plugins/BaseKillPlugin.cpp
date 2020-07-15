/*
 * Copyright (C) 2018-present, Facebook, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "oomd/plugins/BaseKillPlugin.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <csignal>
#include <fstream>
#include <iomanip>
#include <stack>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "oomd/Log.h"
#include "oomd/Stats.h"
#include "oomd/include/CoreStats.h"
#include "oomd/include/Types.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

static auto constexpr kOomdKillInitiationXattr = "trusted.oomd_ooms";
static auto constexpr kOomdKillCompletionXattr = "trusted.oomd_kill";
static auto constexpr kOomdKillUuidXattr = "trusted.oomd_kill_uuid";

namespace Oomd {

int BaseKillPlugin::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  if (args.find("cgroup") != args.end()) {
    const auto& cgroup_fs = context.cgroupFs();

    auto cgroups = Util::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
      cgroups_.emplace(cgroup_fs, c);
    }
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  if (args.find("recursive") != args.end()) {
    const std::string& val = args.at("recursive");

    if (val == "true" || val == "True" || val == "1") {
      recursive_ = true;
    }
  }

  if (args.find("post_action_delay") != args.end()) {
    int val = std::stoi(args.at("post_action_delay"));

    if (val < 0) {
      OLOG << "Argument=post_action_delay must be non-negative";
      return 1;
    }

    post_action_delay_ = val;
  }

  if (args.find("dry") != args.end()) {
    const std::string& val = args.at("dry");

    if (val == "true" || val == "True" || val == "1") {
      dry_ = true;
    }
  }

  if (args.find("always_continue") != args.end()) {
    const std::string& val = args.at("always_continue");

    if (val == "true" || val == "True" || val == "1") {
      always_continue_ = true;
    }
  }

  if (args.find("debug") != args.end()) {
    const std::string& val = args.at("debug");

    if (val == "true" || val == "True" || val == "1") {
      debug_ = true;
    }
  }

  // Success
  return 0;
}

Engine::PluginRet BaseKillPlugin::run(OomdContext& ctx) {
  auto cgroups = ctx.addToCacheAndGet(cgroups_);
  bool ret = tryToKillSomething(ctx, std::move(cgroups));

  if (ret) {
    std::this_thread::sleep_for(std::chrono::seconds(post_action_delay_));
    if (always_continue_) {
      return Engine::PluginRet::CONTINUE;
    }
    return Engine::PluginRet::STOP;
  } else {
    return Engine::PluginRet::CONTINUE;
  }
}

bool BaseKillPlugin::tryToKillSomething(
    OomdContext& ctx,
    std::vector<OomdContext::ConstCgroupContextRef>&& initial_cgroups) {
  // DFS down tree looking for best kill target. Keep a stack (instead of just
  // the current target) because if killing fails, we try the next-best target.
  // This may involve backtracking up the tree.
  // The stack tracks (cgroup, siblings) because ologKillTarget needs to know
  // what peers a cgroup was compared to when it was picked.
  // initial_cgroups are treated as siblings.
  std::stack<std::pair<
      OomdContext::ConstCgroupContextRef,
      std::shared_ptr<const std::vector<OomdContext::ConstCgroupContextRef>>>>
      stack;

  auto push_siblings_onto_stack =
      [&](std::vector<OomdContext::ConstCgroupContextRef>&& peers) {
        auto shared_peers = std::make_shared<
            const std::vector<OomdContext::ConstCgroupContextRef>>(
            std::move(peers));

        std::vector<OomdContext::ConstCgroupContextRef> sorted =
            rankForKilling(ctx, *shared_peers);

        OomdContext::dump(sorted, !debug_);

        // push the lowest ranked sibling onto the stack first, so the highest
        // ranked sibling is on top
        reverse(sorted.begin(), sorted.end());
        for (const auto& cgroup_ctx : sorted) {
          stack.emplace(std::make_pair(cgroup_ctx, shared_peers));
        }
      };

  push_siblings_onto_stack(std::move(initial_cgroups));

  while (!stack.empty()) {
    const CgroupContext& cgroup_ctx = stack.top().first;
    auto peers = stack.top().second;
    stack.pop();

    bool may_recurse = recursive_ && !cgroup_ctx.oom_group().value_or(false);
    if (may_recurse) {
      auto children = ctx.addToCacheAndGetChildren(cgroup_ctx);
      if (children.size() > 0) {
        ologKillTarget(ctx, cgroup_ctx, *peers);
        push_siblings_onto_stack(std::move(children));
        continue;
      }
    }

    // Skip trying to kill an empty cgroup, which would unfairly increment the
    // empty cgroup's kill counters and pollute the logs. We get into a
    // situation where we try to kill empty cgroups when a cgroup marked
    // PREFER is not the source of pressure: KillMemoryGrowth will kill the
    // PREFER cgroup first, but that won't fix the problem so it will kill
    // again; on the second time around, it first targets the now-empty PREFER
    // cgroup before moving on to a better victim.
    if (!cgroup_ctx.is_populated().value_or(true)) {
      continue;
    }

    ologKillTarget(ctx, cgroup_ctx, *peers);

    if (auto kill_uuid =
            tryToKillCgroup(cgroup_ctx.cgroup().absolutePath(), true, dry_)) {
      logKill(
          cgroup_ctx.cgroup(),
          cgroup_ctx,
          ctx.getActionContext(),
          *kill_uuid,
          dry_);
      return true;
    }
  }

  return false;
}

BaseKillPlugin::BaseKillPlugin() {
  /*
   * Initializes kKillsKey in stats for immediate reporting,
   * rather than waiting for first occurrence
   */
  Oomd::setStat(CoreStats::kKillsKey, 0);
}

int BaseKillPlugin::getAndTryToKillPids(
    const std::string& path,
    bool recursive,
    size_t stream_size) {
  int nr_killed = 0;

  std::ifstream f(path + "/" + Fs::kProcsFile, std::ios::in);
  if (!f.is_open()) {
    OLOG << "Unable to open " << path;
    return 0;
  }
  while (!f.eof()) {
    std::string s;
    std::vector<int> pids;
    while (std::getline(f, s)) {
      pids.push_back(std::stoi(s));
      if (pids.size() == stream_size) {
        break;
      }
    }
    nr_killed += tryToKillPids(pids);
    if (f.bad()) {
      OLOG << "Error while processing file " << path;
      // Most likely the cgroup is dead and gone
      break;
    }
  }

  if (recursive) {
    auto de = Fs::readDir(path, Fs::DE_DIR);
    for (const auto& dir : de.dirs) {
      nr_killed += getAndTryToKillPids(path + "/" + dir, true, stream_size);
    }
  }
  return nr_killed;
}

std::optional<BaseKillPlugin::KillUuid> BaseKillPlugin::tryToKillCgroup(
    const std::string& cgroup_path,
    bool recursive,
    bool dry) {
  using namespace std::chrono_literals;

  int last_nr_killed = 0;
  int nr_killed = 0;
  int tries = 10;
  static constexpr size_t stream_size = 20;

  KillUuid kill_uuid = generateKillUuid();

  if (dry) {
    OLOG << "OOMD: In dry-run mode; would have tried to kill " << cgroup_path;
    return kill_uuid;
  }

  OLOG << "Trying to kill " << cgroup_path;

  reportKillUuidToXattr(cgroup_path, kill_uuid);
  reportKillInitiationToXattr(cgroup_path);
  while (tries--) {
    nr_killed += getAndTryToKillPids(cgroup_path, recursive, stream_size);

    if (nr_killed == last_nr_killed) {
      break;
    }

    // Give it a breather before killing again
    //
    // Don't sleep after the first round of kills b/c the majority of the
    // time the sleep isn't necessary. The system responds fast enough.
    if (last_nr_killed) {
      std::this_thread::sleep_for(1s);
    }

    last_nr_killed = nr_killed;
  }
  reportKillCompletionToXattr(cgroup_path, nr_killed);
  return nr_killed > 0 ? kill_uuid : std::optional<KillUuid>{};
}

int BaseKillPlugin::tryToKillPids(const std::vector<int>& pids) {
  std::ostringstream buf;
  int nr_killed = 0;

  for (int pid : pids) {
    auto comm_path = std::string("/proc/") + std::to_string(pid) + "/comm";
    auto comm = Fs::readFileByLine(comm_path);

    if (comm && comm.value().size()) {
      buf << " " << pid << "(" << comm.value()[0] << ")";
    } else {
      buf << " " << pid;
    }

    if (::kill(static_cast<pid_t>(pid), SIGKILL) == 0) {
      nr_killed++;
    } else {
      buf << "[E" << errno << "]";
    }
  }
  if (buf.tellp()) {
    OLOG << "Killed " << nr_killed << ":" << buf.str();
  }
  return nr_killed;
}

BaseKillPlugin::KillUuid BaseKillPlugin::generateKillUuid() const {
  return Util::generateUuid();
}

std::string BaseKillPlugin::getxattr(
    const std::string& path,
    const std::string& attr) {
  return Fs::getxattr(path, attr);
}

bool BaseKillPlugin::setxattr(
    const std::string& path,
    const std::string& attr,
    const std::string& val) {
  return Fs::setxattr(path, attr, val);
}

void BaseKillPlugin::reportKillInitiationToXattr(
    const std::string& cgroup_path) {
  auto prev_xattr_str = getxattr(cgroup_path, kOomdKillInitiationXattr);
  const int prev_xattr = std::stoi(prev_xattr_str != "" ? prev_xattr_str : "0");
  std::string new_xattr_str = std::to_string(prev_xattr + 1);

  if (setxattr(cgroup_path, kOomdKillInitiationXattr, new_xattr_str)) {
    OLOG << "Set xattr " << kOomdKillInitiationXattr << "=" << new_xattr_str
         << " on " << cgroup_path;
  }
}

void BaseKillPlugin::reportKillCompletionToXattr(
    const std::string& cgroup_path,
    int num_procs_killed) {
  auto prev_xattr_str = getxattr(cgroup_path, kOomdKillCompletionXattr);
  const int prev_xattr = std::stoi(prev_xattr_str != "" ? prev_xattr_str : "0");
  std::string new_xattr_str = std::to_string(prev_xattr + num_procs_killed);

  if (setxattr(cgroup_path, kOomdKillCompletionXattr, new_xattr_str)) {
    OLOG << "Set xattr " << kOomdKillCompletionXattr << "=" << new_xattr_str
         << " on " << cgroup_path;
  }
}

void BaseKillPlugin::reportKillUuidToXattr(
    const std::string& cgroup_path,
    const std::string& kill_uuid) {
  if (setxattr(cgroup_path, kOomdKillUuidXattr, kill_uuid)) {
    OLOG << "Set xattr " << kOomdKillUuidXattr << "=" << kill_uuid << " on "
         << cgroup_path;
  }
}

void BaseKillPlugin::logKill(
    const CgroupPath& killed_cgroup,
    const CgroupContext& context,
    const ActionContext& action_context,
    const std::string& kill_uuid,
    bool dry) const {
  auto mem_pressure = context.mem_pressure().value_or(ResourcePressure{});
  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << mem_pressure.sec_10 << " " << mem_pressure.sec_60 << " "
      << mem_pressure.sec_300 << " " << killed_cgroup.relativePath() << " "
      << context.current_usage().value_or(0) << " "
      << "ruleset:[" << action_context.ruleset << "] "
      << "detectorgroup:[" << action_context.detectorgroup << "] "
      << "killer:" << (dry ? "(dry)" : "") << getName() << " v2";
  Oomd::incrementStat(CoreStats::kKillsKey, 1);
  OOMD_KMSG_LOG(oss.str(), "oomd kill");

  dumpKillInfo(killed_cgroup, context, action_context, kill_uuid, dry);
}
} // namespace Oomd
