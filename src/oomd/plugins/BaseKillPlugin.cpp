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

#include <fcntl.h>
#include <unistd.h>
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
#include "oomd/engine/Ruleset.h"
#include "oomd/include/Assert.h"
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

  if (!ret || always_continue_) {
    return Engine::PluginRet::CONTINUE;
  }
  if (auto ruleset = ctx.getActionContext().ruleset) {
    ruleset->pause_actions(std::chrono::seconds(post_action_delay_));
  }
  return Engine::PluginRet::STOP;
}

namespace {
struct KillCandidate {
 public:
  const CgroupContext& cgroup_ctx;
  // .kill_root and .peers are for logging
  // .kill_root is for when recursive targeting is enabled. It is the ancestor
  // cgroup of .cgroup_ctx that was targeted in the plugin's "cgroup" arg. When
  // recursive targeting is disabled, .kill_root == .cgroup_ctx
  const CgroupContext& kill_root;
  std::shared_ptr<const std::vector<OomdContext::ConstCgroupContextRef>> peers;
};
} // namespace

bool BaseKillPlugin::tryToKillSomething(
    OomdContext& ctx,
    std::vector<OomdContext::ConstCgroupContextRef>&& initial_cgroups) {
  // DFS down tree looking for best kill target. Keep a stack (instead of just
  // the current target) because if killing fails, we try the next-best target.
  // This may involve backtracking up the tree.
  // The stack tracks (cgroup, siblings) because ologKillTarget needs to know
  // what peers a cgroup was compared to when it was picked.
  // initial_cgroups are treated as siblings.
  std::stack<KillCandidate> stack;

  auto push_siblings_onto_stack =
      [&](std::vector<OomdContext::ConstCgroupContextRef>&& peers,
          std::optional<OomdContext::ConstCgroupContextRef> kill_root) {
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
          stack.emplace(KillCandidate{
              .cgroup_ctx = cgroup_ctx,
              // kill_root is nullopt when peers are themselves
              // the roots, in the first call. Each cgroup is then
              // its own kill_root.
              .kill_root = kill_root.value_or(cgroup_ctx),
              .peers = shared_peers});
        }
      };

  push_siblings_onto_stack(std::move(initial_cgroups), std::nullopt);

  while (!stack.empty()) {
    const auto candidate = stack.top();
    stack.pop();

    bool may_recurse =
        recursive_ && !candidate.cgroup_ctx.oom_group().value_or(false);
    if (may_recurse) {
      auto children = ctx.addChildrenToCacheAndGet(candidate.cgroup_ctx);
      if (children.size() > 0) {
        ologKillTarget(ctx, candidate.cgroup_ctx, *candidate.peers);
        push_siblings_onto_stack(std::move(children), candidate.kill_root);
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
    if (!candidate.cgroup_ctx.is_populated().value_or(true)) {
      continue;
    }

    ologKillTarget(ctx, candidate.cgroup_ctx, *candidate.peers);

    if (auto kill_uuid = tryToKillCgroup(candidate.cgroup_ctx, dry_)) {
      logKill(
          candidate.cgroup_ctx.cgroup(),
          candidate.cgroup_ctx,
          candidate.kill_root,
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

int BaseKillPlugin::getAndTryToKillPids(const CgroupContext& target) {
  static constexpr size_t stream_size = 20;
  int nr_killed = 0;

  const auto fd = ::openat(target.fd().fd(), Fs::kProcsFile, O_RDONLY);
  if (fd == -1) {
    return 0;
  }
  auto fp = ::fdopen(fd, "r");
  if (fp == nullptr) {
    ::close(fd);
    return 0;
  }
  std::vector<int> pids;
  char* line = nullptr;
  size_t len = 0;
  ssize_t read;
  errno = 0;
  while ((read = ::getline(&line, &len, fp)) != -1) {
    OCHECK(line != nullptr);
    pids.push_back(std::stoi(line));
    if (pids.size() == stream_size) {
      nr_killed += tryToKillPids(pids);
      pids.clear();
    }
  }
  nr_killed += tryToKillPids(pids);
  ::free(line);
  ::fclose(fp);

  // target.children is cached, and may be stale
  if (const auto& children = target.children()) {
    for (const auto& child_name : *children) {
      if (auto child_ctx =
              target.oomd_ctx().addChildToCacheAndGet(target, child_name)) {
        nr_killed += getAndTryToKillPids(*child_ctx);
      }
    }
  }

  return nr_killed;
}

std::optional<BaseKillPlugin::KillUuid> BaseKillPlugin::tryToKillCgroup(
    const CgroupContext& target,
    bool dry) {
  using namespace std::chrono_literals;

  const std::string& cgroup_path = target.cgroup().absolutePath();

  int last_nr_killed = 0;
  int nr_killed = 0;
  int tries = 10;

  KillUuid kill_uuid = generateKillUuid();

  if (dry) {
    OLOG << "OOMD: In dry-run mode; would have tried to kill " << cgroup_path;
    return kill_uuid;
  }

  OLOG << "Trying to kill " << cgroup_path;

  reportKillUuidToXattr(cgroup_path, kill_uuid);
  reportKillInitiationToXattr(cgroup_path);
  while (tries--) {
    // Descendent cgroups created during killing will be missed because
    // getAndTryToKillPids reads cgroup children from OomdContext's cache

    nr_killed += getAndTryToKillPids(target);

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

    if (comm && comm->size()) {
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
  auto ret = Fs::getxattr(path, attr);
  // TODO(dschatzberg): Report error
  if (!ret) {
    return "";
  }
  return *ret;
}

bool BaseKillPlugin::setxattr(
    const std::string& path,
    const std::string& attr,
    const std::string& val) {
  // TODO(dschatzberg): Report error
  if (!Fs::setxattr(path, attr, val)) {
    return false;
  }
  return true;
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
    const CgroupContext& kill_root,
    const ActionContext& action_context,
    const std::string& kill_uuid,
    bool dry) const {
  auto mem_pressure = context.mem_pressure().value_or(ResourcePressure{});
  auto ruleset =
      action_context.ruleset ? action_context.ruleset->getName() : "";
  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << mem_pressure.sec_10 << " " << mem_pressure.sec_60 << " "
      << mem_pressure.sec_300 << " " << killed_cgroup.relativePath() << " "
      << context.current_usage().value_or(0) << " "
      << "ruleset:[" << ruleset << "] "
      << "detectorgroup:[" << action_context.detectorgroup << "] "
      << "killer:" << (dry ? "(dry)" : "") << getName() << " v2";
  if (!dry) {
    Oomd::incrementStat(CoreStats::kKillsKey, 1);
  }
  OOMD_KMSG_LOG(oss.str(), "oomd kill");

  dumpKillInfo(
      killed_cgroup, context, kill_root, action_context, kill_uuid, dry);
}
} // namespace Oomd
