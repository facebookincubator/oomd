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
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "oomd/Log.h"
#include "oomd/OomdContext.h"
#include "oomd/Stats.h"
#include "oomd/engine/Ruleset.h"
#include "oomd/include/Assert.h"
#include "oomd/include/CgroupPath.h"
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
  argParser_.addArgumentCustom(
      "cgroup",
      cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      },
      true);

  argParser_.addArgument("recursive", recursive_);
  argParser_.addArgumentCustom(
      "post_action_delay",
      post_action_delay_,
      PluginArgParser::parseUnsignedInt);
  argParser_.addArgument("dry", dry_);
  argParser_.addArgument("always_continue", always_continue_);
  argParser_.addArgument("debug", debug_);

  if (!argParser_.parse(args)) {
    return 1;
  }

  // Success
  return 0;
}

Engine::PluginRet BaseKillPlugin::run(OomdContext& ctx) {
  KillResult ret;

  if (prekill_hook_state_) {
    ret = resumeFromPrekillHook(ctx);
  } else {
    ret = tryToKillSomething(ctx, ctx.addToCacheAndGet(cgroups_));
  }

  if (ret == KillResult::DEFER) {
    return Engine::PluginRet::ASYNC_PAUSED;
  }

  if (prekill_hook_state_ != std::nullopt) {
    OLOG << "Error: there shouldn't be a running prekill hook"
            " once we're done with a kill cycle";
    prekill_hook_state_ = std::nullopt;
  }

  if (ret == KillResult::FAILED || always_continue_) {
    return Engine::PluginRet::CONTINUE;
  }

  auto ruleset = ctx.getInvokingRuleset();
  if (ruleset && post_action_delay_) {
    (*ruleset)->pause_actions(std::chrono::seconds(*post_action_delay_));
  }
  return Engine::PluginRet::STOP;
}

BaseKillPlugin::KillResult BaseKillPlugin::resumeFromPrekillHook(
    OomdContext& ctx) {
  OCHECK(prekill_hook_state_ != std::nullopt);

  if (prekill_hook_state_->hook_invocation->didFinish()) {
    OLOG << "pre-kill hook finished for Ruleset="
         << ctx.getActionContext().ruleset_name;
  } else if (pastPrekillHookTimeout(ctx)) {
    OLOG << "pre-kill hook timed out for Ruleset="
         << ctx.getActionContext().ruleset_name;
  } else {
    OLOG << "Still running pre-kill hook for Ruleset="
         << ctx.getActionContext().ruleset_name;
    return KillResult::DEFER;
  }

  auto deserialize_cgroup_ref = [&](const SerializedCgroupRef& sc)
      -> std::optional<OomdContext::ConstCgroupContextRef> {
    if (auto cgroup_ctx = ctx.addToCacheAndGet(sc.path)) {
      // Check inodes match and not a re-created cgroup. nullopt ids mean
      // deleted cgroups, which are never equal to each other.
      auto id = cgroup_ctx->get().id();
      if (id.has_value() && sc.id.has_value() && *id == *sc.id) {
        return cgroup_ctx;
      }
    }
    return std::nullopt;
  };

  // memoize deserialize_peer_group by serialized peer group pointer
  std::map<
      std::vector<SerializedCgroupRef>*,
      std::shared_ptr<std::vector<OomdContext::ConstCgroupContextRef>>>
      memoized_peer_groups;
  auto deserialize_peer_group =
      [&](std::vector<SerializedCgroupRef>* serialized_peers) {
        auto it = memoized_peer_groups.find(serialized_peers);
        if (it != memoized_peer_groups.end()) {
          return it->second;
        }
        auto deserialized_peers =
            std::make_shared<std::vector<OomdContext::ConstCgroupContextRef>>();
        for (const auto& peer : *serialized_peers) {
          if (auto peer_cgroup_ctx = deserialize_cgroup_ref(peer)) {
            deserialized_peers->emplace_back(*peer_cgroup_ctx);
          }
        }
        memoized_peer_groups[serialized_peers] = deserialized_peers;
        return deserialized_peers;
      };

  auto deserialize_kill_candidate =
      [&](const SerializedKillCandidate& skc) -> std::optional<KillCandidate> {
    if (auto candidate_ctx = deserialize_cgroup_ref(skc.target)) {
      if (auto kill_root_ctx = deserialize_cgroup_ref(skc.kill_root)) {
        return KillCandidate{
            .cgroup_ctx = *candidate_ctx,
            .kill_root = *kill_root_ctx,
            .peers = deserialize_peer_group(skc.peers.get())};
      }
    }

    return std::nullopt;
  };

  // pull state out of prekill_hook_state and clear it to delete the invocation
  auto intended_victim = std::move(prekill_hook_state_->intended_victim);
  auto serialized_next_best_option_stack =
      std::move(prekill_hook_state_->next_best_option_stack);
  prekill_hook_state_ = std::nullopt;

  // Try to kill intended victim
  if (auto intended_candidate = deserialize_kill_candidate(intended_victim)) {
    if (tryToLogAndKillCgroup(ctx, *intended_candidate)) {
      return KillResult::SUCCESS;
    }
  } else {
    // intended_candidate isn't deserializable means someone else removed it
    // before we could. Consider that they did our job for us. If we still
    // need to kill something detectors will fire again in the next interval and
    // start a fresh kill cycle.
    return KillResult::FAILED;
  }

  std::vector<KillCandidate> next_best_option_stack;
  for (const auto& skc : serialized_next_best_option_stack) {
    if (auto candidate = deserialize_kill_candidate(skc)) {
      next_best_option_stack.emplace_back(*candidate);
    } else {
      // candidate isn't deserializable means someone else killed it before we
      // got a chance to. resumeTryingToKillSomething gets to the point where it
      // would have killed candidate, consider the other folks to have done our
      // job for us. Don't keep going further down the DFS stack looking for
      // another cgroup to kill. This means removing all candidates *lower* in
      // the stack, or *earlier* in the vector.
      next_best_option_stack.clear();
    }
  }
  serialized_next_best_option_stack.clear();

  return resumeTryingToKillSomething(ctx, std::move(next_best_option_stack));
}

BaseKillPlugin::KillResult BaseKillPlugin::tryToKillSomething(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& initial_cgroups) {
  std::vector<KillCandidate> next_best_option_stack;

  auto sorted =
      std::make_shared<std::vector<OomdContext::ConstCgroupContextRef>>(
          rankForKilling(ctx, initial_cgroups));
  OomdContext::dump(*sorted, !debug_);

  // push the lowest ranked sibling onto the next_best_option_stack first, so
  // the highest ranked sibling is on top
  reverse(sorted->begin(), sorted->end());
  for (const auto& cgroup_ctx : *sorted) {
    next_best_option_stack.emplace_back(KillCandidate{
        .cgroup_ctx = cgroup_ctx,
        // kill_roots are the initial_cgroups
        .kill_root = cgroup_ctx,
        .peers = sorted});
  }

  return resumeTryingToKillSomething(ctx, std::move(next_best_option_stack));
}

// DFS down tree looking for best kill target. Keep a next_best_option_stack
// (instead of just the current target) because if killing fails, we try the
// next-best target. This may involve backtracking up the tree. The
// next_best_option_stack tracks (cgroup, siblings) because ologKillTarget needs
// to know what peers a cgroup was compared to when it was picked.
// initial_cgroups are treated as siblings.
BaseKillPlugin::KillResult BaseKillPlugin::resumeTryingToKillSomething(
    OomdContext& ctx,
    std::vector<KillCandidate> next_best_option_stack) {
  OCHECK_EXCEPT(
      prekill_hook_state_ == std::nullopt,
      std::runtime_error("Shouldn't be trying to kill anything while pre-kill"
                         " hook is still running"));

  while (!next_best_option_stack.empty()) {
    const auto candidate = next_best_option_stack.back();
    next_best_option_stack.pop_back();

    bool may_recurse =
        recursive_ && !candidate.cgroup_ctx.oom_group().value_or(false);
    if (may_recurse) {
      auto children = ctx.addChildrenToCacheAndGet(candidate.cgroup_ctx);
      if (children.size() > 0) {
        ologKillTarget(ctx, candidate.cgroup_ctx, *candidate.peers);

        auto sorted =
            std::make_shared<std::vector<OomdContext::ConstCgroupContextRef>>(
                rankForKilling(ctx, children));

        OomdContext::dump(*sorted, !debug_);

        // push the lowest ranked sibling onto the next_best_option_stack first,
        // so the highest ranked sibling is on top
        reverse(sorted->begin(), sorted->end());
        for (const auto& cgroup_ctx : *sorted) {
          next_best_option_stack.emplace_back(KillCandidate{
              .cgroup_ctx = cgroup_ctx,
              // kill_root is nullopt when peers are themselves
              // the roots, in the first call. Each cgroup is then
              // its own kill_root.
              .kill_root = candidate.kill_root,
              .peers = sorted});
        }

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

    if (!pastPrekillHookTimeout(ctx)) {
      auto hook_invocation = ctx.firePrekillHook(candidate.cgroup_ctx);
      if (hook_invocation && !(*hook_invocation)->didFinish()) {
        auto serialize_cgroup_ref = [&](const CgroupContext& cgroup_ctx) {
          // cgroup_ctx.id() may be nullopt, which means the cgroup is deleted
          return SerializedCgroupRef{
              .path = cgroup_ctx.cgroup(), .id = cgroup_ctx.id()};
        };

        // memoize serialize_peer_group by unserialized peer group pointer
        std::map<
            const std::vector<OomdContext::ConstCgroupContextRef>*,
            std::shared_ptr<std::vector<SerializedCgroupRef>>>
            memoized_peer_groups;
        auto serialize_peer_group =
            [&](const std::vector<OomdContext::ConstCgroupContextRef>* peers) {
              auto it = memoized_peer_groups.find(peers);
              if (it != memoized_peer_groups.end()) {
                return it->second;
              }
              auto serialized_peers =
                  std::make_shared<std::vector<SerializedCgroupRef>>();
              for (const auto& peer : *peers) {
                serialized_peers->emplace_back(serialize_cgroup_ref(peer));
              }
              memoized_peer_groups[peers] = serialized_peers;
              return serialized_peers;
            };

        auto serialize_kill_candidate = [&](const KillCandidate& kc) {
          return SerializedKillCandidate{
              .target = serialize_cgroup_ref(kc.cgroup_ctx),
              .kill_root = serialize_cgroup_ref(kc.cgroup_ctx),
              .peers = serialize_peer_group(kc.peers.get())};
        };

        prekill_hook_state_ = ActivePrekillHook{
            .hook_invocation = std::move(*hook_invocation),
            .intended_victim = serialize_kill_candidate(candidate)};

        for (KillCandidate& kc : next_best_option_stack) {
          prekill_hook_state_->next_best_option_stack.emplace_back(
              serialize_kill_candidate(kc));
        }

        return KillResult::DEFER;
      }
    }

    if (tryToLogAndKillCgroup(ctx, candidate)) {
      return KillResult::SUCCESS;
    }
  }

  return KillResult::FAILED;
}

bool BaseKillPlugin::pastPrekillHookTimeout(const OomdContext& ctx) const {
  auto timeout = ctx.getActionContext().prekill_hook_timeout_ts;
  return timeout.has_value() && std::chrono::steady_clock::now() > timeout;
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

bool BaseKillPlugin::tryToKillCgroup(
    const CgroupContext& target,
    const KillUuid& kill_uuid,
    bool dry) {
  using namespace std::chrono_literals;

  const std::string& cgroup_path = target.cgroup().absolutePath();

  int last_nr_killed = 0;
  int nr_killed = 0;
  int tries = 10;

  if (dry) {
    OLOG << "OOMD: In dry-run mode; would have tried to kill " << cgroup_path;
    return true;
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
  return nr_killed > 0;
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

bool BaseKillPlugin::tryToLogAndKillCgroup(
    const OomdContext& ctx,
    const KillCandidate& candidate) {
  KillUuid kill_uuid = generateKillUuid();
  auto action_context = ctx.getActionContext();

  bool success = tryToKillCgroup(candidate.cgroup_ctx, kill_uuid, dry_);

  if (success) {
    auto mem_pressure =
        candidate.cgroup_ctx.mem_pressure().value_or(ResourcePressure{});
    std::ostringstream oss;
    oss << std::setprecision(2) << std::fixed;
    oss << mem_pressure.sec_10 << " " << mem_pressure.sec_60 << " "
        << mem_pressure.sec_300 << " "
        << candidate.cgroup_ctx.cgroup().relativePath() << " "
        << candidate.cgroup_ctx.current_usage().value_or(0) << " "
        << "ruleset:[" << action_context.ruleset_name << "] "
        << "detectorgroup:[" << action_context.detectorgroup << "] "
        << "killer:" << (dry_ ? "(dry)" : "") << getName() << " v2";
    if (!dry_) {
      Oomd::incrementStat(CoreStats::kKillsKey, 1);
    }
    OOMD_KMSG_LOG(oss.str(), "oomd kill");
  }

  dumpKillInfo(
      candidate.cgroup_ctx.cgroup(),
      candidate.cgroup_ctx,
      candidate.kill_root,
      action_context,
      kill_uuid,
      success,
      dry_);

  return success;
}
} // namespace Oomd
