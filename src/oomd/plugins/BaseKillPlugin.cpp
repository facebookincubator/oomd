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
#include <signal.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <algorithm>
#include <chrono>
#include <iomanip>
#include <map>
#include <memory>
#include <optional>
#include <string>
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
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

#ifndef __NR_process_mrelease
#define __NR_process_mrelease 448
#endif

namespace {
static int pidfd_open(pid_t pid, unsigned int flags) noexcept {
  return ::syscall(SYS_pidfd_open, pid, flags);
}

static int process_mrelease(int pidfd, unsigned int flags) noexcept {
  return ::syscall(__NR_process_mrelease, pidfd, flags);
}

static int getMsSince(std::chrono::steady_clock::time_point start) {
  return std::chrono::duration_cast<std::chrono::milliseconds>(
             std::chrono::steady_clock::now() - start)
      .count();
}

} // namespace

static auto constexpr kOomdKillInitiationTrustedXattr = "trusted.oomd_ooms";
static auto constexpr kOomdKillInitiationUserXattr = "user.oomd_ooms";
static auto constexpr kOomdKillCompletionTrustedXattr = "trusted.oomd_kill";
static auto constexpr kOomdKillCompletionUserXattr = "user.oomd_kill";
static auto constexpr kOomdKillUuidTrustedXattr = "trusted.oomd_kill_uuid";
static auto constexpr kOomdKillUuidUserXattr = "user.oomd_kill_uuid";

namespace Oomd {

int BaseKillPlugin::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  argParser_.addArgumentCustom(
      "cgroup", cgroups_, [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      });

  argParser_.addArgument("recursive", recursive_);
  argParser_.addArgumentCustom(
      "post_action_delay", postActionDelay_, PluginArgParser::parseUnsignedInt);
  argParser_.addArgument("dry", dry_);
  argParser_.addArgument("always_continue", alwaysContinue_);
  argParser_.addArgument("debug", debug_);
  argParser_.addArgument("kernelkill", kernelKill_);
  argParser_.addArgument("reap_memory", reapMemory_);

  if (!argParser_.parse(args)) {
    return 1;
  }

  // Success
  return 0;
}

Engine::PluginRet BaseKillPlugin::run(OomdContext& ctx) {
  KillResult ret;

  if (prekillHookState_) {
    ret = resumeFromPrekillHook(ctx);
  } else {
    ret = tryToKillSomething(ctx, ctx.addToCacheAndGet(cgroups_));
  }

  if (ret == KillResult::DEFER) {
    return Engine::PluginRet::ASYNC_PAUSED;
  }

  if (prekillHookState_ != std::nullopt) {
    OLOG << "Error: there shouldn't be a running prekill hook"
            " once we're done with a kill cycle";
    prekillHookState_ = std::nullopt;
  }

  if (ret == KillResult::FAILED || alwaysContinue_) {
    return Engine::PluginRet::CONTINUE;
  }

  auto ruleset = ctx.getInvokingRuleset();
  if (ruleset && postActionDelay_) {
    (*ruleset)->pause_actions(std::chrono::seconds(*postActionDelay_));
  }
  return Engine::PluginRet::STOP;
}

BaseKillPlugin::KillResult BaseKillPlugin::resumeFromPrekillHook(
    OomdContext& ctx) {
  OCHECK(prekillHookState_ != std::nullopt);

  if (prekillHookState_->hookInvocation->didFinish()) {
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

  auto deserializeCgroupRef = [&](const SerializedCgroupRef& sc)
      -> std::optional<OomdContext::ConstCgroupContextRef> {
    if (auto cgroupCtx = ctx.addToCacheAndGet(sc.path)) {
      // Check inodes match and not a re-created cgroup. nullopt ids mean
      // deleted cgroups, which are never equal to each other.
      auto id = cgroupCtx->get().id();
      if (id.has_value() && sc.id.has_value() && *id == *sc.id) {
        return cgroupCtx;
      }
    }
    return std::nullopt;
  };

  // memoize deserializePeerGroup by serialized peer group pointer
  std::map<
      std::vector<SerializedCgroupRef>*,
      std::shared_ptr<std::vector<OomdContext::ConstCgroupContextRef>>>
      memoizedPeerGroups;
  auto deserializePeerGroup =
      [&](std::vector<SerializedCgroupRef>* serializedPeers) {
        auto it = memoizedPeerGroups.find(serializedPeers);
        if (it != memoizedPeerGroups.end()) {
          return it->second;
        }
        auto deserializedPeers =
            std::make_shared<std::vector<OomdContext::ConstCgroupContextRef>>();
        for (const auto& peer : *serializedPeers) {
          if (auto peerCgroupCtx = deserializeCgroupRef(peer)) {
            deserializedPeers->emplace_back(*peerCgroupCtx);
          }
        }
        memoizedPeerGroups[serializedPeers] = deserializedPeers;
        return deserializedPeers;
      };

  auto deserializeKillCandidate =
      [&](const SerializedKillCandidate& skc) -> std::optional<KillCandidate> {
    if (auto candidateCtx = deserializeCgroupRef(skc.target)) {
      if (auto killRootCtx = deserializeCgroupRef(skc.killRoot)) {
        return KillCandidate{
            .cgroupCtx = *candidateCtx,
            .killRoot = *killRootCtx,
            .peers = deserializePeerGroup(skc.peers.get())};
      }
    }

    return std::nullopt;
  };

  // pull state out of prekillHookState and clear it to delete the invocation
  auto intendedVictim = std::move(prekillHookState_->intendedVictim);
  auto serializedNextBestOptionStack =
      std::move(prekillHookState_->nextBestOptionStack);
  prekillHookState_ = std::nullopt;

  // Try to kill intended victim
  if (auto intendedCandidate = deserializeKillCandidate(intendedVictim)) {
    if (tryToLogAndKillCgroup(ctx, *intendedCandidate)) {
      return KillResult::SUCCESS;
    }
  } else {
    // intendedCandidate isn't deserializable means someone else removed it
    // before we could. Consider that they did our job for us. If we still
    // need to kill something detectors will fire again in the next interval and
    // start a fresh kill cycle.
    return KillResult::FAILED;
  }

  std::vector<KillCandidate> nextBestOptionStack;
  for (const auto& skc : serializedNextBestOptionStack) {
    if (auto candidate = deserializeKillCandidate(skc)) {
      nextBestOptionStack.emplace_back(*candidate);
    } else {
      // candidate isn't deserializable means someone else killed it before we
      // got a chance to. resumeTryingToKillSomething gets to the point where it
      // would have killed candidate, consider the other folks to have done our
      // job for us. Don't keep going further down the DFS stack looking for
      // another cgroup to kill. This means removing all candidates *lower* in
      // the stack, or *earlier* in the vector.
      nextBestOptionStack.clear();
    }
  }
  serializedNextBestOptionStack.clear();

  return resumeTryingToKillSomething(ctx, std::move(nextBestOptionStack), true);
}

BaseKillPlugin::KillResult BaseKillPlugin::tryToKillSomething(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& initialCgroups) {
  std::vector<KillCandidate> nextBestOptionStack;

  auto sorted =
      std::make_shared<std::vector<OomdContext::ConstCgroupContextRef>>(
          rankForKilling(ctx, initialCgroups));
  OomdContext::dump(*sorted, !debug_);

  // push the lowest ranked sibling onto the nextBestOptionStack first, so
  // the highest ranked sibling is on top
  reverse(sorted->begin(), sorted->end());
  for (const auto& cgroupCtx : *sorted) {
    nextBestOptionStack.emplace_back(KillCandidate{
        .cgroupCtx = cgroupCtx,
        // killRoots are the initialCgroups
        .killRoot = cgroupCtx,
        .peers = sorted});
  }

  return resumeTryingToKillSomething(
      ctx, std::move(nextBestOptionStack), false);
}

// DFS down tree looking for best kill target. Keep a nextBestOptionStack
// (instead of just the current target) because if killing fails, we try the
// next-best target. This may involve backtracking up the tree. The
// nextBestOptionStack tracks (cgroup, siblings) because ologKillTarget needs
// to know what peers a cgroup was compared to when it was picked.
// initialCgroups are treated as siblings.
BaseKillPlugin::KillResult BaseKillPlugin::resumeTryingToKillSomething(
    OomdContext& ctx,
    std::vector<KillCandidate> nextBestOptionStack,
    bool hasTriedToKillSomethingAlready) {
  OCHECK_EXCEPT(
      prekillHookState_ == std::nullopt,
      std::runtime_error("Shouldn't be trying to kill anything while pre-kill"
                         " hook is still running"));

  std::optional<KillCandidate> firstKillCandidate = std::nullopt;

  while (!nextBestOptionStack.empty()) {
    const KillCandidate candidate = nextBestOptionStack.back();
    nextBestOptionStack.pop_back();

    bool mayRecurse =
        recursive_ && !candidate.cgroupCtx.get().oom_group().value_or(false);
    if (mayRecurse) {
      auto children = ctx.addChildrenToCacheAndGet(candidate.cgroupCtx.get());
      if (children.size() > 0) {
        ologKillTarget(ctx, candidate.cgroupCtx.get(), *candidate.peers);

        auto sorted =
            std::make_shared<std::vector<OomdContext::ConstCgroupContextRef>>(
                rankForKilling(ctx, children));

        OomdContext::dump(*sorted, !debug_);

        // push the lowest ranked sibling onto the nextBestOptionStack first,
        // so the highest ranked sibling is on top
        reverse(sorted->begin(), sorted->end());
        for (const auto& cgroupCtx : *sorted) {
          nextBestOptionStack.emplace_back(KillCandidate{
              .cgroupCtx = cgroupCtx,
              // killRoot is nullopt when peers are themselves
              // the roots, in the first call. Each cgroup is then
              // its own killRoot.
              .killRoot = candidate.killRoot,
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
    // There is another scenario where we killed a cgroup but the processes do
    // not go away because init is stuck or process stuck in D state etc. With
    // process_mrelease, we can release all anon memory of the processes
    // regardless of their states. Skip such cgroup if it has zero anon usage.
    if (!candidate.cgroupCtx.get().is_populated().value_or(true) ||
        candidate.cgroupCtx.get().anon_usage() == 0) {
      if (!hasTriedToKillSomethingAlready && !firstKillCandidate) {
        firstKillCandidate = candidate;
      }
      continue;
    }

    ologKillTarget(ctx, candidate.cgroupCtx.get(), *candidate.peers);

    if (!pastPrekillHookTimeout(ctx)) {
      auto hookInvocation = ctx.firePrekillHook(candidate.cgroupCtx.get());
      if (hookInvocation && !(*hookInvocation)->didFinish()) {
        auto serializeCgroupRef = [&](const CgroupContext& cgroupCtx) {
          // cgroupCtx.id() may be nullopt, which means the cgroup is deleted
          return SerializedCgroupRef{
              .path = cgroupCtx.cgroup(), .id = cgroupCtx.id()};
        };

        // memoize serializePeerGroup by unserialized peer group pointer
        std::map<
            const std::vector<OomdContext::ConstCgroupContextRef>*,
            std::shared_ptr<std::vector<SerializedCgroupRef>>>
            memoizedPeerGroups;
        auto serializePeerGroup =
            [&](const std::vector<OomdContext::ConstCgroupContextRef>* peers) {
              auto it = memoizedPeerGroups.find(peers);
              if (it != memoizedPeerGroups.end()) {
                return it->second;
              }
              auto serializedPeers =
                  std::make_shared<std::vector<SerializedCgroupRef>>();
              for (const auto& peer : *peers) {
                serializedPeers->emplace_back(serializeCgroupRef(peer));
              }
              memoizedPeerGroups[peers] = serializedPeers;
              return serializedPeers;
            };

        auto serializeKillCandidate = [&](const KillCandidate& kc) {
          return SerializedKillCandidate{
              .target = serializeCgroupRef(kc.cgroupCtx),
              .killRoot = serializeCgroupRef(kc.cgroupCtx),
              .peers = serializePeerGroup(kc.peers.get())};
        };

        prekillHookState_ = ActivePrekillHook{
            .hookInvocation = std::move(*hookInvocation),
            .intendedVictim = serializeKillCandidate(candidate)};

        for (KillCandidate& kc : nextBestOptionStack) {
          prekillHookState_->nextBestOptionStack.emplace_back(
              serializeKillCandidate(kc));
        }

        return KillResult::DEFER;
      }
    }

    hasTriedToKillSomethingAlready = true;
    if (tryToLogAndKillCgroup(ctx, candidate)) {
      return KillResult::SUCCESS;
    }
  }

  if (!hasTriedToKillSomethingAlready) {
    KillUuid killUuid = generateKillUuid();
    auto actionContext = ctx.getActionContext();

    // We haven't tried to kill anything yet. If firstKillCandidate is unset,
    // then nextBestOptionStack must be provided empty, likely because plugin's
    // rankForKilling() returns empty result, i.e. all candidates have low usage
    // of the threshold resource. If firstKillCandidate is set, then all the
    // candidates must have no process inside.
    auto errorMsg = firstKillCandidate.has_value()
        ? "All kill candidate cgroups are empty"
        : "No kill candidate cgroups";
    dumpKillInfo(
        firstKillCandidate, actionContext, killUuid, false, dry_, errorMsg, {});
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
  static constexpr size_t streamSize = 20;
  int nrKilled = 0;

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
    if (pids.size() == streamSize) {
      nrKilled += tryToKillPids(pids);
      pids.clear();
    }
  }
  nrKilled += tryToKillPids(pids);
  ::free(line);
  ::fclose(fp);

  // target.children is cached, and may be stale
  if (const auto& children = target.children()) {
    for (const auto& childName : *children) {
      if (auto childCtx =
              target.oomd_ctx().addChildToCacheAndGet(target, childName)) {
        nrKilled += getAndTryToKillPids(*childCtx);
      }
    }
  }

  return nrKilled;
}

int BaseKillPlugin::dumpMemoryStat(const CgroupContext& target) {
  auto stats =
      Fs::readFileByLine(Fs::Fd::openat(target.fd(), Fs::kMemStatFile));
  if (!stats) {
    return 1;
  }
  OLOG << "memory.stat for " + target.cgroup().relativePath() + ": ";
  for (auto& line : *stats) {
    OLOG << line;
  }

  return 0;
}

bool BaseKillPlugin::reapProcess(pid_t pid) {
  const int pidfd = ::pidfd_open(pid, 0);
  if (pidfd < 0) {
    if (errno != ESRCH) {
      OLOG << "pidfd_open " << pid << " failed: " << Util::strerror_r();
    }
    return false;
  }
  OOMD_SCOPE_EXIT {
    ::close(pidfd);
  };

  if (::process_mrelease(pidfd, 0) < 0) {
    if (errno != ESRCH) {
      OLOG << "process_mrelease " << pid << " failed: " << Util::strerror_r();
    }
    return false;
  }
  return true;
}

int BaseKillPlugin::reapCgroupRecursively(const CgroupContext& target) {
  int reaped = 0;
  if (const auto& children = target.children()) {
    for (const auto& childName : *children) {
      if (auto childCtx =
              target.oomd_ctx().addChildToCacheAndGet(target, childName)) {
        reaped += reapCgroupRecursively(*childCtx);
      }
    }
  }

  if (const auto& pids = Fs::getPidsAt(target.fd())) {
    for (int pid : *pids) {
      reaped += reapProcess(pid);
    }
  }

  return reaped;
}

SystemMaybe<int> BaseKillPlugin::tryToKillCgroup(
    const CgroupContext& target,
    const KillUuid& killUuid,
    bool dry,
    KillCgroupStats& stats) {
  using namespace std::chrono_literals;
  const auto killStart = std::chrono::steady_clock::now();

  const std::string& cgroupPath = target.cgroup().absolutePath();

  int lastNrKilled = 0;
  int nrKilled = 0;
  int tries = 10;

  if (dry) {
    OLOG << "OOMD: In dry-run mode; would have tried to kill " << cgroupPath;
    return true;
  }

  if (kernelKill_) {
    OLOG << "Trying to kill " << cgroupPath
         << " with cgroup.freeze and cgroup.kill";
  } else {
    OLOG << "Trying to kill " << cgroupPath;
  }

  if (dumpMemoryStat(target)) {
    OLOG << "Failed to open " << cgroupPath << "/memory.stat";
  }

  reportKillUuidToXattr(cgroupPath, killUuid);
  reportKillInitiationToXattr(cgroupPath);

  if (kernelKill_) {
    if (auto maybeFreezed = Fs::writeFreezeAt(target.fd(), 1); !maybeFreezed) {
      OLOG << "Failed to freeze cgroup " << cgroupPath
           << ", proceed to kill without freezing: "
           << maybeFreezed.error().what();
    }

    auto procsBeforeKill = Fs::readPidsCurrentAt(target.fd());
    auto populated = Fs::readIsPopulatedAt(target.fd());

    if (!populated) {
      return SYSTEM_ERROR(populated.error(), "Failed to read cgroup.events");
    }
    if (!populated.value()) {
      return 0;
    }
    auto maybeKilled = Fs::writeKillAt(target.fd());
    if (!maybeKilled) {
      return SYSTEM_ERROR(
          maybeKilled.error(), "Failed to kill cgroup with cgroup.kill");
    }
    if (!procsBeforeKill || procsBeforeKill.value() == 0) {
      // Placeholder if we cannot figure out how many we actually killed
      nrKilled = 1;
    } else {
      nrKilled = procsBeforeKill.value();
    }
  } else {
    while (tries--) {
      // Descendent cgroups created during killing will be missed because
      // getAndTryToKillPids reads cgroup children from OomdContext's cache

      nrKilled += getAndTryToKillPids(target);

      if (nrKilled == lastNrKilled) {
        break;
      }

      // Give it a breather before killing again
      //
      // Don't sleep after the first round of kills b/c the majority of the
      // time the sleep isn't necessary. The system responds fast enough.
      if (lastNrKilled) {
        std::this_thread::sleep_for(1s);
      }

      lastNrKilled = nrKilled;
    }
  }

  if (reapMemory_ && nrKilled > 0) {
    OLOG << "Reaping processes in " << target.cgroup().absolutePath();
    const auto reapStart = std::chrono::steady_clock::now();
    stats.nrReaped = reapCgroupRecursively(target);
    stats.reapDurMs = getMsSince(reapStart);
    OLOG << "Reaped " << *stats.nrReaped << " processes in " << *stats.reapDurMs
         << "ms";
  }

  reportKillCompletionToXattr(cgroupPath, nrKilled);

  stats.totalDurMs = getMsSince(killStart);
  return nrKilled;
}

int BaseKillPlugin::tryToKillPids(const std::vector<int>& pids) {
  std::ostringstream buf;
  int nrKilled = 0;

  for (int pid : pids) {
    auto statusPath = std::string("/proc/") + std::to_string(pid) + "/status";
    auto status = Fs::readFileByLine(statusPath);
    // Ignore dead processes. Their parent process will reap them.
    bool dead = false;
    if (status) {
      for (auto& line : *status) {
        if (line.starts_with("State:")) {
          if (line.ends_with("X (dead)") || line.ends_with("Z (zombie)")) {
            dead = true;
          }
          break;
        }
      }
    }
    if (dead) {
      continue;
    }

    auto commPath = std::string("/proc/") + std::to_string(pid) + "/comm";
    auto comm = Fs::readFileByLine(commPath);

    if (comm && comm->size()) {
      buf << " " << pid << "(" << comm.value()[0] << ")";
    } else {
      buf << " " << pid;
    }

    if (::kill(static_cast<pid_t>(pid), SIGKILL) == 0) {
      nrKilled++;
    } else {
      buf << "[E" << errno << "]";
    }
  }
  if (buf.tellp()) {
    OLOG << "Killed " << nrKilled << ":" << buf.str();
  }
  return nrKilled;
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
    const std::string& cgroupPath) {
  // Helper function that reports kill initiation to an extended attribute
  const auto reportKillHelperFunc = [this,
                                     &cgroupPath](const std::string& xattr) {
    auto prevXattrStr = getxattr(cgroupPath, xattr);
    const int prevXattr = std::stoi(prevXattrStr != "" ? prevXattrStr : "0");
    std::string newXattrStr = std::to_string(prevXattr + 1);

    if (setxattr(cgroupPath, xattr, newXattrStr)) {
      OLOG << "Set xattr " << xattr << "=" << newXattrStr << " on "
           << cgroupPath;
    }
  };
  reportKillHelperFunc(kOomdKillInitiationTrustedXattr);
  reportKillHelperFunc(kOomdKillInitiationUserXattr);
}

void BaseKillPlugin::reportKillCompletionToXattr(
    const std::string& cgroupPath,
    int numProcsKilled) {
  // Helper function that reports kill completion to an extended attribute
  const auto reportKillHelperFunc = [this, &cgroupPath, numProcsKilled](
                                        const std::string& xattr) {
    auto prevXattrStr = getxattr(cgroupPath, xattr);
    const int prevXattr = std::stoi(prevXattrStr != "" ? prevXattrStr : "0");
    std::string newXattrStr = std::to_string(prevXattr + numProcsKilled);

    if (setxattr(cgroupPath, xattr, newXattrStr)) {
      OLOG << "Set xattr " << xattr << "=" << newXattrStr << " on "
           << cgroupPath;
    }
  };

  reportKillHelperFunc(kOomdKillCompletionTrustedXattr);
  reportKillHelperFunc(kOomdKillCompletionUserXattr);
}

void BaseKillPlugin::reportKillUuidToXattr(
    const std::string& cgroupPath,
    const std::string& killUuid) {
  // Helper function that reports kill uuid to an extended attribute
  const auto reportKillHelperFunc = [this, &cgroupPath, &killUuid](
                                        const std::string& xattr) {
    if (setxattr(cgroupPath, xattr, killUuid)) {
      OLOG << "Set xattr " << xattr << "=" << killUuid << " on " << cgroupPath;
    }
  };

  reportKillHelperFunc(kOomdKillUuidTrustedXattr);
  reportKillHelperFunc(kOomdKillUuidUserXattr);
}

bool BaseKillPlugin::tryToLogAndKillCgroup(
    const OomdContext& ctx,
    const KillCandidate& candidate) {
  KillUuid killUuid = generateKillUuid();
  auto actionContext = ctx.getActionContext();
  auto& target = candidate.cgroupCtx.get();
  auto& cgroupPath = target.cgroup().relativePath();

  KillCgroupStats stats;
  auto maybeNrKilled = tryToKillCgroup(target, killUuid, dry_, stats);
  auto nrKilled = maybeNrKilled ? *maybeNrKilled : 0;

  std::optional<std::string> errorMsg = std::nullopt;
  if (!maybeNrKilled) {
    errorMsg = maybeNrKilled.error().what();
    OLOG << "Failed to kill cgroup " << cgroupPath << ": " << *errorMsg;
  } else if (*maybeNrKilled == 0) {
    OLOG << "No processed killed from " << cgroupPath;
  } else {
    auto memPressure = target.mem_pressure().value_or(ResourcePressure{});
    std::ostringstream oss;
    oss << std::setprecision(2) << std::fixed;
    oss << memPressure.sec_10 << " " << memPressure.sec_60 << " "
        << memPressure.sec_300 << " " << cgroupPath << " "
        << target.current_usage().value_or(0) << " " << "ruleset:["
        << actionContext.ruleset_name << "] " << "detectorgroup:["
        << actionContext.detectorgroup << "] "
        << "killer:" << (dry_ ? "(dry)" : "") << getName() << " v2";
    if (!dry_) {
      Oomd::incrementStat(CoreStats::kKillsKey, 1);
    }
    OOMD_KMSG_LOG(oss.str(), "oomd kill");
  }

  dumpKillInfo(
      candidate, actionContext, killUuid, nrKilled, dry_, errorMsg, stats);

  return nrKilled > 0;
}
} // namespace Oomd
