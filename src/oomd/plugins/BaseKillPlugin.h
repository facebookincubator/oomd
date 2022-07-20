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

#pragma once

#include <memory>
#include <optional>
#include <vector>
#include "oomd/CgroupContext.h"
#include "oomd/OomdContext.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/PrekillHook.h"
#include "oomd/include/CgroupPath.h"

namespace Oomd {

/*
 * This abstract base class provides an overridable set of methods that
 * enables reuse of kill mechanism code. All plugins that kill processes
 * need code that can traverse a cgroup directory and start killing PIDs.
 * There is no reason for every plugin to rewrite that code.
 *
 * Ideally all killing plugins should inherit from this base class.
 * If customized behavior is desired, then simply override the relevant
 * methods.
 */
class BaseKillPlugin : public Engine::BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& ctx) override;

  /*
   * Runs @param fn on every cgroup in cgroups_ and their descendants
   *
   * Useful when implementing prerun() to track historical stats of cgroups
   * plugin will use to rank for killing.
   * Respects `recursive` config automatically.
   */
  template <class Functor>
  void prerunOnCgroups(OomdContext& ctx, Functor&& fn) {
    // Order doesn't matter. Use DFS instead of BFS because we expect tree to be
    // shallower than wide.
    std::vector<OomdContext::ConstCgroupContextRef> unvisited;

    const auto& rootCgroups = ctx.addToCacheAndGet(cgroups_);
    std::move(
        rootCgroups.begin(), rootCgroups.end(), std::back_inserter(unvisited));

    while (!unvisited.empty()) {
      const CgroupContext& cgroupCtx = unvisited.back();
      unvisited.pop_back();

      if (recursive_ && !cgroupCtx.oom_group().value_or(false)) {
        const auto& children = ctx.addChildrenToCacheAndGet(cgroupCtx);
        std::move(
            children.begin(), children.end(), std::back_inserter(unvisited));
      }

      fn(cgroupCtx);
    }
  }

 protected:
  /*
   * Required implementation point for kill plugins
   *
   * @return cgroups in the order of worthiness to kill, best target first.
   * @param cgroups is an unordered set of cgroups to rank.
   *
   * rankForKilling() will typically @return its input in a different order, but
   * doesn't strictly have to. It may eg. return a subset of its input.
   *
   * @return a vector<ConstCgroupContextRef> instead of an
   * optional<ConstCgroupContextRef> because if killing the best-choice cgroup
   * fails, we'll try to kill the next-best, and so on down the list.
   *
   * The caller, BaseKillPlugin::run, implements "recurse", "dry", and "debug"
   * config and memory.oom.group support, so rankForKilling() doesn't have to.
   * What it passes as @param cgroups is the resolved "cgroup" config, or if
   * "recursive" is set, the children of the highest-ranked cgroup from the last
   * iteration.
   */
  virtual std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) = 0;

  /*
   * Override point to OLOG why plugin chose @param target to die.
   *
   * @param target is the cgroup to be killed
   * @param peers are the set of cgroups from which target was chosen. This may
   * be useful for logging eg. target's memory as a fraction of total memory
   * usage among the peer group. peers includes target. If recursive_ is set,
   * peers are often a target's siblings.
   *
   * @param peers is the same vec as was passed to rankForKilling() as @param
   * cgroups in the call when rankForKilling() returned @param target.
   * See KillMemoryGrowth for an example using siblings.
   */
  virtual void ologKillTarget(
      OomdContext& ctx,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& peers) = 0;

  BaseKillPlugin();

  using KillUuid = std::string;

  /*
   * Kills a cgroup
   *
   * @param target is the cgroup to kill
   * @param killUuid is the name of this kill to use in logs
   * @param dry sets whether or not we should actually issue SIGKILLs
   * @returns true if successfully killed anything
   */
  virtual bool tryToKillCgroup(
      const CgroupContext& target,
      const KillUuid& killUuid,
      bool dry);

  /*
   * Sends SIGKILL to every PID in @param procs
   */
  virtual int tryToKillPids(const std::vector<int>& procs);

  virtual KillUuid generateKillUuid() const;

  /*
   * get/set methods for xattrs values. Since manipulating extended attributes
   * requires root permission, we can't use ::get/setxattr in unit tests.
   */
  virtual std::string getxattr(
      const std::string& path,
      const std::string& attr);

  virtual bool setxattr(
      const std::string& path,
      const std::string& attr,
      const std::string& val);
  /*
   * Increments the "trusted.oomd_ooms" and "user.oomd_ooms" extended attribute
   * keys on @param cgroupPath
   */
  virtual void reportKillInitiationToXattr(const std::string& cgroupPath);

  /*
   * Increments the "trusted.oomd_kill" and "user.oomd_kill" extended attribute
   * keys by @param numProcsKilled on @param cgroupPath
   */
  virtual void reportKillCompletionToXattr(
      const std::string& cgroupPath,
      int numProcsKilled);

  /*
   * Sets the "trusted.oomd_kill_uuid" and "user.oomd_kill_uuid" extended
   * attribute keys to @param killUuid on @param cgroupPath
   */
  virtual void reportKillUuidToXattr(
      const std::string& cgroupPath,
      const std::string& killUuid);

  virtual void dumpKillInfo(
      const CgroupPath& killedGroup,
      std::optional<OomdContext::ConstCgroupContextRef> context,
      std::optional<OomdContext::ConstCgroupContextRef> killRoot,
      const ActionContext& actionContext,
      const std::string& killUuid,
      bool success,
      bool dry) const;

  /*
   * Override point for tests to control the clock
   */
  virtual bool pastPrekillHookTimeout(const OomdContext& ctx) const;

 private:
  virtual int getAndTryToKillPids(const CgroupContext& target);

  enum class KillResult {
    SUCCESS,
    FAILED,
    DEFER,
  };
  KillResult tryToKillSomething(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& initialCgroups);

  struct KillCandidate {
   public:
    OomdContext::ConstCgroupContextRef cgroupCtx;
    // .killRoot and .peers are for logging
    // .killRoot is for when recursive targeting is enabled. It is the ancestor
    // cgroup of .cgroupCtx that was targeted in the plugin's "cgroup" arg.
    // When recursive targeting is disabled, .killRoot == .cgroupCtx
    OomdContext::ConstCgroupContextRef killRoot;
    std::shared_ptr<std::vector<OomdContext::ConstCgroupContextRef>> peers;
  };
  KillResult resumeTryingToKillSomething(
      OomdContext& ctx,
      std::vector<KillCandidate> nextBestOptionStack,
      bool hasTriedToKillSomethingAlready);

  /*
   * Kills cgroup and logs a structured kill message to kmsg and stderr.
   * Returns false on failure.
   */
  bool tryToLogAndKillCgroup(
      const OomdContext& ctx,
      const KillCandidate& candidate);

  // SerializedKillCandidates may be held across intervals because unlike
  // KillCandidates, Serialized* versions do not hold CgroupContext refs.
  struct SerializedCgroupRef {
   public:
    CgroupPath path;
    // id=nullopt represents a deleted cgroup which is undeserializable
    std::optional<CgroupContext::Id> id;
  };
  struct SerializedKillCandidate {
   public:
    SerializedCgroupRef target;
    SerializedCgroupRef killRoot;
    std::shared_ptr<std::vector<SerializedCgroupRef>> peers;
  };
  KillResult resumeFromPrekillHook(OomdContext& ctx);

  std::unordered_set<CgroupPath> cgroups_;
  bool recursive_{false};
  std::optional<int> postActionDelay_{std::nullopt};
  bool dry_{false};
  bool alwaysContinue_{false};
  bool debug_{false};

  struct ActivePrekillHook {
   public:
    std::unique_ptr<Engine::PrekillHookInvocation> hookInvocation;
    SerializedKillCandidate intendedVictim;
    std::vector<SerializedKillCandidate> nextBestOptionStack;
  };
  std::optional<ActivePrekillHook> prekillHookState_{std::nullopt};
};

} // namespace Oomd
