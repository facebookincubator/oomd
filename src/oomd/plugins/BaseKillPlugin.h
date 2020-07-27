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

#include "oomd/engine/BasePlugin.h"

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
class BaseKillPlugin : public Oomd::Engine::BasePlugin {
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

    const auto& root_cgroups = ctx.addToCacheAndGet(cgroups_);
    std::move(
        root_cgroups.begin(),
        root_cgroups.end(),
        std::back_inserter(unvisited));

    while (!unvisited.empty()) {
      const CgroupContext& cgroup_ctx = unvisited.back();
      unvisited.pop_back();

      if (recursive_ && !cgroup_ctx.oom_group().value_or(false)) {
        const auto& children = ctx.addChildrenToCacheAndGet(cgroup_ctx);
        std::move(
            children.begin(), children.end(), std::back_inserter(unvisited));
      }

      fn(cgroup_ctx);
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
   * @param cgroup_path is the absolute path to a cgroup (eg /sys/fs/cgroup/...)
   * @param recursive, if true, recursively kills every process (ie children
   * cgroups) starting at @param cgroup_path
   * @param dry sets whether or not we should actually issue SIGKILLs
   */
  virtual std::optional<KillUuid>
  tryToKillCgroup(const std::string& cgroup_path, bool recursive, bool dry);

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
   * Increments the "trusted.oomd_ooms" extended attribute key on @param
   * cgroup_path
   */
  virtual void reportKillInitiationToXattr(const std::string& cgroup_path);

  /*
   * Increments the "trusted.oomd_kill" extended attribute key by @param
   * num_procs_killed on @param cgroup_path
   */
  virtual void reportKillCompletionToXattr(
      const std::string& cgroup_path,
      int num_procs_killed);

  /*
   * Sets the "trusted.oomd_kill_uuid" extended attribute key to @param
   * kill_uuid on @param cgroup_path
   */
  virtual void reportKillUuidToXattr(
      const std::string& cgroup_path,
      const std::string& kill_uuid);

  /*
   * Logs a structured kill message to kmsg and stderr
   */
  virtual void logKill(
      const CgroupPath& killed_group,
      const CgroupContext& context,
      const ActionContext& action_context,
      const std::string& kill_uuid,
      bool dry = false) const;

  virtual void dumpKillInfo(
      const CgroupPath& killed_group,
      const CgroupContext& context,
      const ActionContext& action_context,
      const std::string& kill_uuid,
      bool dry = false) const;

 private:
  virtual int getAndTryToKillPids(
      const std::string& path,
      bool recursive,
      size_t stream_size);

  bool tryToKillSomething(
      OomdContext& ctx,
      std::vector<OomdContext::ConstCgroupContextRef>&& cgroups);

  std::unordered_set<CgroupPath> cgroups_;
  bool recursive_{false};
  int post_action_delay_{15};
  bool dry_{false};
  bool always_continue_{false};
  bool debug_{false};
};

} // namespace Oomd
