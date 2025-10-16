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

#include <chrono>

#include "oomd/OomdContext.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/DetectorGroup.h"

namespace Oomd {
namespace Engine {

#define DEFAULT_POST_ACTION_DELAY 15
#define DEFAULT_PREKILL_HOOK_TIMEOUT 5

class Ruleset {
 public:
  Ruleset(
      const std::string& name,
      std::vector<std::unique_ptr<DetectorGroup>> detector_groups,
      std::vector<std::unique_ptr<BasePlugin>> action_group,
      bool disable_on_drop_in = false,
      bool detectorgroups_dropin_enabled = false,
      bool actiongroup_dropin_enabled = false,
      uint32_t silenced_logs = 0,
      int post_action_delay = DEFAULT_POST_ACTION_DELAY,
      int prekill_hook_timeout = DEFAULT_PREKILL_HOOK_TIMEOUT,
      const std::string& xattr_filter = "",
      const std::string& cgroup_fs = "",
      const std::string& cgroup = "");
  Ruleset(
      const std::string& name,
      std::vector<std::unique_ptr<DetectorGroup>> detector_groups,
      std::vector<std::unique_ptr<BasePlugin>> action_group,
      bool disable_on_drop_in,
      bool detectorgroups_dropin_enabled,
      bool actiongroup_dropin_enabled,
      uint32_t silenced_logs,
      int post_action_delay,
      int prekill_hook_timeout,
      std::unordered_set<CgroupPath>);
  ~Ruleset() = default;

  /*
   * Merges this ruleset with @param ruleset with priority given to @param
   * ruleset.
   *
   * @returns false if @param ruleset tries to override configs to ruleset
   * members which do not have drop in turned on. true otherwise.
   */
  [[nodiscard]] bool mergeWithDropIn(std::unique_ptr<Ruleset> ruleset);

  /*
   * Mark/unmark this ruleset as being targeted by an active drop in.
   */
  void markDropInTargeted();
  void markDropInUntargeted();

  /*
   * Prerun all plugins in this ruleset.
   */
  void prerun(OomdContext& context);

  /*
   * Runs the all the DetectorGroup's. If any of them fires, then begin
   * running the action chain.
   *
   * @returns 1 if we attempted to run the action chain. 0 otherwise.
   */
  uint32_t runOnce(OomdContext& context);

  const std::string& getName() const {
    return name_;
  }

  /*
   * for the next @param duration seconds, runOnce wont run the action chain,
   * even if the DetectorGroups fire.
   */
  void pause_actions(std::chrono::seconds duration);

 private:
  uint32_t runOnceImpl(OomdContext& context);
  void registerRunnableRulesetForCgroupPath(
      OomdContext& context,
      const CgroupPath& cgroup_path);

  std::string name_;
  std::vector<std::unique_ptr<DetectorGroup>> detector_groups_;
  std::vector<std::unique_ptr<BasePlugin>> action_group_;
  int post_action_delay_{DEFAULT_POST_ACTION_DELAY};
  int prekill_hook_timeout_{DEFAULT_PREKILL_HOOK_TIMEOUT};
  bool enabled_{true};
  bool disable_on_drop_in_{false};
  bool detectorgroups_dropin_enabled_{false};
  bool actiongroup_dropin_enabled_{false};
  uint32_t silenced_logs_{0};
  int32_t numTargeted_{0};
  std::string xattr_filter_;
  std::optional<std::unordered_set<CgroupPath>> cgroups_{std::nullopt};
  std::unordered_map<std::string, std::unique_ptr<Ruleset>> runnable_rulesets_;

  struct AsyncActionChainState {
   public:
    std::reference_wrapper<BasePlugin> active_plugin;
    ActionContext action_context;
  };
  std::optional<AsyncActionChainState> active_action_chain_state_{std::nullopt};
  uint32_t run_action_chain(
      std::vector<std::unique_ptr<BasePlugin>>::iterator action_chain_start,
      std::vector<std::unique_ptr<BasePlugin>>::iterator action_chain_end,
      OomdContext& context);

  std::chrono::steady_clock::time_point pause_actions_until_ =
      std::chrono::steady_clock::time_point();
  bool plugin_overrode_post_action_delay_{false};
};

} // namespace Engine
} // namespace Oomd
