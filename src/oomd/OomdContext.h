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

#include <sys/types.h>
#include <algorithm>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "oomd/CgroupContext.h"
#include "oomd/engine/PrekillHook.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/Types.h"

namespace Oomd {

namespace Engine {
class Ruleset;
class PrekillHook;
class PrekillHookInvocation;
} // namespace Engine

struct ActionContext {
  std::string ruleset_name;
  std::string detectorgroup;
  std::string action_group_run_uuid;
  std::optional<std::chrono::steady_clock::time_point> prekill_hook_timeout_ts{
      std::nullopt};
  std::optional<CgroupPath> target_cgroup{std::nullopt};
};

struct ContextParams {
  // TODO(dlxu): migrate to ring buffer for raw datapoints so plugins
  // can calculate weighted average themselves
  double average_size_decay{4.0};
  // root io device IDs (<major>:<minor>) and their device types (SSD/HDD)
  std::unordered_map<std::string, DeviceType> io_devs;
  IOCostCoeffs hdd_coeffs;
  IOCostCoeffs ssd_coeffs;
};

class OomdContext {
 public:
  // Immutable CgroupContext type returned from public APIs, which has life time
  // valid for the interval being accessed. Plugins should never store it.
  using ConstCgroupContextRef = std::reference_wrapper<const CgroupContext>;

  explicit OomdContext(const ContextParams& params = {}) : params_(params) {}
  ~OomdContext() = default;
  OomdContext(OomdContext&& other) noexcept = default;
  OomdContext& operator=(OomdContext&& other) = default;

  std::vector<CgroupPath> cgroups() const;

  const ContextParams& getParams() const {
    return params_;
  }

  /*
   * Add a cgroup to cache if not already exist, and return the result. If it's
   * invalid, return std::nullopt.
   */
  std::optional<ConstCgroupContextRef> addToCacheAndGet(
      const CgroupPath& cgroup);

  /*
   * Add a set of cgroups to cache if not already exist, and return the result.
   * Cgroup paths may contain glob pattern, which will be expanded if valid.
   * If ruleset_cgroup_ is set, also add each ruleset_cgroups relative to it.
   * Returned CgroupContexts are all valid and won't contain duplicate.
   */
  std::vector<ConstCgroupContextRef> addToCacheAndGet(
      const std::unordered_set<CgroupPath>& cgroups,
      const std::unordered_set<CgroupPath>& ruleset_cgroups);
  /*
   * Get child of cgroup, adding it to the cache if it doesn't exist yet.
   */
  std::optional<ConstCgroupContextRef> addChildToCacheAndGet(
      const CgroupContext& cgroup_ctx,
      const std::string& child);

  /*
   * Get children of cgroup, adding them to the cache if they don't exist yet.
   */
  std::vector<ConstCgroupContextRef> addChildrenToCacheAndGet(
      const CgroupContext& cgroup_ctx);

  /*
   * Add a set of cgroups to cache, and return the resulting CgroupContext
   * sorting in descending order by the get_key functor, which accepts a const
   * reference of CgroupContext and returns something comparable.
   */
  template <class Functor>
  std::vector<ConstCgroupContextRef> reverseSort(
      const std::unordered_set<CgroupPath>& cgroups,
      const std::unordered_set<CgroupPath>& ruleset_cgroups,
      Functor&& get_key) {
    auto sorted = addToCacheAndGet(cgroups, ruleset_cgroups);
    std::sort(sorted.begin(), sorted.end(), [&](const auto& a, const auto& b) {
      return get_key(a.get()) > get_key(b.get());
    });
    return sorted;
  }

  /*
   * Sorts cgroups by kill_preference, then get_key. Highest first to lowest
   * last. Returns new vec; does not mutate cgroups arg.
   */
  template <class Functor>
  static std::vector<ConstCgroupContextRef> sortDescWithKillPrefs(
      const std::vector<ConstCgroupContextRef>& cgroups,
      Functor&& get_key) {
    auto sorted = cgroups;
    std::sort(sorted.begin(), sorted.end(), [&](const auto& a, const auto& b) {
      return std::make_tuple(
                 a.get().kill_preference().value_or(KillPreference::NORMAL),
                 get_key(a.get())) >
          std::make_tuple(
                 b.get().kill_preference().value_or(KillPreference::NORMAL),
                 get_key(b.get()));
    });
    return sorted;
  }

  /*
   * Dumps OomdContext state to stderr
   */
  void dump();
  static void dump(
      const std::vector<ConstCgroupContextRef>& vec,
      const bool skip_negligible = false);

  /*
   * Used to let action plugins know which ruleset and detector group
   * triggered it
   */
  const ActionContext& getActionContext() const;
  void setActionContext(const ActionContext& context);

  /*
   * Used to let action plugins retrieve and set info stored in struct
   */
  const SystemContext& getSystemContext() const;
  void setSystemContext(const SystemContext& context);

  /*
   * Used for plugins to know how long it's been since their last run()
   */
  uint64_t getCurrentTick();
  void bumpCurrentTick();

  /*
   * Lets action plugins call pause_actions(post_action_delay) on their owning
   * ruleset. Only available while the ruleset is running its action chain.
   */
  const std::optional<Engine::Ruleset*> getInvokingRuleset();
  void setInvokingRuleset(std::optional<Engine::Ruleset*> ruleset);

  /*
   * Used to let kill plugins invoke prekill hooks
   */
  std::optional<std::unique_ptr<Engine::PrekillHookInvocation>> firePrekillHook(
      const CgroupContext& cgroup_ctx);
  void setPrekillHooksHandler(
      std::function<
          std::optional<std::unique_ptr<Engine::PrekillHookInvocation>>(
              const CgroupContext& cgroup_ctx)> prekill_hook_handler);

  /*
   * used to define rule-set level cgroup
   */
  const std::optional<CgroupPath> getRulesetCgroup() const;
  void setRulesetCgroup(std::optional<CgroupPath> context);

  /*
   * Refresh all cgroups and remove ones no longer exist.
   */
  void refresh();

 private:
  // Test only
  friend class TestHelper;

  struct ContextParams params_;
  std::unordered_map<CgroupPath, CgroupContext> cgroups_;
  ActionContext action_context_;
  SystemContext system_ctx_;
  uint64_t current_tick_{0};
  std::optional<Engine::Ruleset*> invoking_ruleset_{std::nullopt};
  std::function<std::optional<std::unique_ptr<Engine::PrekillHookInvocation>>(
      const CgroupContext& cgroup_ctx)>
      prekill_hook_handler_{nullptr};
  std::optional<CgroupPath> ruleset_cgroup_{std::nullopt};
};

} // namespace Oomd
