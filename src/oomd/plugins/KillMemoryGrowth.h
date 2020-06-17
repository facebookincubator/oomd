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
#include <cmath>
#include <unordered_set>

#include "oomd/plugins/BaseKillPlugin.h"

namespace Oomd {

// KillMemoryGrowh kills in 3 phases, as described in docs/core_plugins.md
// KMGPhase is only for internal use in KillMemoryGrowth ranking.
enum struct KMGPhase {
  SIZE_THRESHOLD,
  GROWTH,
  SIZE_NO_THRESHOLD,
};

template <typename Base = BaseKillPlugin>
class KillMemoryGrowth : public Base {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  void prerun(OomdContext& ctx) override;

  static KillMemoryGrowth* create() {
    return new KillMemoryGrowth();
  }

  ~KillMemoryGrowth() = default;

 protected:
  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) override;

  void ologKillTarget(
      OomdContext& ctx,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& peers) override;

  std::function<std::pair<KMGPhase, std::tuple<int64_t, float, int64_t>>(
      const CgroupContext&)>
  get_ranking_fn(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups);

  int size_threshold_{50};
  int growing_size_percentile_{80};
  float min_growth_ratio_{1.25};
};

} // namespace Oomd

#include "oomd/plugins/KillMemoryGrowth-inl.h"
