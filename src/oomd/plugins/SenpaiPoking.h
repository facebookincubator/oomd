/*
 * Copyright (C) 2019-present, Facebook, Inc.
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

#include "oomd/plugins/SenpaiCommon.h"

// This is a Senpai plugin which works by setting and immediately
// unsetting a memory limit in order to trigger reclaim independently
// from the workload allocation rate.
namespace Oomd {
template <>
struct CgroupState<class SenpaiPoking> {
  // Count-up to decision
  int64_t ticks{0};
  // Probe statistics for logging
  uint64_t probe_bytes{0};
  uint64_t probe_count{0};
};

class SenpaiPoking : public SenpaiCommon<SenpaiPoking> {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static SenpaiPoking* create() {
    return new SenpaiPoking();
  }

  using CgroupState = Oomd::CgroupState<SenpaiPoking>;

  SystemMaybe<Unit> initializeCgroup(
      const CgroupContext& cgroup_ctx,
      CgroupState& state);
  bool tick(const CgroupContext& cgroup_ctx, CgroupState& state);
  std::ostream& log(std::ostream& os, CgroupState& state);

  ~SenpaiPoking() override = default;

 private:
  SystemMaybe<bool> validatePressure(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<bool> validateSwap(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<double> calculateSwappinessFactor(
      const CgroupContext& cgroup_ctx) const;

  double mem_pressure_pct_{0.1};
  double io_pressure_pct_{0.1};
  double swap_threshold_{0.8};
  int64_t swapout_bps_threshold_{3ull << 20};
  bool swap_validation_{false};
  bool modulate_swappiness_{false};
};
} // namespace Oomd
