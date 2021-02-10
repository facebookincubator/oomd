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

// This is a Senpai plugin which works by continuously adjusting a
// memory limit in order to keep pressure steady.
namespace Oomd {
template <>
struct CgroupState<class SenpaiHold> {
  // Current memory limit
  int64_t limit;
  // Last recorded total memory pressure
  std::chrono::microseconds last_total;
  // Cumulative memory pressure since last adjustment
  std::chrono::microseconds cumulative{0};
  // Count-down to decision to probe/backoff
  int64_t ticks;
  // Probe statistics for logging
  uint64_t probe_bytes{0};
  uint64_t probe_count{0};
};

class SenpaiHold : public SenpaiCommon<SenpaiHold> {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static SenpaiHold* create() {
    return new SenpaiHold();
  }

  using CgroupState = CgroupState<SenpaiHold>;

  std::optional<CgroupState> initializeCgroup(const CgroupContext& cgroup_ctx);
  bool tick(const CgroupContext& cgroup_ctx, CgroupState& state);

  ~SenpaiHold() override = default;

 private:
  SystemMaybe<bool> validatePressure(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<bool> validateSwap(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<double> calculateSwappinessFactor(
      const CgroupContext& cgroup_ctx) const;

  std::chrono::microseconds pressure_ms_{std::chrono::milliseconds{10}};
  double max_backoff_{1.0};
  double coeff_probe_{10};
  double coeff_backoff_{20};
};
} // namespace Oomd
