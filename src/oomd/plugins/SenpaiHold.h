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
//
// When deciding when to act, Senpai will take a look at the
// observed pressure vs the target pressure and calculate some
// error function - `abs(observed - target) / target`

// Senpai can act every "tick" of the oomd loop or do nothing. The
// time since the last action (e.g. setting of the memory limit)
// must factor in here because it captures some level of certainty
// that the observed pressure was due to the last action or not.

// To give an example, Senpai could set a limit too low, reclaim a
// bunch of memory and then in the next tick adjust the limit much
// higher. Refaults from memory reclaimed in the first setting could
// cause pressure after the second setting. The longer it has been
// since Senpai last took action, the more certain we are that the
// observed pressure is a function of the last action.
//
// In order to capture this, Senpai will act when the error exceeds
// some threshold that decays exponentially over time.
namespace Oomd {
template <>
struct CgroupState<class SenpaiHold> {
  // Current memory limit
  int64_t limit;
  // Last recorded total memory pressure
  std::chrono::microseconds last_total;
  // Cumulative memory pressure since last adjustment
  std::chrono::microseconds cumulative{0};
  // Time of last Senpai action
  std::chrono::time_point<std::chrono::steady_clock> last_action_time;
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

  SystemMaybe<Unit> initializeCgroup(
      const CgroupContext& cgroup_ctx,
      CgroupState& state);
  bool tick(const CgroupContext& cgroup_ctx, CgroupState& state);

  ~SenpaiHold() override = default;

 private:
  SystemMaybe<bool> validatePressure(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<bool> validateSwap(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<double> calculateSwappinessFactor(
      const CgroupContext& cgroup_ctx) const;

  // This is the target pressure Senpai will aim to maintain
  double pressure_target_{0.02};

  // If the error rate is above the exponential curve intersecting
  // (0, blowout_threshold_) and (max_action_interval_ms_, epsilon)
  // then Senpai will take action. You can think of these as
  // sensitivty parameters - the lower they are, the more
  // aggressively Senpai will adjust limits based on observed
  // pressure.

  // The blowout factor is the error threshold at which Senpai will
  // always act, no matter how long it's been since the last
  // action.
  double blowout_threshold_{4};
  // This value represents how long before Senpai will act no matter
  // how small the error.
  std::chrono::milliseconds max_action_interval_ms_{std::chrono::seconds(10)};
  // Calculated in init() based on the above two parameters
  double error_threshold_exponent_;

  // These parameters effect how aggressively or conservatively
  // Senpai adjusts the limit once it has decided to act.
  double max_backoff_{1.0};
  double coeff_backoff_{19};
};
} // namespace Oomd
