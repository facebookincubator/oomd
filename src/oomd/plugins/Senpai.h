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

#include "oomd/engine/BasePlugin.h"
#include "oomd/util/SystemMaybe.h"

#include <chrono>
#include <map>
#include <set>
#include <string>
#include <unordered_set>

namespace Oomd {

/*
 * A plugin which adjusts memory.high on a cgroup in order to create a
 * light amount of memory pressure. This allows memory.current to more
 * accurately represent the amount of memory required by the cgroup.
 */
class Senpai : public Engine::BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& ctx) override;

  static Senpai* create() {
    return new Senpai();
  }

  ~Senpai() = default;

 private:
  struct CgroupState {
    CgroupState(
        int64_t start_limit,
        std::chrono::microseconds total,
        int64_t start_ticks);

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

  std::optional<bool> hasMemoryReclaim(const CgroupContext& cgroup_ctx);
  std::optional<bool> hasMemoryHighTmp(const CgroupContext& cgroup_ctx);
  std::optional<int64_t> readMemhigh(const CgroupContext& cgroup_ctx);
  bool writeMemhigh(const CgroupContext& cgroup_ctx, int64_t value);
  bool writeMemhighTimeout(
      const CgroupContext& cgroup_ctx,
      int64_t value,
      std::chrono::milliseconds timeout);
  bool resetMemhigh(const CgroupContext& cgroup_ctx);
  bool reclaim(const CgroupContext& cgroup_ctx, int64_t size);
  SystemMaybe<int64_t> getReclaimableBytes(const CgroupContext& cgroup_ctx);
  std::optional<int64_t> getLimitMinBytes(const CgroupContext& cgroup_ctx);
  std::optional<int64_t> getLimitMaxBytes(const CgroupContext& cgroup_ctx);
  void checkAndLogHighPressure(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<bool> validatePressure(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<bool> validateSwap(const CgroupContext& cgroup_ctx) const;
  SystemMaybe<double> calculateSwappinessFactor(
      const CgroupContext& cgroup_ctx) const;

  bool tick(const CgroupContext& cgroup_ctx, CgroupState& state);
  bool tick_immediate_backoff(
      const CgroupContext& cgroup_ctx,
      CgroupState& state);
  std::optional<CgroupState> initializeCgroup(const CgroupContext& cgroup_ctx);

  int64_t host_mem_total_{0};

  std::optional<bool> has_memory_reclaim_{};
  std::optional<bool> has_memory_high_tmp_{};

  std::unordered_set<CgroupPath> cgroups_;
  std::map<CgroupContext::Id, CgroupState> tracked_cgroups_;

  // cgroup size limits
  int64_t limit_min_bytes_{100ull << 20};
  int64_t limit_max_bytes_{10ull << 30};
  // pressure target - stall time over sampling period
  int64_t interval_{6};
  // interval between aggregation logging; only for immediate_backoff
  int64_t log_interval_{60};
  int64_t log_ticks_{0};
  std::chrono::milliseconds pressure_ms_{10};
  // Currently only used for immediate backoff
  double mem_pressure_pct_{0.1};
  double io_pressure_pct_{0.1};
  // translate observed target deviation to cgroup adjustment rate
  // - max_probe is reached when stalling falls below pressure / coeff_probe
  // - max_backoff is reached when stalling exceeds pressure * coeff_backoff
  double max_probe_{0.01};
  double max_backoff_{1.0};
  double coeff_probe_{10};
  double coeff_backoff_{20};
  double swap_threshold_{0.8};
  int64_t swapout_bps_threshold_{1ull << 20};
  std::chrono::milliseconds memory_high_timeout_{};
  bool swap_validation_{false};
  bool immediate_backoff_{false};
  bool modulate_swappiness_{false};
};

} // namespace Oomd
