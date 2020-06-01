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
  };

  std::optional<bool> hasMemoryHighTmp(const CgroupContext& cgroup_ctx);
  std::optional<int64_t> readMemhigh(const CgroupContext& cgroup_ctx);
  bool writeMemhigh(const CgroupContext& cgroup_ctx, int64_t value);

  bool tick(const CgroupContext& cgroup_ctx, CgroupState& state);
  std::optional<CgroupState> initializeCgroup(const CgroupContext& cgroup_ctx);

  std::optional<bool> has_memory_high_tmp_{};

  std::unordered_set<CgroupPath> cgroups_;
  std::map<CgroupContext::Id, CgroupState> tracked_cgroups_;

  // cgroup size limits
  int64_t limit_min_bytes_{100ull << 20};
  int64_t limit_max_bytes_{500ull << 30};
  // pressure target - stall time over sampling period
  int64_t interval_{6};
  std::chrono::microseconds pressure_ms_{std::chrono::milliseconds{10}};
  // translate observed target deviation to cgroup adjustment rate
  // - max_probe is reached when stalling falls below pressure / coeff_probe
  // - max_backoff is reached when stalling exceeds pressure * coeff_backoff
  double max_probe_{0.01};
  double max_backoff_{1.0};
  double coeff_probe_{10};
  double coeff_backoff_{20};
};

} // namespace Oomd
