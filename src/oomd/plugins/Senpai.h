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
      Engine::MonitoredResources& resources,
      const Engine::PluginArgs& args) override;

  Engine::PluginRet run(OomdContext& ctx) override;

  static Senpai* create() {
    return new Senpai();
  }

  ~Senpai() = default;

 private:
  struct CgroupState {
    CgroupState(
        uint64_t start_limit,
        std::chrono::microseconds total,
        uint64_t start_ticks,
        const std::string& path);

    // Current memory limit
    uint64_t limit;
    // Last recorded total memory pressure
    std::chrono::microseconds last_total;
    // Cumulative memory pressure since last adjustment
    std::chrono::microseconds cumulative{0};
    // Count-down to decision to probe/backoff
    uint64_t ticks;
  };

  // Removes any untracked cgroups and returns new cgroups to be watched
  std::map<std::string, CgroupState> addRemoveTrackedCgroups(
      const std::set<std::string>& resolved_cgroups);
  void tick(const std::string& name, CgroupState& state);
  CgroupState initializeCgroup(const std::string& path);
  // Uses memory.high.tmp if available
  int64_t readMemhigh(const std::string& path);
  void writeMemhigh(const std::string& path, int64_t value);

  std::unordered_set<CgroupPath> cgroups_;
  std::map<std::string, CgroupState> tracked_cgroups_;

  // Assume true until we find the first cgroup with/without this file, and then
  // we are sure what's the actual value. Prefer memory.high.tmp over the other
  bool has_memory_high_tmp_{true};

  // cgroup size limits
  uint64_t limit_min_bytes_{1ull << 30};
  uint64_t limit_max_bytes_{500ull << 30};
  // pressure target - stall time over sampling period
  uint64_t interval_{6};
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
