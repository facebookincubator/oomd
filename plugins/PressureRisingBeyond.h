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
#include <unordered_set>

#include "oomd/engine/BasePlugin.h"

namespace Oomd {

class PressureRisingBeyond : public Oomd::Engine::BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& resources,
      const Engine::PluginArgs& args) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static PressureRisingBeyond* create() {
    return new PressureRisingBeyond();
  }

  ~PressureRisingBeyond() = default;

 private:
  std::unordered_set<std::string> cgroups_;
  std::string cgroup_fs_;
  ResourceType resource_;
  // Initialized to bogus values; init() will crash oomd if non-0 return
  int threshold_;
  int duration_;
  float fast_fall_ratio_;

  ResourcePressure last_pressure_{100, 100, 100};
  std::chrono::steady_clock::time_point hit_thres_at_{};
};

} // namespace Oomd
