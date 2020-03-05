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

class MemoryAbove : public Oomd::Engine::BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& resources,
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static MemoryAbove* create() {
    return new MemoryAbove();
  }

  ~MemoryAbove() = default;

 private:
  std::unordered_set<CgroupPath> cgroups_;
  // Initialized to bogus values; init() will crash oomd if non-0 return
  int64_t threshold_;
  int duration_;
  bool is_anon_{false};
  bool debug_{false};
  std::chrono::steady_clock::time_point hit_thres_at_{};
};

} // namespace Oomd
