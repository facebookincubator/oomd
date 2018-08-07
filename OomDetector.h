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
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

#include "oomd/shared/OomdContext.h"
#include "oomd/shared/Plugin.h"

namespace Oomd {

class OomDetector {
 public:
  OomDetector() = delete;
  explicit OomDetector(const PluginArgs& args);
  virtual ~OomDetector() = default;

  virtual bool isOOM(OomdContext& ctx);
  virtual void postKill(OomdContext& ctx);

  virtual std::string getCgroupPath() const {
    return cgroup_path_;
  }

  static OomDetector* create(const PluginArgs& args) {
    return new OomDetector(args);
  }

 protected:
  bool isPressureOOM(const ResourcePressure& pressure, OomContext& octx) const;
  bool isSwapOOM(int64_t swapfree, int64_t swaptotal, OomContext& octx);
  bool isHeuristicOOM(
      int64_t current,
      const ResourcePressure& pressure,
      OomContext& octx);

  // tunables
  std::string cgroup_path_;
  std::shared_ptr<KillList> kill_list_;
  std::shared_ptr<Tunables> tunables_;

  std::chrono::steady_clock::time_point high_thres_at_{};
  ResourcePressure last_pressure_{100, 100, 100};
  int64_t last_pgscan_{0};
  int64_t pgscan_window_{0}; // number of ticks where delta(pgscan) > 0

  // map of cgroup_name -> status
  std::shared_ptr<OomdContext> state_{nullptr};
};

} // namespace Oomd
