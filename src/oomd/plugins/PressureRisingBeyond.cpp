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

#include "oomd/plugins/PressureRisingBeyond.h"

#include <iomanip>
#include <string>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/include/Types.h"
#include "oomd/util/ScopeGuard.h"

namespace Oomd {

REGISTER_PLUGIN(pressure_rising_beyond, PressureRisingBeyond::create);

int PressureRisingBeyond::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  argParser_.addArgumentCustom(
      "cgroup",
      cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      },
      true);

  argParser_.addArgument("resource", resource_, true);
  argParser_.addArgument("threshold", threshold_, true);
  argParser_.addArgument("duration", duration_, true);
  argParser_.addArgument("fast_fall_ratio", fast_fall_ratio_);

  if (!argParser_.parse(args)) {
    return 1;
  }

  // Success
  return 0;
}

Engine::PluginRet PressureRisingBeyond::run(OomdContext& ctx) {
  using std::chrono::steady_clock;

  ResourcePressure current_pressure;
  int64_t current_memory_usage = 0;

  for (const CgroupContext& cgroup_ctx : ctx.addToCacheAndGet(cgroups_)) {
    ResourcePressure rp;
    switch (resource_) {
      case ResourceType::IO:
        rp = cgroup_ctx.io_pressure().value_or(rp);
        break;
      case ResourceType::MEMORY:
        rp = cgroup_ctx.mem_pressure().value_or(rp);
        break;
        // No default case to catch for future additions to ResourceType
    }

    // Do a weighted comparison (we care more about 10s, then 60s, then 300s)
    if (rp.sec_10 * 3 + rp.sec_60 * 2 + rp.sec_300 >
        current_pressure.sec_10 * 3 + current_pressure.sec_60 * 2 +
            current_pressure.sec_300) {
      current_pressure = rp;
      current_memory_usage = cgroup_ctx.current_usage().value_or(0);
    }
  }

  OOMD_SCOPE_EXIT {
    last_pressure_ = current_pressure;
  };

  const auto now = steady_clock::now();

  // Check if the 60s pressure is above threshold_ for duration_
  bool pressure_duration_met_60s = false;
  if (current_pressure.sec_60 > threshold_) {
    if (hit_thres_at_ == steady_clock::time_point()) {
      hit_thres_at_ = now;
    }

    const auto diff =
        std::chrono::duration_cast<std::chrono::seconds>(now - hit_thres_at_)
            .count();

    if (diff >= duration_) {
      pressure_duration_met_60s = true;
    }
  } else {
    hit_thres_at_ = steady_clock::time_point();
  }

  bool above_threshold_10s = current_pressure.sec_10 > threshold_;
  bool falling_rapidly_10s =
      current_pressure.sec_10 < last_pressure_.sec_10 * fast_fall_ratio_;

  if (pressure_duration_met_60s && above_threshold_10s &&
      !falling_rapidly_10s) {
    std::ostringstream oss;
    oss << std::setprecision(2) << std::fixed;
    oss << "1m pressure " << current_pressure.sec_60
        << " is over the threshold of " << threshold_ << " for " << duration_
        << " seconds , total usage is " << current_memory_usage / 1024 / 1024
        << "MB";
    OLOG << oss.str();

    return Engine::PluginRet::CONTINUE;
  } else {
    return Engine::PluginRet::STOP;
  }
}

} // namespace Oomd
