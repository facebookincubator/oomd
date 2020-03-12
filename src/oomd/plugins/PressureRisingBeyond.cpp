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
#include "oomd/util/Fs.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(pressure_rising_beyond, PressureRisingBeyond::create);

int PressureRisingBeyond::init(
    Engine::MonitoredResources& resources,
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  if (args.find("cgroup") != args.end()) {
    const auto& cgroup_fs = context.cgroupFs();
    auto cgroups = Util::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
      resources.emplace(cgroup_fs, c);
      cgroups_.emplace(cgroup_fs, c);
    }
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  if (args.find("resource") != args.end() &&
      (args.at("resource") == "io" || args.at("resource") == "memory")) {
    const auto& res = args.at("resource");
    if (res == "io") {
      resource_ = ResourceType::IO;
    } else if (res == "memory") {
      resource_ = ResourceType::MEMORY;
    }
  } else {
    OLOG << "Argument=resource missing or not (io|memory)";
    return 1;
  }

  if (args.find("threshold") != args.end()) {
    threshold_ = std::stoi(args.at("threshold"));
  } else {
    OLOG << "Argument=threshold not present";
    return 1;
  }

  if (args.find("duration") != args.end()) {
    duration_ = std::stoi(args.at("duration"));
  } else {
    OLOG << "Argument=duration not present";
    return 1;
  }

  // `fast_fall_ratio` is optional
  if (args.find("fast_fall_ratio") != args.end()) {
    fast_fall_ratio_ = std::stof(args.at("fast_fall_ratio"));
  } else {
    fast_fall_ratio_ = 0.85;
  }

  // Success
  return 0;
}

Engine::PluginRet PressureRisingBeyond::run(OomdContext& ctx) {
  using std::chrono::steady_clock;

  ResourcePressure current_pressure;
  int64_t current_memory_usage = 0;
  auto sorted_cgroups = ctx.reverseSort();
  OomdContext::removeSiblingCgroups(cgroups_, sorted_cgroups);

  for (const auto& state_pair : sorted_cgroups) {
    ResourcePressure rp;
    switch (resource_) {
      case ResourceType::IO:
        rp = state_pair.second.io_pressure;
        break;
      case ResourceType::MEMORY:
        rp = state_pair.second.pressure;
        break;
        // No default case to catch for future additions to ResourceType
    }

    // Do a weighted comparison (we care more about 10s, then 60s, then 300s)
    if (rp.sec_10 * 3 + rp.sec_60 * 2 + rp.sec_300 >
        current_pressure.sec_10 * 3 + current_pressure.sec_60 * 2 +
            current_pressure.sec_300) {
      current_pressure = rp;
      current_memory_usage = state_pair.second.current_usage;
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
