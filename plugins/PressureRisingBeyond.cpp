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

static constexpr auto kCgroupFs = "/sys/fs/cgroup";

namespace Oomd {

REGISTER_PLUGIN(pressure_rising_beyond, PressureRisingBeyond::create);

int PressureRisingBeyond::init(
    Engine::MonitoredResources& resources,
    std::unordered_map<std::string, std::string> args) {
  if (args.find("cgroup") != args.end()) {
    resources.emplace(args["cgroup"]);
    cgroup_ = args["cgroup"];
    cgroup_fs_ =
        (args.find("cgroup_fs") != args.end() ? args["cgroup_fs"] : kCgroupFs);
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  if (args.find("resource") != args.end() &&
      (args["resource"] == "io" || args["resource"] == "memory")) {
    const auto& res = args["resource"];
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
    threshold_ = std::stoi(args["threshold"]);
  } else {
    OLOG << "Argument=threshold not present";
    return 1;
  }

  if (args.find("duration") != args.end()) {
    duration_ = std::stoi(args["duration"]);
  } else {
    OLOG << "Argument=duration not present";
    return 1;
  }

  // `fast_fall_ratio` is optional
  if (args.find("fast_fall_ratio") != args.end()) {
    fast_fall_ratio_ = std::stof(args["fast_fall_ratio"]);
  } else {
    fast_fall_ratio_ = 0.85;
  }

  // Success
  return 0;
}

Engine::PluginRet PressureRisingBeyond::run(OomdContext& /* unused */) {
  using std::chrono::steady_clock;

  ResourcePressure current_pressure;
  switch (resource_) {
    case ResourceType::IO:
      current_pressure = Fs::readIopressure(cgroup_fs_ + "/" + cgroup_);
      break;
    case ResourceType::MEMORY:
      current_pressure = Fs::readMempressure(cgroup_fs_ + "/" + cgroup_);
      break;
      // No default case to catch for future additions to ResourceType
  }
  OOMD_SCOPE_EXIT {
    last_pressure_ = current_pressure;
  };

  int64_t current_memory_usage = Fs::readMemcurrent(cgroup_fs_ + "/" + cgroup_);
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
