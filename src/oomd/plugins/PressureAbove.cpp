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

#include "oomd/plugins/PressureAbove.h"

#include <iomanip>
#include <string>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(pressure_above, PressureAbove::create);

int PressureAbove::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  if (args.find("cgroup") != args.end()) {
    const auto& cgroup_fs = context.cgroupFs();
    auto cgroups = Util::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
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

  // Success
  return 0;
}

Engine::PluginRet PressureAbove::run(OomdContext& ctx) {
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
        // No default to catch new additions in ResourceType
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

  // Check if the 10s pressure is above threshold_ for duration_
  if (current_pressure.sec_10 > threshold_) {
    if (hit_thres_at_ == steady_clock::time_point()) {
      hit_thres_at_ = now;
    }

    const auto diff =
        std::chrono::duration_cast<std::chrono::seconds>(now - hit_thres_at_)
            .count();

    if (diff >= duration_) {
      std::ostringstream oss;
      oss << std::setprecision(2) << std::fixed;
      oss << "10s pressure " << current_pressure.sec_10
          << " is over the threshold of " << threshold_ << " for " << duration_
          << " seconds , total usage is " << current_memory_usage / 1024 / 1024
          << "MB";
      OLOG << oss.str();

      return Engine::PluginRet::CONTINUE;
    }
  } else {
    hit_thres_at_ = steady_clock::time_point();
  }

  return Engine::PluginRet::STOP;
}

} // namespace Oomd
