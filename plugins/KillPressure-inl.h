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

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <string>
#include <utility>
#include <vector>

#include "oomd/Log.h"
#include "oomd/include/Types.h"
#include "oomd/util/Fs.h"

namespace Oomd {

template <typename Base>
int KillPressure<Base>::init(
    Engine::MonitoredResources& resources,
    const Engine::PluginArgs& args) {
  if (args.find("cgroup") != args.end()) {
    auto cgroups = Fs::split(args.at("cgroup"), ',');
    resources.insert(cgroups.begin(), cgroups.end());
    cgroups_.insert(cgroups.begin(), cgroups.end());
    cgroup_fs_ =
        (args.find("cgroup_fs") != args.end() ? args.at("cgroup_fs")
                                              : kCgroupFs);
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

  if (args.find("post_action_delay") != args.end()) {
    int val = std::stoi(args.at("post_action_delay"));

    if (val < 0) {
      OLOG << "Argument=post_action_delay must be non-negative";
      return 1;
    }

    post_action_delay_ = val;
  }

  if (args.find("dry") != args.end()) {
    const std::string& val = args.at("dry");

    if (val == "true" || val == "True" || val == "1") {
      dry_ = true;
    }
  }

  // Success
  return 0;
}

template <typename Base>
Engine::PluginRet KillPressure<Base>::run(OomdContext& ctx) {
  bool ret = tryToKillSomething(ctx);

  if (ret) {
    std::this_thread::sleep_for(std::chrono::seconds(post_action_delay_));
    return Engine::PluginRet::STOP;
  } else {
    return Engine::PluginRet::CONTINUE;
  }
}

template <typename Base>
bool KillPressure<Base>::tryToKillSomething(OomdContext& ctx) {
  auto pressure_sorted =
      ctx.reverseSort([this](const CgroupContext& cgroup_ctx) {
        int average = 0;
        switch (resource_) {
          case ResourceType::IO:
            average = cgroup_ctx.io_pressure.sec_10 / 2 +
                cgroup_ctx.io_pressure.sec_60 / 2;
            break;
          case ResourceType::MEMORY:
            average =
                cgroup_ctx.pressure.sec_10 / 2 + cgroup_ctx.pressure.sec_60 / 2;
            break;
        }

        return average;
      });
  OomdContext::dumpOomdContext(pressure_sorted);
  Base::removeSiblingCgroups(cgroups_, pressure_sorted);
  OLOG << "Removed sibling cgroups";
  OomdContext::dumpOomdContext(pressure_sorted);

  for (const auto& state_pair : pressure_sorted) {
    float pressure10 = 0;
    float pressure60 = 0;
    switch (resource_) {
      case ResourceType::IO:
        pressure10 = state_pair.second.io_pressure.sec_10;
        pressure60 = state_pair.second.io_pressure.sec_60;
        break;
      case ResourceType::MEMORY:
        pressure10 = state_pair.second.pressure.sec_10;
        pressure60 = state_pair.second.pressure.sec_60;
        break;
    }

    OLOG << "Picked \"" << state_pair.first << "\" ("
         << state_pair.second.current_usage / 1024 / 1024
         << "MB) based on pressure generation at "
         << "10s=" << pressure10 << " 60s=" << pressure60;
    if (Base::tryToKillCgroup(
            cgroup_fs_ + "/" + state_pair.first, true, dry_)) {
      Base::logKill(state_pair.first, state_pair.second, dry_);
      return true;
    }
  }

  return false;
}

} // namespace Oomd
