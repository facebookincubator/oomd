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
#include "oomd/util/Util.h"

namespace Oomd {

template <typename Base>
int KillPressure<Base>::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  this->argParser_.addArgument("resource", resource_, true);

  return Base::init(args, context);
}

template <typename Base>
std::vector<OomdContext::ConstCgroupContextRef>
KillPressure<Base>::rankForKilling(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {
  return OomdContext::sortDescWithKillPrefs(
      cgroups, [&](const CgroupContext& cgroup_ctx) {
        int average = 0;
        switch (resource_) {
          case ResourceType::IO:
            if (const auto& pressure = cgroup_ctx.io_pressure()) {
              average = pressure->sec_10 / 2 + pressure->sec_60 / 2;
            }
            break;
          case ResourceType::MEMORY:
            if (const auto& pressure = cgroup_ctx.mem_pressure()) {
              average = pressure->sec_10 / 2 + pressure->sec_60 / 2;
            }
            break;
        }

        return average;
      });
}

template <typename Base>
void KillPressure<Base>::ologKillTarget(
    OomdContext& ctx,
    const CgroupContext& target,
    const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */) {
  float pressure10 = 0;
  float pressure60 = 0;
  switch (resource_) {
    case ResourceType::IO:
      if (const auto& pressure = target.io_pressure()) {
        pressure10 = pressure->sec_10;
        pressure60 = pressure->sec_60;
      }
      break;
    case ResourceType::MEMORY:
      if (const auto& pressure = target.mem_pressure()) {
        pressure10 = pressure->sec_10;
        pressure60 = pressure->sec_60;
      }
      break;
  }

  OLOG << "Picked \"" << target.cgroup().relativePath() << "\" ("
       << target.current_usage().value_or(0) / 1024 / 1024
       << "MB) based on pressure generation at " << "10s=" << pressure10
       << " 60s=" << pressure60;
}

} // namespace Oomd
