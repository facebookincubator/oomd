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
#include "oomd/util/Util.h"

namespace Oomd {

template <typename Base>
int KillSwapUsage<Base>::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  auto meminfo = args.find("meminfo_location") != args.end()
      ? Fs::getMeminfo(args.at("meminfo_location"))
      : Fs::getMeminfo();

  auto swapTotal = 0;
  // TODO(dschatzberg): Report Error
  if (meminfo && meminfo->count("SwapTotal")) {
    swapTotal = (*meminfo)["SwapTotal"];
  }

  // erase meminfo_location since we already loaded it
  auto argsCopy = args;
  argsCopy.erase("meminfo_location");

  this->argParser_.addArgumentCustom(
      "threshold", threshold_, [swapTotal](const std::string& str) {
        int64_t res = 0;
        if (Util::parseSizeOrPercent(str, &res, swapTotal) != 0) {
          throw std::invalid_argument("Failed to parse threshold: " + str);
        }
        return res;
      });

  return Base::init(argsCopy, context);
}

template <typename Base>
std::vector<OomdContext::ConstCgroupContextRef>
KillSwapUsage<Base>::rankForKilling(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {
  return OomdContext::sortDescWithKillPrefs(
      Util::filter(
          cgroups,
          [=](const CgroupContext& cgroup_ctx) {
            return cgroup_ctx.swap_usage().value_or(0) >= threshold_;
          }),
      [](const CgroupContext& cgroup_ctx) {
        return cgroup_ctx.swap_usage().value_or(0);
      });
}

template <typename Base>
void KillSwapUsage<Base>::ologKillTarget(
    OomdContext& ctx,
    const CgroupContext& target,
    const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */) {
  OLOG << "Picked \"" << target.cgroup().relativePath() << "\" ("
       << target.current_usage().value_or(0) / 1024 / 1024
       << "MB) based on swap usage at "
       << target.swap_usage().value_or(0) / 1024 / 1024 << "MB";
}

} // namespace Oomd
