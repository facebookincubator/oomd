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

#include "oomd/plugins/SwapFree.h"

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"

namespace Oomd {

REGISTER_PLUGIN(swap_free, SwapFree::create);

int SwapFree::init(
    Engine::MonitoredResources& /* unused */,
    std::unordered_map<std::string, std::string> args) {
  if (args.find("threshold_pct") != args.end()) {
    threshold_pct_ = std::stoi(args["threshold_pct"]);
  } else {
    OLOG << "Argument=threshold_pct not present";
    return 1;
  }

  if (args.find("meminfo_location") != args.end()) {
    meminfo_location_ = args["meminfo_location"];
  }

  // Success
  return 0;
}

Engine::PluginRet SwapFree::run(OomdContext& /* unused */) {
  using std::chrono::steady_clock;

  auto meminfo = meminfo_location_.size() ? Fs::getMeminfo(meminfo_location_)
                                          : Fs::getMeminfo();
  const int64_t swapfree = meminfo["SwapFree"];
  const int64_t swaptotal = meminfo["SwapTotal"];

  OLOG << "swapfree=" << swapfree / 1024 / 1024
       << "MB / swaptotal=" << swaptotal / 1024 / 1024 << "MB";

  const int64_t swapthres = swaptotal * threshold_pct_ / 100;
  if (swaptotal > 0 && swapfree < swapthres) {
    OLOG << "SwapFree " << swapfree / 1024 / 1024
         << "MB is smaller than the threshold of " << swapthres / 1024 / 1024
         << "MB, total swap is " << swaptotal / 1024 / 1024 << "MB";
    return Engine::PluginRet::CONTINUE;
  } else {
    return Engine::PluginRet::STOP;
  }
}

} // namespace Oomd
