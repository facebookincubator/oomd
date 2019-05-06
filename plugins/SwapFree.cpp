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
#include "oomd/include/Assert.h"
#include "oomd/util/Fs.h"

static auto constexpr kProcSwapsFile = "/proc/swaps";

namespace Oomd {

REGISTER_PLUGIN(swap_free, SwapFree::create);

int SwapFree::init(
    Engine::MonitoredResources& /* unused */,
    const Engine::PluginArgs& args) {
  if (args.find("threshold_pct") != args.end()) {
    threshold_pct_ = std::stoi(args.at("threshold_pct"));
  } else {
    OLOG << "Argument=threshold_pct not present";
    return 1;
  }

  if (args.find("swaps_location") != args.end()) {
    swaps_location_ = args.at("swaps_location");
  } else {
    swaps_location_ = kProcSwapsFile;
  }

  // Success
  return 0;
}

Engine::PluginRet SwapFree::run(OomdContext& /* unused */) {
  uint64_t swaptotal = 0;
  uint64_t swapused = 0;
  auto swaps = Fs::readFileByLine(swaps_location_);

  // For each swap, tally up used and total
  for (int i = 1; i < swaps.size(); ++i) {
    auto parts = Fs::split(swaps[i], '\t');
    // The /proc/swaps format is pretty bad. The first field is padded by
    // spaces but the rest of the fields are padded by '\t'. Since we don't
    // really care about the first field, we'll just split by '\t'.
    OCHECK_EXCEPT(
        parts.size() == 4, std::runtime_error("/proc/swaps malformed"));
    swaptotal += std::stoll(parts[1]) * 1024; // Values are in KB
    swapused += std::stoll(parts[2]) * 1024; // Values are in KB
  }

  const int64_t swapthres = swaptotal * threshold_pct_ / 100;
  if ((swaptotal - swapused) < swapthres) {
    OLOG << "SwapFree " << (swaptotal - swapused) / 1024 / 1024
         << "MB is smaller than the threshold of " << swapthres / 1024 / 1024
         << "MB, total swap is " << swaptotal / 1024 / 1024 << "MB";
    return Engine::PluginRet::CONTINUE;
  } else {
    return Engine::PluginRet::STOP;
  }
}

} // namespace Oomd
