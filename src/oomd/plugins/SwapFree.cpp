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

namespace Oomd {

REGISTER_PLUGIN(swap_free, SwapFree::create);

int SwapFree::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& /* unused */) {
  argParser_.addArgument("threshold_pct", threshold_pct_, true);
  argParser_.addArgument("swapout_bps_threshold", swapout_bps_threshold_);

  if (!argParser_.parse(args)) {
    return 1;
  }

  // Success
  return 0;
}

Engine::PluginRet SwapFree::run(OomdContext& ctx) {
  auto& system_ctx = ctx.getSystemContext();
  uint64_t swaptotal = system_ctx.swaptotal;
  uint64_t swapused = system_ctx.swapused;
  const uint64_t swapthres = swaptotal * threshold_pct_ / 100;
  if ((swaptotal - swapused) < swapthres &&
      system_ctx.swapout_bps >= swapout_bps_threshold_) {
    OLOG << "SwapFree " << (swaptotal - swapused) / 1024 / 1024
         << "MB is smaller than the threshold of " << swapthres / 1024 / 1024
         << "MB, total swap is " << swaptotal / 1024 / 1024 << "MB";
    if (swapout_bps_threshold_ > 0) {
      OLOG << "SwapOut rate " << (int64_t)system_ctx.swapout_bps
           << "bps is greater than the threshold of " << swapout_bps_threshold_
           << "bps";
    }
    return Engine::PluginRet::CONTINUE;
  } else {
    return Engine::PluginRet::STOP;
  }
}

} // namespace Oomd
