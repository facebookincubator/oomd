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

  // Success
  return 0;
}

Engine::PluginRet SwapFree::run(OomdContext& ctx) {
  auto& system_ctx = ctx.getSystemContext();
  uint64_t swaptotal = system_ctx.swaptotal;
  uint64_t swapused = system_ctx.swapused;
  const uint64_t swapthres = swaptotal * threshold_pct_ / 100;
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
