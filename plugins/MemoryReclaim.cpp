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

#include "oomd/plugins/MemoryReclaim.h"

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/ScopeGuard.h"

static auto constexpr kPgscanSwap = "pgscan_kswapd";
static auto constexpr kPgscanDirect = "pgscan_direct";

namespace Oomd {

REGISTER_PLUGIN(memory_reclaim, MemoryReclaim::create);

int MemoryReclaim::init(
    Engine::MonitoredResources& /* unused */,
    std::unordered_map<std::string, std::string> args) {
  if (args.find("duration") != args.end()) {
    duration_ = std::stoi(args["duration"]);
  } else {
    OLOG << "Argument=duration not present";
    return 1;
  }

  if (args.find("vmstat_location") != args.end()) {
    vmstat_location_ = args["vmstat_location"];
  }

  // Success
  return 0;
}

Engine::PluginRet MemoryReclaim::run(OomdContext& /* unused */) {
  using std::chrono::steady_clock;

  auto vmstat = vmstat_location_.size() ? Fs::getVmstat(vmstat_location_)
                                        : Fs::getVmstat();
  const int64_t pgscan = vmstat[kPgscanSwap] + vmstat[kPgscanDirect];
  OOMD_SCOPE_EXIT {
    last_pgscan_ = pgscan;
  };

  const auto now = steady_clock::now();

  if (pgscan > last_pgscan_) {
    last_reclaim_at_ = now;
  }

  OLOG << "pgscan delta=" << pgscan - last_pgscan_;

  const auto diff =
      std::chrono::duration_cast<std::chrono::seconds>(now - last_reclaim_at_)
          .count();

  if (diff <= duration_) {
    return Engine::PluginRet::CONTINUE;
  } else {
    return Engine::PluginRet::STOP;
  }
}

} // namespace Oomd
