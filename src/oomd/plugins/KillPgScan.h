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

#pragma once

#include <chrono>
#include <optional>
#include <unordered_set>

#include "oomd/plugins/BaseKillPlugin.h"

namespace Oomd {

template <typename Base = BaseKillPlugin>
class KillPgScan : public Base {
 public:
  Engine::PluginRet run(OomdContext& ctx) override;

  static KillPgScan* create() {
    return new KillPgScan();
  }

  ~KillPgScan() override = default;

 protected:
  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) override;

  void ologKillTarget(
      OomdContext& ctx,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& peers) override;

  /*
   * KillPgScan has a 2-tick kill. It needs to collect pgscan data over time
   * so it can pick the cgroup whose pgscan grew the most over the last tick.
   * Because this data collection is mildly costly, we do it only on kill.
   *   On first run(), we collect data and return ASYNC_PAUSED
   *   On second run(), collect data a second time and do the actual kill.
   * We collect data every tick until the kill finishes, because a kill can take
   * multiple ticks (thanks to async prekill hooks) and we don't want
   * accidentally stale-ish data.
   * If we haven't collected data in 2 ticks, consider it dropped to be safe.
   */
  std::optional<uint64_t> last_tick_data_was_collected_{std::nullopt};
};

} // namespace Oomd

#include "oomd/plugins/KillPgScan-inl.h"
