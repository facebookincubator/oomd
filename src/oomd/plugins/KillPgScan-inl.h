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
#include "oomd/engine/BasePlugin.h"
#include "oomd/include/Types.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace Oomd {

template <typename Base>
Engine::PluginRet KillPgScan<Base>::run(OomdContext& ctx) {
  bool has_prev_tick_data =
      (last_tick_data_was_collected_ == ctx.getCurrentTick() - 1);

  // always collect pg scan data
  Base::prerunOnCgroups(
      ctx, [](const auto& cgroup_ctx) { cgroup_ctx.pg_scan_rate(); });
  last_tick_data_was_collected_ = ctx.getCurrentTick();

  if (!has_prev_tick_data) {
    // wait until we have 2 ticks of data to compare
    return Engine::PluginRet::ASYNC_PAUSED;
  }

  // run the kill like normal
  return Base::run(ctx);
}

template <typename Base>
std::vector<OomdContext::ConstCgroupContextRef>
KillPgScan<Base>::rankForKilling(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {
  int64_t num_missing_pg_scan = 0, num_invalid = 0;
  for (const CgroupContext& cgroup : cgroups) {
    if (!cgroup.pg_scan_rate().has_value()) {
      num_missing_pg_scan += 1;
      // assume all invalid cgroups will also fail to fetch pg_scan_rate
      if (!Fs::isCgroupValid(cgroup.fd())) {
        num_invalid += 1;
      }
    }
    if (num_missing_pg_scan > 0) {
      if (num_invalid == 0) {
        OLOG << "couldn't read pgscan data in " << num_missing_pg_scan << "/"
             << cgroups.size() << " cgroups";
      } else {
        OLOG << "couldn't read pgscan data in " << num_missing_pg_scan << "/"
             << cgroups.size() << " cgroups where "
             << (cgroups.size() - num_invalid) << "/" << cgroups.size()
             << " are still valid";
      }
    }
  }

  return OomdContext::sortDescWithKillPrefs(
      Util::filter(
          cgroups,
          [=](const CgroupContext& cgroup_ctx) {
            return cgroup_ctx.pg_scan_rate().value_or(0) > 0;
          }),
      [](const CgroupContext& cgroup_ctx) {
        return cgroup_ctx.pg_scan_rate().value_or(0);
      });
}

template <typename Base>
void KillPgScan<Base>::ologKillTarget(
    OomdContext& ctx,
    const CgroupContext& target,
    const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */) {
  OLOG << "Picked \"" << target.cgroup().relativePath() << "\" ("
       << target.current_usage().value_or(0) / 1024 / 1024
       << "MB) based on pg scan rate at " << target.pg_scan_rate().value_or(0)
       << " with kill preference "
       << target.kill_preference().value_or(KillPreference::NORMAL);
}

} // namespace Oomd
