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

#include "oomd/PluginRegistry.h"
#include "oomd/util/ScopeGuard.h"

static constexpr auto kPgscan = "pgscan";

namespace Oomd {

REGISTER_PLUGIN(memory_reclaim, MemoryReclaim::create);

int MemoryReclaim::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  argParser_.addArgumentCustom(
      "cgroup",
      cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      },
      true);

  argParser_.addArgument("duration", duration_, true);

  if (!argParser_.parse(args)) {
    return 1;
  }

  // Success
  return 0;
}

Engine::PluginRet MemoryReclaim::run(OomdContext& ctx) {
  using std::chrono::steady_clock;

  int64_t pgscan = 0;
  for (const CgroupContext& cgroup_ctx : ctx.addToCacheAndGet(cgroups_)) {
    if (const auto& memstat = cgroup_ctx.memory_stat()) {
      if (auto pos = memstat->find(kPgscan); pos != memstat->end()) {
        pgscan += pos->second;
      }
    }
  }

  OOMD_SCOPE_EXIT {
    last_pgscan_ = pgscan;
  };

  const auto now = steady_clock::now();

  if (pgscan > last_pgscan_) {
    last_reclaim_at_ = now;
  }

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
