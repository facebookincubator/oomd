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

#include "oomd/plugins/DumpCgroupOverview.h"

#include <exception>
#include <iomanip>
#include <sstream>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace {
auto constexpr kPgscanSwap = "pgscan_kswapd";
auto constexpr kPgscanDirect = "pgscan_direct";

void dumpCgroupOverview(const Oomd::CgroupContext& cgroup_ctx, bool always) {
  // Only log on exceptional cases
  auto pressure = cgroup_ctx.mem_pressure().value_or(Oomd::ResourcePressure{});
  bool should_dump = (always || (pressure.sec_10 >= 1 && pressure.sec_60 > 0));
  if (!should_dump) {
    return;
  }

  const auto& path = cgroup_ctx.cgroup();
  const int64_t current = cgroup_ctx.current_usage().value_or(0);
  // TODO(dschatzberg): Report error
  auto meminfo = Oomd::Fs::getMeminfo();
  int64_t swapfree = 0;
  int64_t swaptotal = 0;
  if (meminfo) {
    swapfree = (*meminfo)["SwapFree"];
    swaptotal = (*meminfo)["SwapTotal"];
  }
  int64_t pgscan = 0;
  // TODO(dschatzberg): Report error
  auto vmstat = Oomd::Fs::getVmstat();
  if (vmstat) {
    pgscan = vmstat[kPgscanSwap] + vmstat[kPgscanDirect];
  }

  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << "cgroup=" << path.relativePath() << " total=" << current / 1024 / 1024
      << "MB pressure=" << pressure.sec_10 << ":" << pressure.sec_60 << ":"
      << pressure.sec_300 << " swapfree=" << swapfree / 1024 / 1024 << "MB/"
      << swaptotal / 1024 / 1024 << "MB pgscan=" << pgscan;
  OLOG << oss.str();
}
} // namespace

namespace Oomd {

REGISTER_PLUGIN(dump_cgroup_overview, DumpCgroupOverview::create);

int DumpCgroupOverview::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  if (args.find("cgroup") != args.end()) {
    const auto& cgroup_fs = context.cgroupFs();
    auto cgroups = Util::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
      cgroups_.emplace(cgroup_fs, c);
    }
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  if (args.find("always") != args.end()) {
    const auto& var = args.at("always");
    if (var == "true" || var == "True" || var == "1") {
      always_ = true;
    }
  }

  return 0;
}

Engine::PluginRet DumpCgroupOverview::run(OomdContext& ctx) {
  for (const CgroupContext& cgroup_ctx : ctx.addToCacheAndGet(cgroups_)) {
    dumpCgroupOverview(cgroup_ctx, always_);
  }

  return Engine::PluginRet::CONTINUE;
}

} // namespace Oomd
