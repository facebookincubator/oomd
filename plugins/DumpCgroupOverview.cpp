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

#include <iomanip>
#include <sstream>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace {
auto constexpr kCgroupFs = "/sys/fs/cgroup";
auto constexpr kPgscanSwap = "pgscan_kswapd";
auto constexpr kPgscanDirect = "pgscan_direct";

void dumpCgroupOverview(const std::string& absolute_cgroup_path, bool always) {
  // Only log on exceptional cases
  const auto pressure = Oomd::Fs::readMempressure(absolute_cgroup_path);
  bool should_dump = (always || (pressure.sec_10 >= 1 && pressure.sec_60 > 0));
  if (!should_dump) {
    return;
  }

  const int64_t current = Oomd::Fs::readMemcurrent(absolute_cgroup_path);
  auto meminfo = Oomd::Fs::getMeminfo();
  const int64_t swapfree = meminfo["SwapFree"];
  const int64_t swaptotal = meminfo["SwapTotal"];
  auto vmstat = Oomd::Fs::getVmstat();
  const int64_t pgscan = vmstat[kPgscanSwap] + vmstat[kPgscanDirect];

  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << "cgroup=" << absolute_cgroup_path << " total=" << current / 1024 / 1024
      << "MB pressure=" << pressure.sec_10 << ":" << pressure.sec_60 << ":"
      << pressure.sec_600 << " swapfree=" << swapfree / 1024 / 1024 << "MB/"
      << swaptotal / 1024 / 1024 << "MB pgscan=" << pgscan;
  OLOG << oss.str();
}
} // namespace

namespace Oomd {

REGISTER_PLUGIN(dump_cgroup_overview, DumpCgroupOverview::create);

int DumpCgroupOverview::init(
    Engine::MonitoredResources& /* unused */,
    const Engine::PluginArgs& args) {
  if (args.find("cgroup") != args.end()) {
    auto cgroup_fs =
        (args.find("cgroup_fs") != args.end() ? args.at("cgroup_fs")
                                              : kCgroupFs);
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

Engine::PluginRet DumpCgroupOverview::run(OomdContext& /* unused */) {
  std::unordered_set<std::string> resolved_cgroups;
  for (const auto& cgroup : cgroups_) {
    auto resolved = Fs::resolveWildcardPath(cgroup.absolutePath());
    resolved_cgroups.insert(resolved.begin(), resolved.end());
  }

  for (const auto& resolved : resolved_cgroups) {
    dumpCgroupOverview(resolved, always_);
  }

  return Engine::PluginRet::CONTINUE;
}

} // namespace Oomd
