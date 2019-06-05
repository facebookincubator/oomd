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
#include "oomd/util/Util.h"

static constexpr auto kCgroupFs = "/sys/fs/cgroup/";
static constexpr auto kPgscan = "pgscan";

namespace Oomd {

REGISTER_PLUGIN(memory_reclaim, MemoryReclaim::create);

int MemoryReclaim::init(
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

  if (args.find("duration") != args.end()) {
    duration_ = std::stoi(args.at("duration"));
  } else {
    OLOG << "Argument=duration not present";
    return 1;
  }

  // Success
  return 0;
}

Engine::PluginRet MemoryReclaim::run(OomdContext& /* unused */) {
  using std::chrono::steady_clock;

  std::unordered_set<std::string> resolved_cgroups;
  for (const auto& cgroup : cgroups_) {
    auto resolved = Fs::resolveWildcardPath(cgroup.absolutePath());
    resolved_cgroups.insert(resolved.begin(), resolved.end());
  }

  int64_t pgscan = 0;
  for (const auto& abs_cgroup_path : resolved_cgroups) {
    auto memstat = Fs::getMemstat(abs_cgroup_path);
    pgscan += memstat[kPgscan];
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
