/*
 * Copyright (C) 2019-present, Facebook, Inc.
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

#include "oomd/plugins/AdjustCgroup.h"

#include <iomanip>
#include <string>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

static constexpr auto kCgroupFs = "/sys/fs/cgroup/";

namespace Oomd {

REGISTER_PLUGIN(adjust_cgroup, AdjustCgroup::create);

int AdjustCgroup::init(
    Engine::MonitoredResources& resources,
    const Engine::PluginArgs& args) {
  if (args.find("cgroup") != args.end()) {
    auto cgroup_fs =
        (args.find("cgroup_fs") != args.end() ? args.at("cgroup_fs")
                                              : kCgroupFs);

    auto cgroups = Util::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
      resources.emplace(cgroup_fs, c);
      cgroups_.emplace(cgroup_fs, c);
    }
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  if (args.find("memory") != args.end()) {
    memory_adj_set_ = true;
    if (Util::parseSize(args.at("memory"), &memory_adj_) < 0) {
      OLOG << "Invalid size '" << args.at("memory") << "'";
      return 1;
    }
  }

  if (args.find("debug") != args.end()) {
    const std::string& val = args.at("debug");

    if (val == "true" || val == "True" || val == "1") {
      debug_ = true;
    }
  }

  // Success
  return 0;
}

Engine::PluginRet AdjustCgroup::run(OomdContext& ctx) {
  std::string current_cgroup;
  for (const auto& cgroup : cgroups_) {
    try {
      auto& cgroup_ctx = ctx.getMutableCgroupContext(cgroup);

      if (memory_adj_set_) {
        cgroup_ctx.memory_adj = memory_adj_;
        if (debug_) {
          OLOG << "cgroup \"" << cgroup.relativePath() << "\" "
               << "memory_adj=" << cgroup_ctx.memory_adj;
        }
      }
    } catch (const std::exception& ex) {
      if (debug_) {
        OLOG << "Failed to get cgroup \"" << cgroup.relativePath() << "\" "
             << "context: " << ex.what();
      }
      continue;
    }
  }
  return Engine::PluginRet::CONTINUE;
}
} // namespace Oomd
