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

#include "oomd/plugins/Exists.h"

#include <fnmatch.h>

#include <iomanip>
#include <string>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

static constexpr auto kCgroupFs = "/sys/fs/cgroup/";

namespace Oomd {

REGISTER_PLUGIN(exists, Exists::create);

int Exists::init(
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

  if (args.find("negate") != args.end()) {
    const std::string& val = args.at("negate");

    if (val == "true" || val == "True" || val == "1") {
      negate_ = true;
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

Engine::PluginRet Exists::run(OomdContext& ctx) {
  bool exists = false;

  for (const auto& cgroup : cgroups_) {
    for (const auto& path : ctx.cgroups()) {
      if (!::fnmatch(
              cgroup.absolutePath().c_str(), path.absolutePath().c_str(), 0)) {
        exists = true;
        goto out;
      }
    }
  }

out:
  if (negate_) {
    exists = !exists;
  }

  if (exists) {
    return Engine::PluginRet::CONTINUE;
  } else {
    return Engine::PluginRet::STOP;
  }
}

} // namespace Oomd
