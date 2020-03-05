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

#include "oomd/plugins/NrDyingDescendants.h"

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(nr_dying_descendants, NrDyingDescendants::create);

int NrDyingDescendants::init(
    Engine::MonitoredResources& resources,
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  if (args.find("cgroup") != args.end()) {
    const auto& cgroup_fs = context.cgroupFs();

    auto cgroups = Util::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
      resources.emplace(cgroup_fs, c);
      cgroups_.emplace(cgroup_fs, c);
    }
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  if (args.find("count") != args.end()) {
    int val = std::stoi(args.at("count"));
    if (val < 0) {
      OLOG << "Argument=count must be non-negative";
    }
    count_ = val;
  } else {
    OLOG << "Argument=count not present";
  }

  if (args.find("lte") != args.end()) {
    const std::string& val = args.at("lte");

    if (val == "true" || val == "True" || val == "1") {
      lte_ = true;
    }

    if (val == "false" || val == "False" || val == "0") {
      lte_ = false;
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

Engine::PluginRet NrDyingDescendants::run(OomdContext& ctx) {
  auto cgroups = ctx.reverseSort();
  OomdContext::removeSiblingCgroups(cgroups_, cgroups);

  for (const auto& [name, cgroup_ctx] : cgroups) {
    int64_t nr = cgroup_ctx.nr_dying_descendants;
    if ((lte_ && nr <= count_) || (!lte_ && nr > count_)) {
      if (debug_) {
        OLOG << "nr_dying_descendants=" << nr << (lte_ ? " <= " : " > ")
             << "count=" << count_;
      }
      return Engine::PluginRet::CONTINUE;
    }
  }

  return Engine::PluginRet::STOP;
}

} // namespace Oomd
