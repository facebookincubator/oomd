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
#include "oomd/util/PluginArgParser.h"

namespace Oomd {

REGISTER_PLUGIN(nr_dying_descendants, NrDyingDescendants::create);

int NrDyingDescendants::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  argParser_.addArgumentCustom(
      "cgroup", cgroups_, [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      });
  argParser_.addArgumentCustom(
      "ruleset_cgroup",
      ruleset_cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      });

  argParser_.addArgumentCustom(
      "count", count_, PluginArgParser::parseUnsignedInt, true);
  argParser_.addArgument("lte", lte_);
  argParser_.addArgument("debug", debug_);

  if (!argParser_.parse(args)) {
    return 1;
  }

  // Success
  return 0;
}

Engine::PluginRet NrDyingDescendants::run(OomdContext& ctx) {
  for (const CgroupContext& cgroup_ctx :
       ctx.addToCacheAndGet(cgroups_, ruleset_cgroups_)) {
    if (auto nr = cgroup_ctx.nr_dying_descendants()) {
      if ((lte_ && *nr <= count_) || (!lte_ && *nr > count_)) {
        if (debug_) {
          OLOG << "nr_dying_descendants=" << *nr << (lte_ ? " <= " : " > ")
               << "count=" << count_;
        }
        return Engine::PluginRet::CONTINUE;
      }
    }
  }

  return Engine::PluginRet::STOP;
}

} // namespace Oomd
