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

#include "oomd/engine/Ruleset.h"
#include "oomd/Log.h"

namespace Oomd {
namespace Engine {

Ruleset::Ruleset(
    const std::string& name,
    std::vector<std::unique_ptr<DetectorGroup>> detector_groups,
    std::vector<std::unique_ptr<BasePlugin>> action_group)
    : name_(name),
      detector_groups_(std::move(detector_groups)),
      action_group_(std::move(action_group)) {}

void Ruleset::runOnce(OomdContext& context) {
  bool run_actions = false;

  // If any DetectorGroup fires, then begin running action chain
  for (const auto& dg : detector_groups_) {
    if (dg->check(context)) {
      OLOG << "DetectorGroup=" << dg->name()
           << " has fired for Ruleset=" << name_ << ". Running action chain.";
      run_actions = true;
      context.setActionContext({name_, dg->name()});
      break;
    }
  }

  if (!run_actions) {
    return;
  }

  // Begin running action chain
  for (const auto& action : action_group_) {
    OLOG << "Running Action=" << action->getName();
    PluginRet ret = action->run(context);

    switch (ret) {
      case PluginRet::CONTINUE:
        OLOG << "Action=" << action->getName()
             << " returned CONTINUE. Continuing action chain.";
        continue;
      case PluginRet::STOP:
        OLOG << "Action=" << action->getName()
             << " returned STOP. Terminating action chain.";
        break; // break out of switch
        // missing default to protect against future PluginRet vals
    }

    break;
  }
}

} // namespace Engine
} // namespace Oomd
