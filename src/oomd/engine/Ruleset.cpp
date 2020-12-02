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
#include "oomd/engine/EngineTypes.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

namespace Oomd {
namespace Engine {

Ruleset::Ruleset(
    const std::string& name,
    std::vector<std::unique_ptr<DetectorGroup>> detector_groups,
    std::vector<std::unique_ptr<BasePlugin>> action_group,
    bool disable_on_drop_in,
    bool detectorgroups_dropin_enabled,
    bool actiongroup_dropin_enabled,
    uint32_t silence_logs)
    : name_(name),
      detector_groups_(std::move(detector_groups)),
      action_group_(std::move(action_group)),
      disable_on_drop_in_(disable_on_drop_in),
      detectorgroups_dropin_enabled_(detectorgroups_dropin_enabled),
      actiongroup_dropin_enabled_(actiongroup_dropin_enabled),
      silenced_logs_(silence_logs) {}

bool Ruleset::mergeWithDropIn(std::unique_ptr<Ruleset> ruleset) {
  if (!ruleset) {
    OLOG << "Error: merging with null ruleset";
    return false;
  }

  if (ruleset->detector_groups_.size()) {
    if (!detectorgroups_dropin_enabled_) {
      OLOG << "Error: DetectorGroup drop-in configs disabled";
      return false;
    }

    detector_groups_ = std::move(ruleset->detector_groups_);
  }

  if (ruleset->action_group_.size()) {
    if (!actiongroup_dropin_enabled_) {
      OLOG << "Error: Action drop-in configs disabled";
      return false;
    }

    action_group_ = std::move(ruleset->action_group_);
  }

  return true;
}

void Ruleset::markDropInTargeted() {
  ++numTargeted_;

  if (disable_on_drop_in_ && numTargeted_) {
    enabled_ = false;
  }
}

void Ruleset::markDropInUntargeted() {
  --numTargeted_;

  if (numTargeted_ <= 0) {
    enabled_ = true;
  }
}

void Ruleset::prerun(OomdContext& context) {
  if (!enabled_) {
    return;
  }
  for (const auto& dg : detector_groups_) {
    dg->prerun(context);
  }
  for (const auto& action : action_group_) {
    action->prerun(context);
  }
}

uint32_t Ruleset::runOnce(OomdContext& context) {
  if (!enabled_) {
    return 0;
  }

  // If any DetectorGroup fires, then begin running action chain
  //
  // Note we're still check()'ing the detector groups so that any detectors
  // keeping sliding windows can update their window
  //
  // TODO(lnyng): Use prerun() for detector plugins to generate states, i.e.
  // store sliding window metrics, so we can break early in this loop. Need to
  // make sure time between prerun() and run() is short.
  bool run_actions = false;
  for (const auto& dg : detector_groups_) {
    if (dg->check(context, silenced_logs_) && !run_actions) {
      if (!(silenced_logs_ & LogSources::ENGINE)) {
        OLOG << "DetectorGroup=" << dg->name()
             << " has fired for Ruleset=" << name_ << ". Running action chain.";
      }
      run_actions = true;
      context.setActionContext({this, dg->name(), Util::generateUuid()});
    }
  }

  OOMD_SCOPE_EXIT {
    context.setActionContext({nullptr, "", ""});
  };

  // run actions if now() == pause_actions_until_ because a delay of 0 should
  // not cause a pause.
  if (std::chrono::steady_clock::now() < pause_actions_until_) {
    return 0;
  }

  if (active_async_plugin_ != std::nullopt) {
    // clear active_async_plugin_ and save it to a temp
    std::reference_wrapper<BasePlugin> target = active_async_plugin_.value();
    active_async_plugin_ = std::nullopt;

    for (auto&& it = action_group_.begin(); it != action_group_.end(); ++it) {
      if (it->get() == &target.get()) {
        return run_action_chain(it, action_group_.end(), context);
      }
    }
  }

  if (!run_actions) {
    return 0;
  }

  // Begin running action chain
  return run_action_chain(action_group_.begin(), action_group_.end(), context);
}

int Ruleset::run_action_chain(
    std::vector<std::unique_ptr<BasePlugin>>::iterator action_chain_start,
    std::vector<std::unique_ptr<BasePlugin>>::iterator action_chain_end,
    OomdContext& context) {
  for (; action_chain_start != action_chain_end; ++action_chain_start) {
    const std::unique_ptr<BasePlugin>& action = *action_chain_start;

    if (!(silenced_logs_ & LogSources::ENGINE)) {
      OLOG << "Running Action=" << action->getName();
    }

    if (silenced_logs_ & LogSources::PLUGINS) {
      OLOG << LogStream::Control::DISABLE;
    }

    PluginRet ret = action->run(context);

    if (silenced_logs_ & LogSources::PLUGINS) {
      OLOG << LogStream::Control::ENABLE;
    }

    switch (ret) {
      case PluginRet::CONTINUE:
        if (!(silenced_logs_ & LogSources::ENGINE)) {
          OLOG << "Action=" << action->getName()
               << " returned CONTINUE. Continuing action chain.";
        }
        continue;
      case PluginRet::STOP:
        if (!(silenced_logs_ & LogSources::ENGINE)) {
          OLOG << "Action=" << action->getName()
               << " returned STOP. Terminating action chain.";
        }
        break; // break out of switch
      case PluginRet::ASYNC_PAUSED:
        active_async_plugin_ = std::make_optional(std::ref(*action.get()));
        OLOG << "Action=" << action->getName()
             << " returned ASYNC. Yielding action chain.";
        return 0; // don't return 1 until action returns STOP
        // missing default to protect against future PluginRet vals
    }

    break;
  }

  return 1;
}

void Ruleset::pause_actions(std::chrono::seconds duration) {
  pause_actions_until_ = std::max(
      pause_actions_until_, std::chrono::steady_clock::now() + duration);
}

} // namespace Engine
} // namespace Oomd
