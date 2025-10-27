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
#include "oomd/PluginRegistry.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/EngineTypes.h"
#include "oomd/util/ScopeGuard.h"

namespace Oomd {
namespace Engine {

Ruleset::Ruleset(
    const std::string& name,
    std::vector<std::unique_ptr<DetectorGroup>> detector_groups,
    std::vector<std::unique_ptr<BasePlugin>> action_group,
    bool disable_on_drop_in,
    bool detectorgroups_dropin_enabled,
    bool actiongroup_dropin_enabled,
    uint32_t silence_logs,
    int post_action_delay,
    int prekill_hook_timeout,
    const std::string& xattr_filter,
    const std::string& cgroup_fs,
    // This is actually a comma separated list of cgroup glob patterns
    const std::string& cgroup)
    : name_(name),
      detector_groups_(std::move(detector_groups)),
      action_group_(std::move(action_group)),
      post_action_delay_(post_action_delay),
      prekill_hook_timeout_(prekill_hook_timeout),
      disable_on_drop_in_(disable_on_drop_in),
      detectorgroups_dropin_enabled_(detectorgroups_dropin_enabled),
      actiongroup_dropin_enabled_(actiongroup_dropin_enabled),
      silenced_logs_(silence_logs),
      xattr_filter_(xattr_filter) {
  if (!cgroup.empty()) {
    cgroups_ = PluginArgParser::parseCgroup(
        PluginConstructionContext(cgroup_fs), cgroup);
  }
}

Ruleset::Ruleset(
    const std::string& name,
    std::vector<std::unique_ptr<DetectorGroup>> detector_groups,
    std::vector<std::unique_ptr<BasePlugin>> action_group,
    bool disable_on_drop_in,
    bool detectorgroups_dropin_enabled,
    bool actiongroup_dropin_enabled,
    uint32_t silence_logs,
    int post_action_delay,
    int prekill_hook_timeout,
    std::unordered_set<CgroupPath> cgroups)
    : name_(name),
      detector_groups_(std::move(detector_groups)),
      action_group_(std::move(action_group)),
      post_action_delay_(post_action_delay),
      prekill_hook_timeout_(prekill_hook_timeout),
      disable_on_drop_in_(disable_on_drop_in),
      detectorgroups_dropin_enabled_(detectorgroups_dropin_enabled),
      actiongroup_dropin_enabled_(actiongroup_dropin_enabled),
      silenced_logs_(silence_logs) {
  cgroups_ = std::move(cgroups);
}

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
  if (!cgroups_.has_value()) {
    return runOnceImpl(context);
  }
  auto visited = std::unordered_set<std::string>();
  uint32_t ret = 0;
  for (const auto& cgroupCtx : context.addToCacheAndGet(cgroups_.value(), {})) {
    auto& cgroup = cgroupCtx.get().cgroup();
    if (!xattr_filter_.empty()) {
      auto maybeHasXattr = Fs::hasxattrAt(cgroupCtx.get().fd(), xattr_filter_);
      if (!maybeHasXattr) {
        OLOG << "Failed to fetch xattr: " << xattr_filter_
             << " for cgroup: " << cgroup.absolutePath()
             << ". Error: " << maybeHasXattr.error().what();
        continue;
      }
      if (!*maybeHasXattr) {
        continue;
      }
    }
    if (!runnable_rulesets_.contains(cgroup.absolutePath())) {
      OLOG << "Adding runnable ruleset for cgroup: " << cgroup.absolutePath();
      registerRunnableRulesetForCgroupPath(context, cgroup);
    }
    context.setRulesetCgroup(cgroup);
    ret = runnable_rulesets_[cgroup.absolutePath()]->runOnceImpl(context);
    visited.insert(cgroup.absolutePath());
  }
  for (auto&& cgroup_it = runnable_rulesets_.begin();
       cgroup_it != runnable_rulesets_.end();
       ++cgroup_it) {
    if (visited.contains(cgroup_it->first)) {
      continue;
    }
    OLOG << "Dropping runnable ruleset for cgroup: " << cgroup_it->first;
    runnable_rulesets_.erase(cgroup_it);
  }
  return ret;
}

uint32_t Ruleset::runOnceImpl(OomdContext& context) {
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
      run_actions = true;
      context.setActionContext(
          {name_,
           dg->name(),
           Util::generateUuid(),
           std::chrono::steady_clock::now() +
               std::chrono::seconds(prekill_hook_timeout_)}),
          context.setInvokingRuleset(this);
    }
  }

  OOMD_SCOPE_EXIT {
    context.setActionContext({"", "", "", std::nullopt});
    context.setInvokingRuleset(std::nullopt);
    context.setRulesetCgroup(std::nullopt);
  };

  // run actions if now() == pause_actions_until_ because a delay of 0 should
  // not cause a pause.
  if (std::chrono::steady_clock::now() < pause_actions_until_) {
    return 0;
  }

  if (active_action_chain_state_ != std::nullopt) {
    // resume the action context from when the action chain was fired
    context.setActionContext(active_action_chain_state_->action_context);

    // clear active_async_plugin_ and save it to a temp
    BasePlugin& target = active_action_chain_state_->active_plugin;
    active_action_chain_state_ = std::nullopt;

    for (auto&& it = action_group_.begin(); it != action_group_.end(); ++it) {
      if (it->get() != &target) {
        continue;
      }
      if (!(silenced_logs_ & LogSources::ENGINE)) {
        std::string target_cgroup;
        if (auto& cgroup = context.getRulesetCgroup()) {
          target_cgroup = cgroup->absolutePath();
        }
        OLOG << "DetectorGroup=" << context.getActionContext().detectorgroup
             << " has fired for Ruleset=" << name_ << " with ruleset cgroup `"
             << target_cgroup << "`. Running action chain from state.";
      }
      return run_action_chain(it, action_group_.end(), context);
    }
  }

  if (!run_actions) {
    return 0;
  }

  if (!(silenced_logs_ & LogSources::ENGINE)) {
    std::string target_cgroup;
    if (auto& cgroup = context.getRulesetCgroup()) {
      target_cgroup = cgroup->absolutePath();
    }
    OLOG << "DetectorGroup=" << context.getActionContext().detectorgroup
         << " has fired for Ruleset=" << name_ << " with ruleset cgroup `"
         << target_cgroup << "`. Running action chain.";
  }

  // Begin running action chain
  return run_action_chain(action_group_.begin(), action_group_.end(), context);
}

uint32_t Ruleset::run_action_chain(
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

        if (!plugin_overrode_post_action_delay_) {
          pause_actions_until_ = std::chrono::steady_clock::now() +
              std::chrono::seconds(post_action_delay_);
        }
        plugin_overrode_post_action_delay_ = false;

        break; // break out of switch
      case PluginRet::ASYNC_PAUSED:
        active_action_chain_state_ = std::make_optional(
            AsyncActionChainState{
                .active_plugin = std::ref(*action.get()),
                .action_context = context.getActionContext()});
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
  pause_actions_until_ = std::chrono::steady_clock::now() + duration;
  plugin_overrode_post_action_delay_ = true;
}

void Ruleset::registerRunnableRulesetForCgroupPath(
    OomdContext& context,
    const CgroupPath& cgroup) {
  auto detector_groups = std::vector<std::unique_ptr<DetectorGroup>>();
  detector_groups.reserve(detector_groups_.size());
  for (auto it = detector_groups_.begin(); it != detector_groups_.end(); ++it) {
    detector_groups.push_back(std::make_unique<DetectorGroup>(*it->get()));
  }
  auto& registry = Oomd::getPluginRegistry();
  auto action_group = std::vector<std::unique_ptr<BasePlugin>>();
  action_group.reserve(action_group_.size());
  for (auto it = action_group_.begin(); it != action_group_.end(); ++it) {
    auto plugin = registry.create(it->get()->getName());
    plugin->setName(it->get()->getName());
    auto args = it->get()->getPluginArgs();
    args.try_emplace("cgroup", cgroup.relativePath());
    plugin->init(args, PluginConstructionContext(cgroup.cgroupFs()));
    action_group.emplace_back(plugin);
  }
  auto ruleset = std::make_unique<Ruleset>(
      name_,
      std::move(detector_groups),
      std::move(action_group),
      disable_on_drop_in_,
      detectorgroups_dropin_enabled_,
      actiongroup_dropin_enabled_,
      silenced_logs_,
      post_action_delay_,
      prekill_hook_timeout_,
      std::unordered_set{CgroupPath(cgroup.cgroupFs(), cgroup.relativePath())});
  ruleset->prerun(context);
  runnable_rulesets_[cgroup.absolutePath()] = std::move(ruleset);
}

} // namespace Engine
} // namespace Oomd
