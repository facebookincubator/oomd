/* Copyright (C) 2018-present, Facebook, Inc.
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

#include "oomd/config/ConfigCompiler.h"

#include <vector>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/DetectorGroup.h"
#include "oomd/engine/EngineTypes.h"
#include "oomd/engine/Ruleset.h"
#include "oomd/util/Util.h"

namespace {

template <typename T, typename PluginT>
std::unique_ptr<PluginT> compilePluginGeneric(
    Oomd::PluginRegistry<PluginT>& registry,
    const T& plugin,
    const Oomd::PluginConstructionContext& context) {
  if (plugin.name.empty()) {
    OLOG << "Plugin is missing name";
    return nullptr;
  }

  std::unique_ptr<PluginT> instance(registry.create(plugin.name));
  if (!instance) {
    OLOG << "Could not locate plugin=" << plugin.name << " in plugin registry";
    return nullptr;
  }

  instance->setName(plugin.name);

  int ret = instance->init(plugin.args, context);
  if (ret != 0) {
    OLOG << "Plugin=" << plugin.name << " failed to init() with code=" << ret;
    return nullptr;
  }

  return instance;
}

template <typename T>
std::unique_ptr<Oomd::Engine::BasePlugin> compilePlugin(
    const T& plugin,
    const Oomd::PluginConstructionContext& context) {
  return compilePluginGeneric<T, Oomd::Engine::BasePlugin>(
      Oomd::getPluginRegistry(), plugin, context);
}

std::unique_ptr<Oomd::Engine::PrekillHook> compilePrekillHook(
    const Oomd::Config2::IR::PrekillHook& hook,
    const Oomd::PluginConstructionContext& context) {
  return compilePluginGeneric<
      Oomd::Config2::IR::PrekillHook,
      Oomd::Engine::PrekillHook>(Oomd::getPrekillHookRegistry(), hook, context);
}

std::unique_ptr<Oomd::Engine::DetectorGroup> compileDetectorGroup(
    const Oomd::Config2::IR::DetectorGroup& group,
    const Oomd::PluginConstructionContext& context) {
  std::vector<std::unique_ptr<Oomd::Engine::BasePlugin>> detectors;

  if (group.name.empty()) {
    OLOG << "DetectorGroup is missing name";
    return nullptr;
  }

  if (group.detectors.empty()) {
    OLOG << "DetectorGroup is missing Detectors";
    return nullptr;
  }

  for (const auto& detector : group.detectors) {
    auto compiled_plugin =
        compilePlugin<Oomd::Config2::IR::Detector>(detector, context);
    if (!compiled_plugin) {
      return nullptr;
    }

    detectors.emplace_back(std::move(compiled_plugin));
  }

  return std::make_unique<Oomd::Engine::DetectorGroup>(
      group.name, std::move(detectors));
}

std::unique_ptr<Oomd::Engine::Ruleset> compileRuleset(
    const Oomd::Config2::IR::Ruleset& ruleset,
    bool dropin,
    const Oomd::PluginConstructionContext& context) {
  uint32_t silenced_logs = 0;
  int post_action_delay = DEFAULT_POST_ACTION_DELAY;

  std::vector<std::unique_ptr<Oomd::Engine::DetectorGroup>> detector_groups;
  std::vector<std::unique_ptr<Oomd::Engine::BasePlugin>> actions;

  if (ruleset.name.empty()) {
    OLOG << "Ruleset is missing name";
    return nullptr;
  }

  // Log silencing field is optional
  if (ruleset.silence_logs.size()) {
    auto copy = ruleset.silence_logs;
    Oomd::Util::trim(copy);
    auto parts = Oomd::Util::split(copy, ',');

    for (auto& part : parts) {
      Oomd::Util::trim(part);

      if (part == "engine") {
        silenced_logs |= Oomd::Engine::LogSources::ENGINE;
      } else if (part == "plugins") {
        silenced_logs |= Oomd::Engine::LogSources::PLUGINS;
      } else {
        OLOG << "Unrecognized log source=" << part;
        return nullptr;
      }
    }
  }

  if (!dropin && (ruleset.dgs.empty() || ruleset.acts.empty())) {
    OLOG << "Ruleset is missing DetectorGroups or missing Actions";
    return nullptr;
  }

  // post_action_delay field is optional
  if (ruleset.post_action_delay.size()) {
    post_action_delay = std::stoi(ruleset.post_action_delay);
    if (post_action_delay < 0) {
      OLOG << "Ruleset post_action_delay must be non-negative";
      return nullptr;
    }
  }

  for (const auto& dg : ruleset.dgs) {
    auto compiled_detectorgroup = compileDetectorGroup(dg, context);
    if (!compiled_detectorgroup) {
      return nullptr;
    }

    detector_groups.emplace_back(std::move(compiled_detectorgroup));
  }

  for (const auto& action : ruleset.acts) {
    auto compiled_action =
        compilePlugin<Oomd::Config2::IR::Action>(action, context);
    if (!compiled_action) {
      return nullptr;
    }

    actions.emplace_back(std::move(compiled_action));
  }

  return std::make_unique<Oomd::Engine::Ruleset>(
      ruleset.name,
      std::move(detector_groups),
      std::move(actions),
      ruleset.dropin.disable_on_drop_in,
      ruleset.dropin.detectorgroups_enabled,
      ruleset.dropin.actiongroup_enabled,
      silenced_logs,
      post_action_delay);
}

} // namespace

namespace Oomd {
namespace Config2 {

std::unique_ptr<Engine::Engine> compile(
    const IR::Root& root,
    const PluginConstructionContext& context) {
  std::vector<std::unique_ptr<Engine::Ruleset>> rulesets;

  for (const auto& ruleset : root.rulesets) {
    auto compiled_ruleset = compileRuleset(ruleset, false, context);
    if (!compiled_ruleset) {
      return nullptr;
    }

    rulesets.emplace_back(std::move(compiled_ruleset));
  }

  std::vector<std::unique_ptr<Engine::PrekillHook>> prekill_hooks;

  for (const auto& prekill_hook : root.prekill_hooks) {
    auto compiled_prekill_hook_plugin =
        compilePrekillHook(prekill_hook, context);
    if (!compiled_prekill_hook_plugin) {
      return nullptr;
    }

    prekill_hooks.emplace_back(std::move(compiled_prekill_hook_plugin));
  }

  if (prekill_hooks.size() > 1) {
    OLOG << "Config cannot have more than 1 prekill hook";
    return nullptr;
  }

  return std::make_unique<Engine::Engine>(
      std::move(rulesets), std::move(prekill_hooks));
}

std::optional<DropInUnit> compileDropIn(
    const IR::Root& root,
    const IR::Root& dropin,
    const PluginConstructionContext& context) {
  DropInUnit ret;

  for (const auto& dropin_rs : dropin.rulesets) {
    bool found_target = false;

    for (const auto& rs : root.rulesets) {
      if (rs.name == dropin_rs.name) {
        found_target = true;

        auto target = compileRuleset(rs, false, context);
        if (!target) {
          return std::nullopt;
        }

        auto compiled_drop = compileRuleset(dropin_rs, true, context);
        if (!compiled_drop) {
          return std::nullopt;
        }

        if (!target->mergeWithDropIn(std::move(compiled_drop))) {
          OLOG << "Could not merge drop in ruleset=" << dropin_rs.name;
          return std::nullopt;
        }

        ret.rulesets.emplace_back(std::move(target));
        break;
      }
    }

    if (!found_target) {
      OLOG << "Could not locate targeted ruleset=" << dropin_rs.name;
      return std::nullopt;
    }
  }

  return ret;
}

} // namespace Config2
} // namespace Oomd
