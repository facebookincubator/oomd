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
#include "oomd/engine/Ruleset.h"

namespace {

template <typename T>
std::unique_ptr<Oomd::Engine::BasePlugin> compilePlugin(
    Oomd::Engine::MonitoredResources& resources,
    const T& plugin) {
  if (plugin.name.empty()) {
    OLOG << "Plugin is missing name";
    return nullptr;
  }

  std::unique_ptr<Oomd::Engine::BasePlugin> instance(
      Oomd::getPluginRegistry().create(plugin.name));
  if (!instance) {
    OLOG << "Could not locate plugin=" << plugin.name << " in plugin registry";
    return nullptr;
  }

  instance->setName(plugin.name);

  int ret = instance->init(resources, plugin.args);
  if (ret != 0) {
    OLOG << "Plugin=" << plugin.name << " failed to init() with code=" << ret;
    return nullptr;
  }

  return instance;
}

std::unique_ptr<Oomd::Engine::DetectorGroup> compileDetectorGroup(
    Oomd::Engine::MonitoredResources& resources,
    const Oomd::Config2::IR::DetectorGroup& group) {
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
        compilePlugin<Oomd::Config2::IR::Detector>(resources, detector);
    if (!compiled_plugin) {
      return nullptr;
    }

    detectors.emplace_back(std::move(compiled_plugin));
  }

  return std::make_unique<Oomd::Engine::DetectorGroup>(
      group.name, std::move(detectors));
}

std::unique_ptr<Oomd::Engine::Ruleset> compileRuleset(
    Oomd::Engine::MonitoredResources& resources,
    const Oomd::Config2::IR::Ruleset& ruleset) {
  std::vector<std::unique_ptr<Oomd::Engine::DetectorGroup>> detector_groups;
  std::vector<std::unique_ptr<Oomd::Engine::BasePlugin>> actions;

  if (ruleset.name.empty()) {
    OLOG << "Ruleset is missing name";
    return nullptr;
  }

  if (ruleset.dgs.empty() || ruleset.acts.empty()) {
    OLOG << "Ruleset is missing DetectorGroups or missing Actions";
    return nullptr;
  }

  for (const auto& dg : ruleset.dgs) {
    auto compiled_detectorgroup = compileDetectorGroup(resources, dg);
    if (!compiled_detectorgroup) {
      return nullptr;
    }

    detector_groups.emplace_back(std::move(compiled_detectorgroup));
  }

  for (const auto& action : ruleset.acts) {
    auto compiled_action =
        compilePlugin<Oomd::Config2::IR::Action>(resources, action);
    if (!compiled_action) {
      return nullptr;
    }

    actions.emplace_back(std::move(compiled_action));
  }

  return std::make_unique<Oomd::Engine::Ruleset>(
      ruleset.name, std::move(detector_groups), std::move(actions));
}

} // namespace

namespace Oomd {
namespace Config2 {

std::unique_ptr<Engine::Engine> compile(const IR::Root& root) {
  Engine::MonitoredResources resources;
  std::vector<std::unique_ptr<Engine::Ruleset>> rulesets;

  if (root.version.empty()) {
    OLOG << "No version string specified";
    return nullptr;
  }

  for (const auto& ruleset : root.rulesets) {
    auto compiled_ruleset = compileRuleset(resources, ruleset);
    if (!compiled_ruleset) {
      return nullptr;
    }

    rulesets.emplace_back(std::move(compiled_ruleset));
  }

  return std::make_unique<Engine::Engine>(
      std::move(resources), std::move(rulesets));
}

} // namespace Config2
} // namespace Oomd
