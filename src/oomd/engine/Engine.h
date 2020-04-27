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

#pragma once

#include <cstdint>
#include <deque>
#include <memory>
#include <vector>

#include "oomd/OomdContext.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/Ruleset.h"

namespace Oomd {
namespace Engine {

class Engine {
 public:
  Engine(
      MonitoredResources resources,
      std::vector<std::unique_ptr<Ruleset>> rulesets);
  ~Engine() = default;

  /*
   * Adds a drop in config to the running engine.
   *
   * @param tag is a unique tag to be associated with the drop
   * in config. Removing the dropped in config requires the original tag.
   *
   * @returns false if @param ruleset's target is not found. true otherwise.
   */
  bool addDropInConfig(size_t tag, std::unique_ptr<Ruleset> ruleset);

  /*
   * Removes drop in configs associated with @param tag
   */
  void removeDropInConfig(size_t tag);

  /*
   * Preruns every @class Ruleset once.
   */
  void prerun(OomdContext& context);

  /*
   * Runs every @class Ruleset once.
   */
  void runOnce(OomdContext& context);

  /*
   * This resources instance is passed to all plugins that will run
   * inside this engine. Plugins can then declare what resources they
   * need with the engine. Then the oomd runtime can extract what resources
   * it will need to poll.
   */
  const MonitoredResources& getMonitoredResources() const;

 private:
  struct DropInRuleset {
    size_t tag{0}; // required field
    std::unique_ptr<Ruleset> ruleset;
  };

  struct BaseRuleset {
    std::unique_ptr<Ruleset> ruleset;
    std::deque<DropInRuleset> dropins;
  };

  MonitoredResources resources_;
  std::vector<BaseRuleset> rulesets_;
};

} // namespace Engine
} // namespace Oomd
