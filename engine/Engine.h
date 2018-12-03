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

#include <memory>
#include <vector>

#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/Ruleset.h"
#include "oomd/shared/OomdContext.h"

namespace Oomd {
namespace Engine {

class Engine {
 public:
  Engine(
      MonitoredResources resources,
      std::vector<std::unique_ptr<Ruleset>> rulesets);
  ~Engine() = default;

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
  MonitoredResources resources_;
  std::vector<std::unique_ptr<Ruleset>> rulesets_;
};

} // namespace Engine
} // namespace Oomd
