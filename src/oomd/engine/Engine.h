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
  explicit Engine(std::vector<std::unique_ptr<Ruleset>> rulesets);
  ~Engine() = default;

  /*
   * Adds a drop in config to the running engine.
   *
   * @param tag is a unique tag to be associated with the drop
   * in config. Removing the dropped in config requires the original tag.
   *
   * @returns false if @param ruleset's target is not found. true otherwise.
   */
  bool addDropInConfig(
      const std::string& tag,
      std::unique_ptr<Ruleset> ruleset);

  /*
   * Removes drop in configs associated with @param tag
   */
  void removeDropInConfig(const std::string& tag);

  /*
   * Preruns every @class Ruleset once.
   */
  void prerun(OomdContext& context);

  /*
   * Runs every @class Ruleset once.
   */
  void runOnce(OomdContext& context);

 private:
  struct DropInRuleset {
    std::string tag; // required field
    std::unique_ptr<Ruleset> ruleset;
  };

  struct BaseRuleset {
    std::unique_ptr<Ruleset> ruleset;
    std::deque<DropInRuleset> dropins;
  };

  std::vector<BaseRuleset> rulesets_;
};

} // namespace Engine
} // namespace Oomd
