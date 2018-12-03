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

#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/DetectorGroup.h"
#include "oomd/shared/OomdContext.h"

namespace Oomd {
namespace Engine {

class Ruleset {
 public:
  Ruleset(
      const std::string& name,
      std::vector<std::unique_ptr<DetectorGroup>> detector_groups,
      std::vector<std::unique_ptr<BasePlugin>> action_group);
  ~Ruleset() = default;

  /*
   * Runs the all the DetectorGroup's. If any of them fires, then begin
   * running the action chain.
   */
  void runOnce(OomdContext& context);

 private:
  std::string name_;
  std::vector<std::unique_ptr<DetectorGroup>> detector_groups_;
  std::vector<std::unique_ptr<BasePlugin>> action_group_;
};

} // namespace Engine
} // namespace Oomd
