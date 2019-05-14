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

#include "oomd/OomdContext.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/DetectorGroup.h"

namespace Oomd {
namespace Engine {

class Ruleset {
 public:
  Ruleset(
      const std::string& name,
      std::vector<std::unique_ptr<DetectorGroup>> detector_groups,
      std::vector<std::unique_ptr<BasePlugin>> action_group,
      bool disable_on_drop_in = false,
      bool detectorgroups_dropin_enabled = false,
      bool actiongroup_dropin_enabled = false);
  ~Ruleset() = default;

  /*
   * Merges this ruleset with @param ruleset with priority given to @param
   * ruleset.
   *
   * @returns false if @param ruleset tries to override configs to ruleset
   * members which do not have drop in turned on. true otherwise.
   */
  [[nodiscard]] bool mergeWithDropIn(std::unique_ptr<Ruleset> ruleset);

  /*
   * Mark/unmark this ruleset as being targeted by an active drop in.
   */
  void markDropInTargeted();
  void markDropInUntargeted();

  /*
   * Runs the all the DetectorGroup's. If any of them fires, then begin
   * running the action chain.
   */
  void runOnce(OomdContext& context);

  const std::string& getName() const {
    return name_;
  }

 private:
  std::string name_;
  std::vector<std::unique_ptr<DetectorGroup>> detector_groups_;
  std::vector<std::unique_ptr<BasePlugin>> action_group_;
  bool enabled_{true};
  bool disable_on_drop_in_{false};
  bool detectorgroups_dropin_enabled_{false};
  bool actiongroup_dropin_enabled_{false};
  int32_t numTargeted_{0};
};

} // namespace Engine
} // namespace Oomd
