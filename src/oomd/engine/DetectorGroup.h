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
#include <string>
#include <vector>

#include "oomd/OomdContext.h"
#include "oomd/engine/BasePlugin.h"

namespace Oomd {
namespace Engine {

class DetectorGroup {
 public:
  DetectorGroup(
      const std::string& name,
      std::vector<std::unique_ptr<BasePlugin>> detectors);
  ~DetectorGroup() = default;

  /*
   * Prerun all plugins in this detector group.
   */
  void prerun(OomdContext& context);

  /*
   * @return true if no @class Detector returns PluginRet::STOP.
   */
  bool check(OomdContext& context, uint32_t silenced_logs);

  const std::string& name() const;

 private:
  std::string name_;
  std::vector<std::unique_ptr<BasePlugin>> detectors_;
};

} // namespace Engine
} // namespace Oomd
