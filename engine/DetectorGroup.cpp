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

#include "oomd/engine/DetectorGroup.h"
#include "oomd/Log.h"
#include "oomd/shared/OomdContext.h"

namespace Oomd {
namespace Engine {

DetectorGroup::DetectorGroup(
    const std::string& name,
    std::vector<std::unique_ptr<BasePlugin>> detectors)
    : name_(name), detectors_(std::move(detectors)) {}

bool DetectorGroup::check(OomdContext& context) {
  for (const auto& detector : detectors_) {
    OLOG << "Running Detector=" << detector->getName();
    PluginRet ret = detector->run(context);

    switch (ret) {
      case PluginRet::CONTINUE:
        OLOG << "Detector=" << detector->getName()
             << " returned CONTINUE. Continuing detector chain.";
        continue;
      case PluginRet::STOP:
        OLOG << "Detector=" << detector->getName()
             << " returned STOP. Terminating detector chain.";
        return false;
        // missing default to protect against future PluginRet vals
    }
  }

  return true;
}

const std::string& DetectorGroup::name() const {
  return name_;
}

} // namespace Engine
} // namespace Oomd
