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
#include "oomd/OomdContext.h"
#include "oomd/engine/EngineTypes.h"

namespace Oomd {
namespace Engine {

DetectorGroup::DetectorGroup(
    const std::string& name,
    std::vector<std::unique_ptr<BasePlugin>> detectors)
    : name_(name), detectors_(std::move(detectors)) {}

void DetectorGroup::prerun(OomdContext& context) {
  for (const auto& detector : detectors_) {
    detector->prerun(context);
  }
}

bool DetectorGroup::check(OomdContext& context, uint32_t silenced_logs) {
  // Note we're running all Detectors so that any detectors keeping sliding
  // windows can update their window
  bool triggered = true;

  for (const auto& detector : detectors_) {
    if (silenced_logs & LogSources::PLUGINS) {
      OLOG << LogStream::Control::DISABLE;
    }

    PluginRet ret = detector->run(context);

    if (silenced_logs & LogSources::PLUGINS) {
      OLOG << LogStream::Control::ENABLE;
    }

    switch (ret) {
      case PluginRet::CONTINUE:
        continue;
      case PluginRet::STOP:
        triggered = false;
        break;
        // missing default to protect against future PluginRet vals
    }
  }

  return triggered;
}

const std::string& DetectorGroup::name() const {
  return name_;
}

} // namespace Engine
} // namespace Oomd
