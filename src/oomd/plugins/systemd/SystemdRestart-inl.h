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

#include "oomd/plugins/systemd/SystemdRestart.h"

#include <stdexcept>
#include <string>

#include "oomd/Log.h"
#include "oomd/Stats.h"

namespace Oomd {

template <typename Base>
int SystemdRestart<Base>::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& /* unused */) {
  this->argParser_.addArgumentCustom(
      "service",
      service_,
      [](const std::string& str) {
        if (str.empty()) {
          throw std::invalid_argument("service is empty");
        }
        return str;
      },
      true);

  this->argParser_.addArgumentCustom(
      "post_action_delay",
      post_action_delay_,
      PluginArgParser::parseUnsignedInt);

  this->argParser_.addArgument("dry", dry_);

  if (!this->argParser_.parse(args)) {
    return 1;
  }

  Oomd::setStat(kRestartsKey, 0);

  // Success
  return 0;
}

template <typename Base>
Engine::PluginRet SystemdRestart<Base>::run(OomdContext& /* unused */) {
  bool ret;
  if (dry_) {
    OLOG << "DRY-RUN: restarting service " << service_;
    ret = true;
  } else {
    ret = Base::restartService(service_);
  }

  if (ret) {
    std::ostringstream oss;
    oss << "restarted systemd service=" << service_ << (dry_ ? " (dry)" : "");
    OOMD_KMSG_LOG(oss.str(), "oomd kill");
    Oomd::incrementStat(kRestartsKey, 1);
    std::this_thread::sleep_for(std::chrono::seconds(post_action_delay_));
    return Engine::PluginRet::STOP;
  } else {
    return Engine::PluginRet::CONTINUE;
  }
}

} // namespace Oomd
