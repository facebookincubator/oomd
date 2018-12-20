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

#include <string>

#include "oomd/Log.h"

namespace Oomd {

template <typename Base>
int SystemdRestart<Base>::init(
    Engine::MonitoredResources& resources,
    const Engine::PluginArgs& args) {
  if (args.find("service") != args.end()) {
    const std::string& service = args.at("service");
    if (service.empty()) {
      OLOG << "Argument=service is empty";
    }
    service_ = service;
  } else {
    OLOG << "Argument=service not present";
    return 1;
  }

  if (args.find("post_action_delay") != args.end()) {
    int val = std::stoi(args.at("post_action_delay"));

    if (val < 0) {
      OLOG << "Argument=post_action_delay must be non-negative";
      return 1;
    }

    post_action_delay_ = val;
  }

  if (args.find("dry") != args.end()) {
    const std::string& val = args.at("dry");

    if (val == "true" || val == "True" || val == "1") {
      dry_ = true;
    }
  }

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
    std::this_thread::sleep_for(std::chrono::seconds(post_action_delay_));
    return Engine::PluginRet::STOP;
  } else {
    return Engine::PluginRet::CONTINUE;
  }
}

} // namespace Oomd
