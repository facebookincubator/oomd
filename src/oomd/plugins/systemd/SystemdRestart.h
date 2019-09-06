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

#include "oomd/plugins/systemd/BaseSystemdPlugin.h"

namespace Oomd {

template <typename Base = BaseSystemdPlugin>
class SystemdRestart : public Base {
 public:
  int init(
      Engine::MonitoredResources& resources,
      const Engine::PluginArgs& args) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static SystemdRestart* create() {
    return new SystemdRestart();
  }

  ~SystemdRestart() = default;

 private:
  std::string service_;
  int post_action_delay_{15};
  bool dry_{false};
  static constexpr auto kRestartsKey = "oomd.restarts_structured";
};

} // namespace Oomd

#include "oomd/plugins/systemd/SystemdRestart-inl.h"
