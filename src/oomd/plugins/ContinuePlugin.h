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

namespace Oomd {

class ContinuePlugin : public Engine::BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& /* unused */,
      const Engine::PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 0;
  }

  Engine::PluginRet run(OomdContext& /* unused */) override {
    return Engine::PluginRet::CONTINUE;
  }

  static ContinuePlugin* create() {
    return new ContinuePlugin();
  }

  ~ContinuePlugin() = default;
};

} // namespace Oomd
