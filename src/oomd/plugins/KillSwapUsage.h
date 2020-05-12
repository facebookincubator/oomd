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

#include <chrono>

#include "oomd/plugins/BaseKillPlugin.h"

namespace Oomd {

template <typename Base = BaseKillPlugin>
class KillSwapUsage : public Base {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& ctx) override;

  static KillSwapUsage* create() {
    return new KillSwapUsage();
  }

  ~KillSwapUsage() = default;

 protected:
  virtual bool tryToKillSomething(OomdContext& ctx);

  std::unordered_set<CgroupPath> cgroups_;
  // Default threshold is to kill something with non-zero swap usage
  int64_t threshold_{1};
  int post_action_delay_{15};
  bool dry_{false};
  bool debug_{false};
};

} // namespace Oomd

#include "oomd/plugins/KillSwapUsage-inl.h"
