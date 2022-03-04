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

  static KillSwapUsage* create() {
    return new KillSwapUsage();
  }

  ~KillSwapUsage() = default;

 protected:
  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) override;

  int64_t getSwapExcess(const CgroupContext& cgroup_ctx);

  void ologKillTarget(
      OomdContext& ctx,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& peers) override;

  // Default threshold is to kill something with non-zero swap usage
  int64_t threshold_{1};
  float swapRatio_{0.0};
  bool biasedSwapKill_{false};
};

} // namespace Oomd

#include "oomd/plugins/KillSwapUsage-inl.h"
