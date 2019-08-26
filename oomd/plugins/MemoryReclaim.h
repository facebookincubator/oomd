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
#include <unordered_set>

#include "oomd/plugins/BaseKillPlugin.h"

namespace Oomd {

template <typename Base = BaseKillPlugin>
class MemoryReclaim : public Base {
 public:
  int init(
      Engine::MonitoredResources& /* unused */,
      const Engine::PluginArgs& args) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static MemoryReclaim* create() {
    return new MemoryReclaim();
  }

  ~MemoryReclaim() = default;

 private:
  std::unordered_set<CgroupPath> cgroups_;
  int duration_;

  int64_t last_pgscan_{0};
  std::chrono::steady_clock::time_point last_reclaim_at_{};
};

} // namespace Oomd

#include "oomd/plugins/MemoryReclaim-inl.h"
