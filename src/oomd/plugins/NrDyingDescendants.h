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

#include <unordered_set>

#include "oomd/engine/BasePlugin.h"

namespace Oomd {

class NrDyingDescendants : public Oomd::Engine::BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& resources,
      const Engine::PluginArgs& args) override;

  Engine::PluginRet run(OomdContext& ctx) override;

  static NrDyingDescendants* create() {
    return new NrDyingDescendants();
  }

  ~NrDyingDescendants() = default;

 private:
  std::unordered_set<CgroupPath> cgroups_;
  int64_t count_{0};
  bool lte_{true}; // less than or equal
  bool debug_{false};
};

} // namespace Oomd
