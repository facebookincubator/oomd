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

#include <string>
#include <unordered_set>

namespace Oomd {

class DumpCgroupOverview : public Engine::BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& /* unused */,
      std::unordered_map<std::string, std::string> args) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static DumpCgroupOverview* create() {
    return new DumpCgroupOverview();
  }

  ~DumpCgroupOverview() = default;

 private:
  std::unordered_set<std::string> cgroups_;
  std::string cgroup_fs_;
  bool always_{false};
};

} // namespace Oomd
