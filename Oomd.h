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

#include <atomic>
#include <chrono>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "oomd/engine/Engine.h"

namespace Oomd {

class Oomd {
 public:
  Oomd(std::unique_ptr<Engine::Engine> engine, int interval);
  virtual ~Oomd() = default;

  /*
   * This method takes a @param parent_cgroup rooted at
   * @param cgroup_root_dir to update @param ctx with.
   *
   * For example, consider the following tree:
   *   |/sys/fs/cgroup/
   *   |--system.slice
   *   |--|--chef.service
   *   |--|--cron.service
   *   |--workload.slice
   *   |--|--myworkload.slice
   *
   * If @param cgroup_root_dir == /sys/fs/cgroup and @param parent_cgroups ==
   * {"system.slice"}, then @param ctx will be updated with the status of
   * chef.service and cron.service.
   */
  void updateContext(
      const std::string& cgroup_root_dir,
      const std::unordered_set<std::string>& parent_cgroups,
      OomdContext& ctx);
  bool updateContext(
      const std::string& parent_cgroup,
      const std::string& absolute_cgroup_path,
      OomdContext& ctx);

  int run();

 private:
  // runtime settings
  std::chrono::seconds interval_{0};
  const double average_size_decay_{
      4}; // TODO(dlxu): migrate to ring buffer for raw datapoints so plugins
          // can calculate weighted average themselves
  std::unique_ptr<Engine::Engine> engine_;
  std::unordered_set<std::string> warned_io_pressure_;
  std::unordered_set<std::string> warned_mem_controller_;
};

} // namespace Oomd
