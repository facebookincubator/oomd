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
   * This method updates @param ctx with the status of all the cgroups
   * in @param cgroups. @param cgroup_root_dir is the location the cgroup2
   * filesystem is mounted.
   *
   * Every cgroup in @param cgroups will be treated relative to
   * @param cgroup_root_dir.
   */
  void updateContext(
      const std::string& cgroup_root_dir,
      const std::unordered_set<std::string>& cgroups,
      OomdContext& ctx);

  int run();

 private:
  bool updateContextCgroup(
      const std::string& relative_cgroup_path,
      const std::string& absolute_cgroup_path,
      OomdContext& ctx);

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
