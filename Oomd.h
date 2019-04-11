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
#include "oomd/include/CgroupPath.h"

namespace Oomd {

class Oomd {
 public:
  Oomd(
      std::unique_ptr<Engine::Engine> engine,
      int interval,
      const std::string& cgroup_fs);
  virtual ~Oomd() = default;

  /*
   * This method updates @param ctx with the status of all the cgroups
   * in @param cgroups.
   */
  void updateContext(
      const std::unordered_set<CgroupPath>& cgroups,
      OomdContext& ctx);

  int run();

 private:
  /*
   * Calculate @param cgroup memory protection, taking into account actual
   * distribution of memory protection.
   *
   * Let's say L(cgrp) is the protection amount a cgroup has according to its
   * own config, P(cgrp) is the amount of actual protection it gets.
   *
   * Let L(cgrp) = min(cgrp.memory.current, max(cgrp.memory.min,
   * cgrp.memory.low))
   *
   * Then, P(cgpr) = P(parent) * L(cgrp) / (Sum of L(child) for
   * each child of parent)
   */
  int64_t calculateProtection(
      const CgroupPath& cgroup,
      OomdContext& ctx,
      std::unordered_map<CgroupPath, int64_t>& cache);
  bool updateContextCgroup(const CgroupPath& path, OomdContext& ctx);
  int prepEventLoop(const std::chrono::seconds& interval);
  int processEventLoop();

  int epollfd_{-1};
  int timerfd_{-1};

  // runtime settings
  std::chrono::seconds interval_{0};
  std::string cgroup_fs_;
  const double average_size_decay_{
      4}; // TODO(dlxu): migrate to ring buffer for raw datapoints so plugins
          // can calculate weighted average themselves
  std::unique_ptr<Engine::Engine> engine_;
  Engine::MonitoredResources resources_;
  std::unordered_set<std::string> warned_io_pressure_;
  std::unordered_set<std::string> warned_mem_controller_;
};

} // namespace Oomd
