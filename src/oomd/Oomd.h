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
#include <memory>
#include <string>
#include <unordered_map>

#include "oomd/config/ConfigTypes.h"
#include "oomd/engine/Engine.h"

namespace Oomd {

class Oomd {
 public:
  Oomd(
      std::unique_ptr<Config2::IR::Root> ir_root,
      std::unique_ptr<Engine::Engine> engine,
      int interval,
      const std::string& cgroup_fs,
      const std::string& drop_in_dir,
      const std::unordered_map<std::string, DeviceType>& io_devs = {},
      const IOCostCoeffs& hdd_coeffs = {},
      const IOCostCoeffs& ssd_coeffs = {});
  virtual ~Oomd() = default;

  void updateContext();
  int run();

 private:
  int prepDropInWatcher(const std::string& dir);
  int prepDropInWatcherEventLoop(const std::string& dir);
  int deregisterDropInWatcherFromEventLoop();
  int prepEventLoop(const std::chrono::seconds& interval);
  void processDropInRemove(const std::string& file);
  void processDropInAdd(const std::string& file);
  int processDropInWatcher(int fd);
  int processEventLoop();

  int epollfd_{-1};
  int timerfd_{-1};
  int inotifyfd_{-1};
  int inotifywd_{-1};
  bool drop_in_dir_deleted_{false};

  // runtime settings
  std::chrono::seconds interval_{0};
  std::string cgroup_fs_;
  std::unique_ptr<Config2::IR::Root> ir_root_;
  std::unique_ptr<Engine::Engine> engine_;
  std::string drop_in_dir_;

  OomdContext ctx_;
};

} // namespace Oomd
