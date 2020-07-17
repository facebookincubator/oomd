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
#include <thread>

#include "oomd/dropin/DropInServiceAdaptor.h"

namespace Oomd {

class FsDropInService : public DropInServiceAdaptor {
 private:
  // Hide constructor
  struct Tag {};

 public:
  // Return constructed service object or nullptr if anything fails
  static std::unique_ptr<FsDropInService> create(
      const std::string& cgroup_fs,
      const Config2::IR::Root& root,
      Engine::Engine& engine,
      const std::string& drop_in_dir);
  // Public constructor hidden by Tag, so make_unique works
  FsDropInService(
      const Tag& tag,
      const std::string& cgroup_fs,
      const Config2::IR::Root& root,
      Engine::Engine& engine,
      const std::string& drop_in_dir,
      int epollfd,
      int terminatefd);
  ~FsDropInService() override;

 protected:
  virtual void tick() override;
  virtual void handleDropInAddResult(const std::string& tag, bool ok) override;
  virtual void handleDropInRemoveResult(const std::string& tag, bool ok)
      override;

 private:
  int prepDropInWatcher(const std::string& dir);
  int prepDropInWatcherEventLoop(const std::string& dir);
  int deregisterDropInWatcherFromEventLoop();
  int prepEventLoop(const std::chrono::seconds& interval);
  void processDropInRemove(const std::string& file);
  void processDropInAdd(const std::string& file);
  int processDropInWatcher(int fd);
  int processEventLoop();
  void run();

  int epollfd_{-1};
  int terminatefd_{-1};
  int inotifyfd_{-1};
  int inotifywd_{-1};
  std::atomic_bool drop_in_dir_deleted_{false};
  std::string drop_in_dir_;
  std::thread event_loop_;
  std::mutex event_loop_mutex_;
};

} // namespace Oomd
