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

#include <mutex>
#include <optional>
#include <utility>
#include <vector>

#include "oomd/config/ConfigCompiler.h"

namespace Oomd {

namespace Config2::IR {
struct Root;
}
namespace Engine {
class Engine;
}

/**
 * Adaptor between services that handles oomd drop in configs and the oomd core.
 */
class DropInServiceAdaptor {
 public:
  /**
   * Both root and engine are from the oomd core and stored as references. They
   * are guaranteed to be valid when accessed as they are only used in
   * updateDropIns() which is called by the oomd core event loop.
   */
  DropInServiceAdaptor(
      const std::string& cgroup_fs,
      const Config2::IR::Root& root,
      Engine::Engine& engine)
      : cgroup_fs_(cgroup_fs), root_(root), engine_(engine) {}

  virtual ~DropInServiceAdaptor() = default;

  /**
   * Called by Oomd core in event loop every interval. Drop in config changes
   * received by the service will be applied to the engine. Also update service
   * internal states with virtual functions below.
   */
  void updateDropIns();

 protected:
  virtual void tick() = 0;
  virtual void handleDropInAddResult(const std::string& tag, bool ok) = 0;
  virtual void handleDropInRemoveResult(const std::string& tag, bool ok) = 0;

  /**
   * For drop in service to add or remove configs from core asynchronously.
   * Adding drop in may fail if materialization fails.
   */
  bool scheduleDropInAdd(
      const std::string& tag,
      const Config2::IR::Root& drop_in);
  void scheduleDropInRemove(const std::string& tag);

 private:
  std::string cgroup_fs_;
  const Config2::IR::Root& root_;
  Engine::Engine& engine_;

  std::mutex queue_mutex_;
  std::vector<std::pair<std::string, std::optional<Config2::DropInUnit>>>
      drop_in_queue_;
};

} // namespace Oomd
