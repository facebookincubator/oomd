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

#include "oomd/dropin/DropInServiceAdaptor.h"

#include "oomd/PluginConstructionContext.h"
#include "oomd/config/ConfigTypes.h"
#include "oomd/engine/Engine.h"

namespace Oomd {

void DropInServiceAdaptor::updateDropIns() {
  decltype(drop_in_queue_) drop_in_queue;

  tick();

  {
    std::lock_guard<std::mutex> lock(queue_mutex_);
    drop_in_queue = std::move(drop_in_queue_);
  }

  for (auto& [tag, unit] : drop_in_queue) {
    // First remove then re-add. We don't do in place modifications as it'll
    // be complicated for the code and it probably wouldn't be what the user
    // expects. The user probably expects the entire drop in config is reset
    // and added to the front of the LIFO queue.
    engine_.removeDropInConfig(tag);

    if (!unit) {
      // If unit is nullopt, we just need to remove it
      handleDropInRemoveResult(tag, true);
    } else {
      bool drop_in_add_ok = true;
      for (auto& drop_in : unit->rulesets) {
        if (!engine_.addDropInConfig(tag, std::move(drop_in))) {
          // Clean up partial added drop ins
          engine_.removeDropInConfig(tag);
          drop_in_add_ok = false;
          break;
        }
      }
      handleDropInAddResult(tag, drop_in_add_ok);
    }
  }
}

bool DropInServiceAdaptor::scheduleDropInAdd(
    const std::string& tag,
    const Config2::IR::Root& drop_in) {
  const PluginConstructionContext compile_context(cgroup_fs_);
  auto unit = Config2::compileDropIn(root_, drop_in, compile_context);
  if (!unit.has_value()) {
    return false;
  }

  std::lock_guard<std::mutex> lock(queue_mutex_);
  drop_in_queue_.emplace_back(tag, std::move(unit.value()));
  return true;
}

void DropInServiceAdaptor::scheduleDropInRemove(const std::string& tag) {
  std::lock_guard<std::mutex> lock(queue_mutex_);
  drop_in_queue_.emplace_back(tag, std::nullopt);
}

} // namespace Oomd
