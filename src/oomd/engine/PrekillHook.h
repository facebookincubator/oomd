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

#include <memory>
#include "oomd/CgroupContext.h"
#include "oomd/PluginConstructionContext.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/Types.h"

namespace Oomd {
namespace Engine {

/*
 * A PrekillHook subclass's fire() is expected to start work that takes multiple
 * ticks of the main loop to finish and should not block. Instead, do the work
 * asynchronously and return an instance of PrekillHookInvocation whose
 * didFinish() will be polled.
 * All functions will only be called from main thread, including fire(),
 * didFinish(), and ~PrekillHookInvocation. None should block for long times.
 * See docs/prekill_hooks.md for details.
 */

class PrekillHookInvocation {
 public:
  virtual bool didFinish() = 0;
  virtual ~PrekillHookInvocation() = default;
};

class PrekillHook {
 public:
  PrekillHook() = default;
  virtual ~PrekillHook() = default;

  PrekillHook(const PrekillHook&) = delete;
  PrekillHook& operator=(const PrekillHook&) = delete;

  virtual int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) {
    return 0;
  }

  virtual std::unique_ptr<PrekillHookInvocation> fire(
      const CgroupContext& cgroup_ctx) = 0;

  virtual void setName(const std::string& name) {
    name_ = name;
  }
  virtual const std::string& getName() const {
    return name_;
  }

 private:
  std::string name_;
};

} // namespace Engine
} // namespace Oomd
