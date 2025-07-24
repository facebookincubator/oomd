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
#include <optional>
#include <unordered_set>
#include <vector>
#include "oomd/CgroupContext.h"
#include "oomd/PluginConstructionContext.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/Types.h"
#include "oomd/util/PluginArgParser.h"
#include "oomd/util/Util.h"

namespace Oomd {
struct ActionContext;
}

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

  int initPlugin(
      const PluginArgs& args,
      const PluginConstructionContext& context) {
    return init(args, context);
  }

  virtual int init(
      const PluginArgs& args,
      const PluginConstructionContext& context) {
    argParser_.addArgumentCustom(
        "cgroup", cgroup_patterns_, [context](const std::string& cgroupStr) {
          return PluginArgParser::parseCgroup(context, cgroupStr);
        });

    argParser_.addArgument("xattr", xattr_);

    if (!argParser_.parse(args)) {
      return 1;
    }

    return 0;
  }

  virtual std::unique_ptr<PrekillHookInvocation> fire(
      const CgroupContext& cgroup_ctx,
      const ActionContext& action_ctx) = 0;

  virtual bool canRunOnCgroup(const CgroupContext& cgroup_ctx) {
    if (auto hasXattr = Fs::hasxattrAt(cgroup_ctx.fd(), xattr_);
        hasXattr && *hasXattr) {
      return true;
    }
    for (const auto& pattern : cgroup_patterns_) {
      if (cgroup_ctx.cgroup().hasDescendantWithPrefixMatching(pattern)) {
        return true;
      }
    }

    return false;
  }

  virtual void setName(const std::string& name) {
    name_ = name;
  }
  virtual const std::string& getName() const {
    return name_;
  }

 protected:
  PluginArgParser argParser_;

 private:
  std::string name_;
  std::unordered_set<CgroupPath> cgroup_patterns_{};
  std::string xattr_;
};

} // namespace Engine
} // namespace Oomd
