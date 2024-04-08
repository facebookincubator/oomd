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

#include <string>
#include <unordered_map>
#include <unordered_set>

#include "oomd/OomdContext.h"
#include "oomd/PluginConstructionContext.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/Types.h"
#include "oomd/util/PluginArgParser.h"

namespace Oomd {
namespace Engine {

enum class PluginRet {
  CONTINUE = 0,
  STOP,
  ASYNC_PAUSED,
};

class BasePlugin {
 public:
  /*
   * Config arguments are passed via @param args. If some required args are
   * missing, the plugin can return a non-zero value and oomd will abort
   * initialization.
   *
   *
   * @return 0 on successful initialization.
   */
  virtual int init(
      const PluginArgs& args,
      const PluginConstructionContext& context) = 0;

  /*
   * This is always run at the beginning of the event loop before run() has
   * been called on any plugin.
   *
   * This is the ideal place to generate and store state inside object instance
   * because it is guaranteed to be called each interval, but run() may not.
   * Therefore, this function should be lightweight, i.e. no sleep() inside.
   */
  virtual void prerun(OomdContext& context) {}

  /*
   * This is the main work method every plugin will implement.
   *
   * If part of a detector chain, a PluginRet::STOP will terminate and fail
   * the detector chain. If the entire detector chain returns
   * PluginRet::CONTINUE, the detector chain will succeed.
   *
   * If part of an action chain, a PluginRet::STOP will terminate further
   * execution of the action chain. PluginRet::Continue continues execution.
   *
   * Plugins may store state between invocations inside the object instance.
   *
   * @return @class PluginRet
   */
  virtual PluginRet run(OomdContext& context) = 0;

  virtual void setName(const std::string& name) {
    name_ = name;
    argParser_.setName(name_);
  }
  virtual const std::string& getName() const {
    return name_;
  }

  virtual ~BasePlugin() = default;

 protected:
  PluginArgParser argParser_;

 private:
  std::string name_;
};

} // namespace Engine
} // namespace Oomd
