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
#include "oomd/include/Types.h"

namespace Oomd {
namespace Engine {

enum class PluginRet {
  CONTINUE = 0,
  STOP,
};

using MonitoredResources = std::unordered_set<std::string>;

class BasePlugin {
 public:
  /*
   * Plugins can register cgroups they want monitored by adding paths to
   * @param resources.
   *
   * Config arguments are passed via @param args. If some required args are
   * missing, the plugin can return a non-zero value and oomd will abort
   * initialization.
   *
   *
   * @return 0 on successful initialization.
   */
  virtual int init(
      MonitoredResources& resources,
      std::unordered_map<std::string, std::string> args) = 0;

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
  }
  virtual const std::string& getName() const {
    return name_;
  }

  virtual ~BasePlugin() = default;

 private:
  std::string name_;
};

} // namespace Engine
} // namespace Oomd
