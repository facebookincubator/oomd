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

#include <functional>
#include <string>
#include <unordered_map>

#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/PrekillHook.h"

namespace Oomd {

template <typename T>
class PluginRegistry {
 public:
  using FactoryFunction = std::function<T*()>;
  using FactoryMap = std::unordered_map<std::string, FactoryFunction>;

  bool add(const std::string& name, FactoryFunction fac) {
    if (map_.find(name) != map_.end()) {
      return false;
    }

    map_[name] = fac;
    return true;
  }

  T* create(const std::string& name) {
    if (map_.find(name) == map_.end()) {
      return nullptr;
    }

    return map_[name]();
  }

  std::vector<std::string> getRegistered() const {
    std::vector<std::string> list_plugins;
    for (auto const& data : map_) {
      list_plugins.push_back(data.first);
    }
    return list_plugins;
  }

 private:
  FactoryMap map_;
};

PluginRegistry<Engine::BasePlugin>& getPluginRegistry();

#define REGISTER_PLUGIN(plugin_name, create_func) \
  bool plugin_name##_plugin_entry =               \
      getPluginRegistry().add(#plugin_name, (create_func))

PluginRegistry<Engine::PrekillHook>& getPrekillHookRegistry();

#define REGISTER_PREKILL_HOOK(hook_name, create_func) \
  bool hook_name##_plugin_entry =                     \
      getPrekillHookRegistry().add(#hook_name, (create_func))

} // namespace Oomd
