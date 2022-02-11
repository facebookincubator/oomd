/*
 * Copyright (C) 2021-present, Meta Platforms, Inc. and affiliates
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
#include <unordered_set>

#include "oomd/Log.h"
#include "oomd/PluginConstructionContext.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/Types.h"
#include "oomd/util/SystemMaybe.h"
#include "oomd/util/Util.h"

namespace Oomd {

namespace {
// this is to help deduce function template.
// this is the same as std::type_identity in c++20
template <typename T>
struct Identity {
  using type = T;
};
} // namespace

class PluginArgParser {
 public:
  static std::unordered_set<CgroupPath> parseCgroup(
      const PluginConstructionContext& context,
      const std::string& cgroupStr);

  static int parseUnsignedInt(const std::string& intStr);

  PluginArgParser() {}
  explicit PluginArgParser(const std::string& pluginName)
      : pluginName_(pluginName) {}

  void setName(const std::string& pluginName);
  std::string getName();

  // pasrse arg input, it fails (returns a error SystemMaybe) when:
  // 1. any required arg is missing
  // 2. any unknown arg passed in
  // 3. failed to parse value string of any arg
  SystemMaybe<Unit> parse(const Engine::PluginArgs& args);

  // return a set of valid arg names registered
  std::unordered_set<std::string> validArgNames();

  template <typename T>
  void
  addArgument(const std::string& argName, T& argDest, bool required = false) {
    addArgumentCustom(argName, argDest, parseValue<T>, required);
  }

  template <typename T>
  void addArgumentCustom(
      const std::string& argName,
      T& argDest,
      typename Identity<const std::function<T(const std::string&)>>::type& func,
      bool required = false) {
    if (required) {
      requiredArgs_.emplace(argName);
    }

    argValueFillingFuncs_.emplace(
        argName,
        [&argDest, argName, func](
            const std::string& valueStr) -> SystemMaybe<Unit> {
          try {
            argDest = func(valueStr);
          } catch (std::exception& e) {
            return SYSTEM_ERROR(
                EINVAL,
                "Failed to parse argument \"",
                argName,
                "\", error: ",
                e.what());
          }

          return noSystemError();
        });
  }

  template <typename T>
  static T parseValue(const std::string& valueStr);

 private:
  std::unordered_map<
      std::string,
      std::function<SystemMaybe<Unit>(const std::string&)>>
      argValueFillingFuncs_;
  std::unordered_set<std::string> requiredArgs_;
  std::string pluginName_;
};

} // namespace Oomd
