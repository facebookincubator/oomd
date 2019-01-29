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

#include <algorithm>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "oomd/include/Types.h"

namespace Oomd {

class OomdContext {
 public:
  OomdContext() {} // = default warns about dynamic exceptions
  ~OomdContext() = default;
  OomdContext(OomdContext&& other) noexcept;
  OomdContext& operator=(OomdContext&& other);

  /**
   * @returns whether or not OomdContext holds a particular cgroup
   */
  bool hasCgroupContext(const std::string& name) const;

  /**
   * @returns all the stored cgroup names (eg "chef.service")
   */
  std::vector<std::string> cgroups() const;

  /*
   * @returns a CgroupContext reference associated with @param name
   * @throws std::invalid_argument for missing cgroup
   */
  const CgroupContext& getCgroupContext(const std::string& name);

  /**
   * Assigns a mapping of cgroup -> CgroupContext
   */
  void setCgroupContext(const std::string& name, CgroupContext context);

  /**
   * Manipulates CgroupContexts into helpful other helpful datastructures
   *
   * @param getKey is a lambda that accesses the key you want to reverse sort by
   */
  std::vector<std::pair<std::string, CgroupContext>> reverseSort(
      std::function<double(const CgroupContext& cgroup_ctx)> getKey = nullptr);

  /**
   * In place sorts @param vec. Similar to @method
   * reverseSort(std::function<...>)
   */
  static void reverseSort(
      std::vector<std::pair<std::string, CgroupContext>>& vec,
      std::function<double(const CgroupContext& cgroup_ctx)> getKey);

  /**
   * Dumps OomdContext state to stderr
   */
  void dump();
  static void dumpOomdContext(
      const std::vector<std::pair<std::string, CgroupContext>>& vec);

  /**
   * Used to let action plugins know which ruleset and detector group
   * triggered it
   */
  const ActionContext& getActionContext() const;
  void setActionContext(ActionContext context);

 private:
  std::unordered_map<std::string, CgroupContext> memory_state_;
  ActionContext action_context_;
};

} // namespace Oomd
