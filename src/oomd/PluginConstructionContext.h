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

namespace Oomd {

class PluginConstructionContext {
 public:
  PluginConstructionContext(const std::string& cgroup_fs);
  ~PluginConstructionContext() = default;
  PluginConstructionContext(const PluginConstructionContext& other) = default;
  PluginConstructionContext(PluginConstructionContext&& other) = default;
  PluginConstructionContext& operator=(const PluginConstructionContext& other) =
      default;
  PluginConstructionContext& operator=(PluginConstructionContext&& other) =
      default;

  const std::string& cgroupFs() const;

 private:
  std::string cgroup_fs_;
};

} // namespace Oomd
