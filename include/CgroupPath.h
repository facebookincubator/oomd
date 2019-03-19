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
#include <vector>

namespace Oomd {

class CgroupPath {
 public:
  CgroupPath(const std::string& cgroup_fs, const std::string& cgroup_path);
  ~CgroupPath() = default;
  CgroupPath(const CgroupPath& other);
  CgroupPath(CgroupPath&& other);
  CgroupPath& operator=(const CgroupPath& other);
  CgroupPath& operator=(CgroupPath&& other);

  std::string absolutePath() const;
  // cgroup path without the cgroup fs
  std::string relativePath() const;
  // leaf of the path
  std::string name() const;
  std::string cgroupFs() const;

  /*
   * Ascend or descend up the cgroup filesystem
   */
  void ascend();
  void descend(const std::string& path);

  bool operator==(const CgroupPath& other) const;
  bool operator!=(const CgroupPath& other) const;
  // Do we represent the root cgroup?
  bool isRoot() const;

 private:
  void copyFrom(const CgroupPath& other);
  void moveFrom(CgroupPath&& other);

  std::string cgroup_fs_;
  std::vector<std::string> cgroup_path_;
};

} // namespace Oomd

namespace std {
template <>
struct hash<Oomd::CgroupPath> {
  size_t operator()(const Oomd::CgroupPath& path) const {
    return hash<std::string>()(path.absolutePath());
  }
};
} // namespace std
