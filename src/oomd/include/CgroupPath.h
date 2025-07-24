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
  CgroupPath(const CgroupPath& other) = default;
  CgroupPath(CgroupPath&& other) = default;
  CgroupPath& operator=(const CgroupPath& other) = default;
  CgroupPath& operator=(CgroupPath&& other) = default;

  const std::string& absolutePath() const;
  // cgroup path without the cgroup fs
  const std::string& relativePath() const;
  const std::vector<std::string>& relativePathParts() const;
  const std::string& cgroupFs() const;
  // CgroupPath taking parent as root
  const CgroupPath relativeTo(const std::string& parent) const;

  CgroupPath getParent() const;
  CgroupPath getChild(const std::string& path) const;
  /*
   * Resolve the glob pattern in this CgroupPath to a vector of CgroupPaths that
   * exist, share the same cgroup fs, and are directories.
   */
  std::vector<CgroupPath> resolveWildcard() const;

  // hasDescendantWithPrefixMatching is true if any descendant of relativePath()
  // has a prefix that matches @param pattern.
  // @param pattern is a glob-like pattern, supporting only wildcard path
  // components. /foo/*/baz/ matches relativePath() /foo/bar/baz, but the
  // pattern /foo/b*/baz only matches the exact path "/foo/b*/baz". Leading and
  // trailing slashes are ignored.
  // @returns true if
  //  1. relativePath() matches a prefix of pattern, in which case there may
  //     exist descendants matching the full pattern,
  //  2. if pattern matches a prefix of relativePath(), in which case
  //     relativePath() is a descendant of a path matching pattern, or
  //  3. if pattern matches relativePath() itself.
  bool hasDescendantWithPrefixMatching(const CgroupPath& pattern) const;

  bool operator==(const CgroupPath& other) const;
  bool operator!=(const CgroupPath& other) const;
  // Do we represent the root cgroup?
  bool isRoot() const;

 private:
  void recomputeReadCache();

  std::string cgroup_fs_;
  std::vector<std::string> cgroup_path_;
  std::string absolute_cache_;
  std::string relative_cache_;
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
