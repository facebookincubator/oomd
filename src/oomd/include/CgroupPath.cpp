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

#include "oomd/include/CgroupPath.h"

#include <exception>

#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace Oomd {

CgroupPath::CgroupPath(
    const std::string& cgroup_fs,
    const std::string& cgroup_path)
    : cgroup_fs_(cgroup_fs) {
  // Strip trailing '/'
  if (cgroup_fs_.at(cgroup_fs_.size() - 1) == '/' && cgroup_fs.size() > 1) {
    cgroup_fs_.pop_back();
  }

  cgroup_path_ = Util::split(cgroup_path, '/');
  recomputeReadCache();
}

const std::string& CgroupPath::absolutePath() const {
  return absolute_cache_;
}

const std::string& CgroupPath::relativePath() const {
  return relative_cache_;
}

const std::vector<std::string>& CgroupPath::relativePathParts() const {
  return cgroup_path_;
}

const std::string& CgroupPath::cgroupFs() const {
  return cgroup_fs_;
}

CgroupPath CgroupPath::getParent() const {
  if (this->isRoot()) {
    throw std::invalid_argument("Cannot get parent of root");
  }

  CgroupPath parent(*this);
  parent.cgroup_path_.pop_back();
  parent.recomputeReadCache();

  return parent;
}

CgroupPath CgroupPath::getChild(const std::string& path) const {
  CgroupPath child(*this);

  auto pieces = Util::split(path, '/');
  child.cgroup_path_.reserve(child.cgroup_path_.size() + pieces.size());
  for (auto& piece : pieces) {
    child.cgroup_path_.emplace_back(std::move(piece));
  }
  child.recomputeReadCache();

  return child;
}

bool CgroupPath::operator==(const CgroupPath& other) const {
  return this->absolutePath() == other.absolutePath();
}

bool CgroupPath::operator!=(const CgroupPath& other) const {
  return !operator==(other);
}

bool CgroupPath::isRoot() const {
  return cgroup_path_.size() == 0;
}

void CgroupPath::recomputeReadCache() {
  // Recreate relative_cache_
  relative_cache_.clear();
  size_t s = 0;
  for (size_t i = 0; i < cgroup_path_.size(); ++i) {
    s += cgroup_path_.at(i).size();
    if (i == cgroup_path_.size() - 1) {
      break;
    }
    ++s; // for the path separator
  }

  relative_cache_.reserve(s);

  for (size_t i = 0; i < cgroup_path_.size(); ++i) {
    relative_cache_ += cgroup_path_.at(i);
    if (i == cgroup_path_.size() - 1) {
      break;
    }
    relative_cache_ += '/';
  }

  // Recreate absolute_cache_
  absolute_cache_.clear();
  absolute_cache_.reserve(cgroup_fs_.size() + 1 + relative_cache_.size());
  absolute_cache_ += cgroup_fs_;
  if (relative_cache_.size()) {
    absolute_cache_ += '/';
    absolute_cache_ += relative_cache_;
  }
}

} // namespace Oomd
