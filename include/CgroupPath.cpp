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

namespace Oomd {

CgroupPath::CgroupPath(
    const std::string& cgroup_fs,
    const std::string& cgroup_path)
    : cgroup_fs_(cgroup_fs) {
  // Strip trailing '/'
  if (cgroup_fs_.at(cgroup_fs_.size() - 1) == '/' && cgroup_fs.size() > 1) {
    cgroup_fs_.pop_back();
  }

  cgroup_path_ = Fs::split(cgroup_path, '/');
}

CgroupPath::CgroupPath(const CgroupPath& other) {
  copyFrom(other);
}

CgroupPath::CgroupPath(CgroupPath&& other) {
  moveFrom(std::move(other));
}

CgroupPath& CgroupPath::operator=(const CgroupPath& other) {
  copyFrom(other);
  return *this;
}

CgroupPath& CgroupPath::operator=(CgroupPath&& other) {
  moveFrom(std::move(other));
  return *this;
}

std::string CgroupPath::absolutePath() const {
  std::string relpath = relativePath();
  if (!relpath.size()) {
    return cgroup_fs_;
  }

  std::string ret;
  ret.reserve(cgroup_fs_.size() + 1 + relpath.size());
  ret += cgroup_fs_;
  ret += '/';
  ret += std::move(relpath);

  return ret;
}

std::string CgroupPath::relativePath() const {
  size_t s = 0;
  for (size_t i = 0; i < cgroup_path_.size(); ++i) {
    s += cgroup_path_.at(i).size();
    if (i == cgroup_path_.size() - 1) {
      break;
    }
    ++s; // for the path separator
  }

  std::string ret;
  ret.reserve(s);

  for (size_t i = 0; i < cgroup_path_.size(); ++i) {
    ret += cgroup_path_.at(i);
    if (i == cgroup_path_.size() - 1) {
      break;
    }
    ret += '/';
  }

  return ret;
}

std::string CgroupPath::name() const {
  if (!cgroup_path_.size()) {
    return {};
  }

  return cgroup_path_.at(cgroup_path_.size() - 1);
}

std::string CgroupPath::cgroupFs() const {
  return cgroup_fs_;
}

void CgroupPath::ascend() {
  if (!isRoot()) {
    cgroup_path_.pop_back();
  }
}

void CgroupPath::descend(const std::string& path) {
  auto pieces = Fs::split(path, '/');
  cgroup_path_.reserve(cgroup_path_.size() + pieces.size());
  for (auto& piece : pieces) {
    cgroup_path_.emplace_back(std::move(piece));
  }
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

void CgroupPath::copyFrom(const CgroupPath& other) {
  this->cgroup_fs_ = other.cgroup_fs_;
  this->cgroup_path_ = other.cgroup_path_;
}

void CgroupPath::moveFrom(CgroupPath&& other) {
  this->cgroup_fs_ = std::move(other.cgroup_fs_);
  this->cgroup_path_ = std::move(other.cgroup_path_);
}

} // namespace Oomd
