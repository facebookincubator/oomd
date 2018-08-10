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

#include "oomd/shared/OomdContext.h"
#include "oomd/Log.h"

#include <exception>

namespace Oomd {

OomdContext& OomdContext::operator=(OomdContext&& other) {
  memory_state_ = std::move(other.memory_state_);
  oom_context_ = other.oom_context_;
  return *this;
}

OomdContext::OomdContext(OomdContext&& other) noexcept {
  memory_state_ = std::move(other.memory_state_);
  oom_context_ = other.oom_context_;
}

bool OomdContext::hasCgroupContext(const std::string& name) const {
  return memory_state_.find(name) != memory_state_.end();
}

std::vector<std::string> OomdContext::cgroups() const {
  std::vector<std::string> keys;

  for (const auto& pair : memory_state_) {
    keys.emplace_back(pair.first);
  }

  return keys;
}

const CgroupContext& OomdContext::getCgroupContext(const std::string& name) {
  if (!hasCgroupContext(name)) {
    throw std::invalid_argument("Cgroup not present");
  }

  return memory_state_[name];
}

void OomdContext::setCgroupContext(
    const std::string& name,
    CgroupContext context) {
  memory_state_[name] = context;
}

std::vector<std::pair<std::string, CgroupContext>> OomdContext::reverseSort(
    std::function<double(const CgroupContext& cc)> getKey) {
  std::vector<std::pair<std::string, CgroupContext>> vec;

  for (const auto& pair : memory_state_) {
    vec.emplace_back(
        std::pair<std::string, CgroupContext>{pair.first, pair.second});
  }

  if (getKey) {
    reverseSort(vec, getKey);
  }

  return vec;
}

void OomdContext::reverseSort(
    std::vector<std::pair<std::string, CgroupContext>>& vec,
    std::function<double(const CgroupContext& cc)> getKey) {
  std::sort(
      vec.begin(),
      vec.end(),
      [getKey](
          std::pair<std::string, CgroupContext>& first,
          std::pair<std::string, CgroupContext>& second) {
        // Want to sort in reverse order (largest first), so return
        // true if first element is ordered before second element
        return getKey(first.second) > getKey(second.second);
      });
}

const OomContext& OomdContext::getOomContext() const {
  return oom_context_;
}

void OomdContext::setOomContext(OomContext context) {
  oom_context_ = context;
}

void OomdContext::dump() {
  dumpOomdContext(reverseSort());
}

void OomdContext::dumpOomdContext(
    const std::vector<std::pair<std::string, CgroupContext>>& vec) {
  OLOG << "Dumping OomdContext: ";
  for (const auto& ms : vec) {
    OLOG << "name=" << ms.first;
    OLOG << "\tpressure=" << ms.second.pressure.sec_10 << " "
         << ms.second.pressure.sec_60 << " " << ms.second.pressure.sec_600;
    OLOG << "\tcurrent_usage=" << ms.second.current_usage / 1024 / 1024
         << " MB";
    OLOG << "\taverage_usage=" << ms.second.average_usage / 1024 / 1024
         << " MB";
    OLOG << "\tmemory_low=" << ms.second.memory_low / 1024 / 1024 << " MB";
  }
}

} // namespace Oomd
