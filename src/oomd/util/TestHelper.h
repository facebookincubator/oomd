/*
 * Copyright (C) 2019-present, Facebook, Inc.
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

#include "oomd/CgroupContext.h"
#include "oomd/NewOomdContext.h"

namespace Oomd {

/*
 * Friend of data classes to access their private fields for test injection.
 * This class must only be included in tests and not the main binary.
 */
class TestHelper {
 public:
  using CgroupData = NewCgroupContext::CgroupData;

  static CgroupData& getDataRef(const NewCgroupContext& cgroup_ctx) {
    return *cgroup_ctx.data_;
  }

  static std::unordered_map<CgroupPath, NewCgroupContext>& getCgroupsRef(
      OomdContext& ctx) {
    return ctx.cgroups_;
  }

  /*
   * Set the cgroup data of a CgroupContext in OomdContext.
   * This is a shortcut for setting up CgroupContext without creating control
   * file fixtures. However, retrieving CgroupContext from OomdContext via
   * addToCacheAndGet still requires the requested CgroupPath exists, which
   * could be done using the Fixture utils.
   */
  static void setCgroupData(
      OomdContext& ctx,
      const CgroupPath& cgroup,
      const CgroupData& data) {
    *ctx.cgroups_.try_emplace(cgroup, ctx, cgroup).first->second.data_ = data;
  }
};

} // namespace Oomd
