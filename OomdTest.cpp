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

#include <gtest/gtest.h>

#include <string>
#include <unordered_set>

#include "oomd/Oomd.h"

using namespace Oomd;
using namespace testing;

constexpr auto kCgroupDataDir = "oomd/fixtures/cgroup";

class OomdTest : public ::testing::Test {
 public:
  OomdTest() {
    oomd = std::make_unique<::Oomd::Oomd>(nullptr, 5);
  }

  std::string cgroup_path{kCgroupDataDir};
  OomdContext ctx;
  std::unique_ptr<::Oomd::Oomd> oomd{nullptr};
};

TEST_F(OomdTest, OomdContextUpdate) {
  EXPECT_EQ(ctx.cgroups().size(), 0);

  std::unordered_set<std::string> parent_cgroups;
  parent_cgroups.emplace("system.slice");
  oomd->updateContext(cgroup_path, parent_cgroups, ctx);

  EXPECT_EQ(ctx.cgroups().size(), 5);

  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/service1.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/service2.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/service3.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/service4.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/slice1.slice"));
}

TEST_F(OomdTest, OomdContextMultipleUpdates) {
  std::unordered_set<std::string> parent_cgroups;
  parent_cgroups.emplace("system.slice");
  oomd->updateContext(cgroup_path, parent_cgroups, ctx);

  for (int i = 0; i < 3; i++) {
    int64_t average =
        ctx.getCgroupContext("system.slice/service1.service").average_usage;
    oomd->updateContext(cgroup_path, parent_cgroups, ctx);

    // We expect the avg usage to slowly converge from 0 -> true avg
    // b/c of AVERAGE_SIZE_DECAY
    EXPECT_GT(
        ctx.getCgroupContext("system.slice/service1.service").average_usage,
        average);
  }
}

TEST_F(OomdTest, OomdContextUpdateMultiCgroup) {
  EXPECT_EQ(ctx.cgroups().size(), 0);

  std::unordered_set<std::string> parent_cgroups;
  parent_cgroups.emplace("system.slice");
  parent_cgroups.emplace("workload.slice");
  oomd->updateContext(cgroup_path, parent_cgroups, ctx);

  EXPECT_EQ(ctx.cgroups().size(), 6);

  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/service1.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/service2.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/service3.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/service4.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("system.slice/slice1.slice"));
  EXPECT_TRUE(ctx.hasCgroupContext("workload.slice/service1.service"));
}
