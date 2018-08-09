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

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "oomd/shared/OomdContext.h"

using namespace Oomd;
using namespace testing;

class OomdContextTest : public ::testing::Test {
 public:
  OomdContextTest() = default;
  OomdContext ctx;
};

TEST_F(OomdContextTest, MoveConstructor) {
  EXPECT_FALSE(ctx.hasCgroupContext("asdf"));

  OomdContext other;
  other.setCgroupContext("asdf", CgroupContext{{}, {}, 1, 2, 3});

  ctx = std::move(other);

  ASSERT_TRUE(ctx.hasCgroupContext("asdf"));
  auto moved_cgroup_ctx = ctx.getCgroupContext("asdf");
  EXPECT_EQ(moved_cgroup_ctx.current_usage, 1);
  EXPECT_EQ(moved_cgroup_ctx.average_usage, 2);
  EXPECT_EQ(moved_cgroup_ctx.memory_low, 3);
}

TEST_F(OomdContextTest, HasCgroupCheck) {
  EXPECT_FALSE(ctx.hasCgroupContext("asdf"));
  ctx.setCgroupContext("asdf", CgroupContext{});
  EXPECT_TRUE(ctx.hasCgroupContext("asdf"));
}

TEST_F(OomdContextTest, CgroupKeys) {
  EXPECT_EQ(ctx.cgroups().size(), 0);
  ctx.setCgroupContext("asdf", CgroupContext{});
  ctx.setCgroupContext("wow", CgroupContext{});
  EXPECT_EQ(ctx.cgroups().size(), 2);
  EXPECT_THAT(ctx.cgroups(), Contains("asdf"));
  EXPECT_THAT(ctx.cgroups(), Contains("wow"));
}

TEST_F(OomdContextTest, SetCgroup) {
  ctx.setCgroupContext("asdf", CgroupContext{});
  EXPECT_EQ(ctx.getCgroupContext("asdf").memory_low, 0);

  ctx.setCgroupContext("asdf", CgroupContext{{}, {}, 0, 0, 222});
  EXPECT_EQ(ctx.getCgroupContext("asdf").memory_low, 222);
}

TEST_F(OomdContextTest, SortContext) {
  ctx.setCgroupContext("biggest", {{}, {}, 99999999, 0, 1});
  ctx.setCgroupContext("smallest", {{}, {}, 1, 988888888888, 4});
  ctx.setCgroupContext("asdf", {{}, {}, 88888888, 289349817823, 2});
  ctx.setCgroupContext("fdsa", {{}, {}, 77777777, 6, 3});

  auto sorted = ctx.reverseSort(
      [](const CgroupContext& cgroup_ctx) { return cgroup_ctx.current_usage; });

  ASSERT_EQ(sorted.size(), 4);
  EXPECT_EQ(sorted.at(0).first, "biggest");
  EXPECT_EQ(sorted.at(3).first, "smallest");

  OomdContext::reverseSort(sorted, [](const CgroupContext& cgroup_ctx) {
    return cgroup_ctx.memory_low;
  });

  ASSERT_EQ(sorted.size(), 4);
  EXPECT_EQ(sorted.at(0).first, "smallest");
  EXPECT_EQ(sorted.at(3).first, "biggest");
}

TEST_F(OomdContextTest, OomContext) {
  EXPECT_EQ(ctx.getOomContext().type, OomType::NONE);

  OomStat s;
  s.pressure_10_duration = 22;
  OomContext c{OomType::PRESSURE_10, s};
  ctx.setOomContext(c);

  EXPECT_EQ(ctx.getOomContext().type, OomType::PRESSURE_10);
  EXPECT_EQ(ctx.getOomContext().stat.pressure_10_duration, 22);
}
