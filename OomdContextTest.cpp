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

#include "oomd/OomdContext.h"

using namespace Oomd;
using namespace testing;

class OomdContextTest : public ::testing::Test {
 public:
  OomdContextTest() = default;
  OomdContext ctx;
};

TEST_F(OomdContextTest, MoveConstructor) {
  CgroupPath path("/sys/fs/cgroup", "asdf");
  EXPECT_FALSE(ctx.hasCgroupContext(path));

  OomdContext other;
  other.setCgroupContext(path, CgroupContext{{}, {}, 1, 2, 3});

  ctx = std::move(other);

  ASSERT_TRUE(ctx.hasCgroupContext(path));
  auto moved_cgroup_ctx = ctx.getCgroupContext(path);
  EXPECT_EQ(moved_cgroup_ctx.current_usage, 1);
  EXPECT_EQ(moved_cgroup_ctx.average_usage, 2);
  EXPECT_EQ(moved_cgroup_ctx.memory_low, 3);
}

TEST_F(OomdContextTest, HasCgroupCheck) {
  CgroupPath path("/sys/fs/cgroup", "asdf");

  EXPECT_FALSE(ctx.hasCgroupContext(path));
  ctx.setCgroupContext(path, CgroupContext{});
  EXPECT_TRUE(ctx.hasCgroupContext(path));
}

TEST_F(OomdContextTest, CgroupKeys) {
  CgroupPath p1("/sys/fs/cgroup", "asdf");
  CgroupPath p2("/sys/fs/cgroup", "wow");

  EXPECT_EQ(ctx.cgroups().size(), 0);
  ctx.setCgroupContext(p1, CgroupContext{});
  ctx.setCgroupContext(p2, CgroupContext{});
  EXPECT_EQ(ctx.cgroups().size(), 2);
  EXPECT_THAT(ctx.cgroups(), Contains(p1));
  EXPECT_THAT(ctx.cgroups(), Contains(p2));
}

TEST_F(OomdContextTest, SetCgroup) {
  CgroupPath p1("/sys/fs/cgroup", "asdf");
  ctx.setCgroupContext(p1, CgroupContext{});
  EXPECT_EQ(ctx.getCgroupContext(p1).memory_low, 0);

  ctx.setCgroupContext(p1, CgroupContext{{}, {}, 0, 0, 222});
  EXPECT_EQ(ctx.getCgroupContext(p1).memory_low, 222);
}

TEST_F(OomdContextTest, SortContext) {
  CgroupPath p1("/sys/fs/cgroup", "biggest");
  CgroupPath p2("/sys/fs/cgroup", "smallest");
  CgroupPath p3("/sys/fs/cgroup", "asdf");
  CgroupPath p4("/sys/fs/cgroup", "fdsa");

  ctx.setCgroupContext(p1, {{}, {}, 99999999, 0, 1});
  ctx.setCgroupContext(p2, {{}, {}, 1, 988888888888, 4});
  ctx.setCgroupContext(p3, {{}, {}, 88888888, 289349817823, 2});
  ctx.setCgroupContext(p4, {{}, {}, 77777777, 6, 3});

  auto sorted = ctx.reverseSort(
      [](const CgroupContext& cgroup_ctx) { return cgroup_ctx.current_usage; });

  ASSERT_EQ(sorted.size(), 4);
  EXPECT_EQ(sorted.at(0).first, p1);
  EXPECT_EQ(sorted.at(3).first, p2);

  OomdContext::reverseSort(sorted, [](const CgroupContext& cgroup_ctx) {
    return cgroup_ctx.memory_low;
  });

  ASSERT_EQ(sorted.size(), 4);
  EXPECT_EQ(sorted.at(0).first, p2);
  EXPECT_EQ(sorted.at(3).first, p1);
}
