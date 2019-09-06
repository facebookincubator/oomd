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

TEST_F(OomdContextTest, CgroupKeyRoot) {
  CgroupPath root("/sys/fs/cgroup", "/");
  ctx.setCgroupContext(root, CgroupContext{.current_usage = 12345});
  EXPECT_EQ(ctx.cgroups().size(), 1);
  EXPECT_EQ(ctx.getCgroupContext(root).current_usage, 12345);
}

TEST_F(OomdContextTest, SetCgroup) {
  CgroupPath p1("/sys/fs/cgroup", "asdf");
  ctx.setCgroupContext(p1, CgroupContext{});
  EXPECT_EQ(ctx.getCgroupContext(p1).memory_low, 0);

  ctx.setCgroupContext(p1, CgroupContext{{}, {}, 0, 0, 222});
  EXPECT_EQ(ctx.getCgroupContext(p1).memory_low, 222);

  CgroupPath p2("/sys/fs/cgroup", "A/B/C");
  ctx.setCgroupContext(p2, CgroupContext{});
  CgroupPath p2_parent = p2.getParent();
  CgroupPath p2_parent_parent = p2_parent.getParent();
  EXPECT_TRUE(ctx.hasCgroupContext(p2));
  EXPECT_FALSE(ctx.hasCgroupContext(p2_parent));
  EXPECT_FALSE(ctx.hasCgroupContext(p2_parent_parent));
}

TEST_F(OomdContextTest, DeepNesting) {
  CgroupPath p1("/sys/fs/cgroup", "A");
  CgroupPath p2("/sys/fs/cgroup", "A/B");
  CgroupPath p3("/sys/fs/cgroup", "A/B/C");
  CgroupPath p4("/sys/fs/cgroup", "A/B/D");
  CgroupPath p5("/sys/fs/cgroup", "A/B/D/E");

  ctx.setCgroupContext(p1, CgroupContext{{}, {}, 1});
  ctx.setCgroupContext(p2, CgroupContext{{}, {}, 2});
  ctx.setCgroupContext(p3, CgroupContext{{}, {}, 3});
  ctx.setCgroupContext(p4, CgroupContext{{}, {}, 4});
  ctx.setCgroupContext(p5, CgroupContext{{}, {}, 5});

  EXPECT_TRUE(ctx.hasCgroupContext(p1));
  EXPECT_TRUE(ctx.hasCgroupContext(p2));
  EXPECT_TRUE(ctx.hasCgroupContext(p3));
  EXPECT_TRUE(ctx.hasCgroupContext(p4));
  EXPECT_TRUE(ctx.hasCgroupContext(p5));

  EXPECT_EQ(ctx.getCgroupContext(p1).current_usage, 1);
  EXPECT_EQ(ctx.getCgroupContext(p2).current_usage, 2);
  EXPECT_EQ(ctx.getCgroupContext(p3).current_usage, 3);
  EXPECT_EQ(ctx.getCgroupContext(p4).current_usage, 4);
  EXPECT_EQ(ctx.getCgroupContext(p5).current_usage, 5);
}

TEST_F(OomdContextTest, TreeWalk) {
  CgroupPath p1("/sys/fs/cgroup", "A");
  CgroupPath p2("/sys/fs/cgroup", "A/B");
  CgroupPath p3("/sys/fs/cgroup", "A/B/C");
  CgroupPath p4("/sys/fs/cgroup", "A/B/C/D");

  ctx.setCgroupContext(p1, CgroupContext{});
  ctx.setCgroupContext(p2, CgroupContext{});
  ctx.setCgroupContext(p3, CgroupContext{});
  ctx.setCgroupContext(p4, CgroupContext{});

  auto pp4 = ctx.getCgroupNode(p4);
  ASSERT_TRUE(pp4->parent.lock());
  EXPECT_EQ(pp4->parent.lock()->path, p3);
  ASSERT_TRUE(pp4->parent.lock()->parent.lock());
  EXPECT_EQ(pp4->parent.lock()->parent.lock()->path, p2);
  ASSERT_TRUE(pp4->parent.lock()->parent.lock()->parent.lock());
  EXPECT_EQ(pp4->parent.lock()->parent.lock()->parent.lock()->path, p1);
  ASSERT_TRUE(pp4->parent.lock()->parent.lock()->parent.lock()->parent.lock());
  EXPECT_TRUE(pp4->parent.lock()
                  ->parent.lock()
                  ->parent.lock()
                  ->parent.lock()
                  ->path.isRoot());
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
