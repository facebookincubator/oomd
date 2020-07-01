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
#include "oomd/util/Fixture.h"
#include "oomd/util/TestHelper.h"

using namespace Oomd;
using namespace testing;

class OomdContextTest : public ::testing::Test {
 public:
  OomdContextTest() = default;
  OomdContext ctx;

 protected:
  using F = Fixture;
  void SetUp() override {
    tempdir_ = F::mkdtempChecked();
  }
  void TearDown() override {
    F::rmrChecked(tempdir_);
  }
  std::string tempdir_;
};

namespace std {
// Check if two reference_wrappers are exactly the same
bool operator==(
    const OomdContext::ConstCgroupContextRef& lhs,
    const OomdContext::ConstCgroupContextRef& rhs) {
  return std::addressof(lhs.get()) == std::addressof(rhs.get());
};
} // namespace std

TEST_F(OomdContextTest, MoveConstructor) {
  CgroupPath path(tempdir_, "asdf");
  EXPECT_FALSE(ctx.addToCacheAndGet(path));

  F::materialize(F::makeDir(tempdir_, {F::makeDir("asdf")}));

  OomdContext other;
  TestHelper::setCgroupData(
      other,
      path,
      TestHelper::CgroupData{
          .current_usage = 1, .memory_low = 2, .average_usage = 3});

  ctx = std::move(other);

  ASSERT_THAT(ctx.cgroups(), ElementsAre(path));
  auto moved_cgroup_ctx = ctx.addToCacheAndGet(path);
  ASSERT_TRUE(moved_cgroup_ctx);
  EXPECT_EQ(moved_cgroup_ctx->get().current_usage(), 1);
  EXPECT_EQ(moved_cgroup_ctx->get().memory_low(), 2);
  EXPECT_EQ(moved_cgroup_ctx->get().average_usage(), 3);
}

TEST_F(OomdContextTest, CgroupKeys) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("asdf"), F::makeDir("wow")}));
  CgroupPath p1(tempdir_, "asdf");
  CgroupPath p2(tempdir_, "wow");

  EXPECT_EQ(ctx.cgroups().size(), 0);
  EXPECT_TRUE(ctx.addToCacheAndGet(p1));
  EXPECT_TRUE(ctx.addToCacheAndGet(p2));
  EXPECT_THAT(ctx.cgroups(), UnorderedElementsAre(p1, p2));
}

TEST_F(OomdContextTest, CgroupKeyRoot) {
  CgroupPath root(tempdir_, "/");
  EXPECT_TRUE(ctx.addToCacheAndGet(root));
  EXPECT_THAT(ctx.cgroups(), ElementsAre(root));
}

TEST_F(OomdContextTest, GetMultiple) {
  F::materialize(F::makeDir(
      tempdir_,
      {F::makeDir("dir1"),
       F::makeDir("dir2"),
       F::makeDir("dir3"),
       F::makeFile("file1")}));
  CgroupPath p1(tempdir_, "dir1");
  CgroupPath p2(tempdir_, "dir2");
  CgroupPath p3(tempdir_, "dir3");
  CgroupPath p4(tempdir_, "file1");
  CgroupPath p5(tempdir_, "NOT_EXIST");

  auto cgroups = ctx.addToCacheAndGet({p1, p2, p3, p4, p5});
  auto cg1 = ctx.addToCacheAndGet(p1);
  auto cg2 = ctx.addToCacheAndGet(p2);
  auto cg3 = ctx.addToCacheAndGet(p3);
  ASSERT_TRUE(cg1);
  ASSERT_TRUE(cg2);
  ASSERT_TRUE(cg3);
  EXPECT_THAT(cgroups, UnorderedElementsAre(*cg1, *cg2, *cg3));

  CgroupPath p6(tempdir_, "*");
  EXPECT_THAT(
      ctx.addToCacheAndGet(std::unordered_set<CgroupPath>{p6}),
      UnorderedElementsAreArray(cgroups));
  // Check no duplicates
  EXPECT_THAT(
      ctx.addToCacheAndGet({p1, p2, p3, p4, p5, p6}),
      UnorderedElementsAreArray(cgroups));
  // Check empty result
  EXPECT_EQ(ctx.addToCacheAndGet({p4, p5}).size(), 0);
  EXPECT_EQ(ctx.addToCacheAndGet({}).size(), 0);
}

TEST_F(OomdContextTest, SortContext) {
  F::materialize(F::makeDir(
      tempdir_,
      {F::makeDir(
           "biggest",
           {F::makeFile("memory.current", "99999999\n"),
            F::makeFile("memory.low", "1\n")}),
       F::makeDir(
           "smallest",
           {F::makeFile("memory.current", "1\n"),
            F::makeFile("memory.low", "4\n")}),
       F::makeDir(
           "asdf",
           {F::makeFile("memory.current", "88888888\n"),
            F::makeFile("memory.low", "2\n")}),
       F::makeDir(
           "fdsa",
           {F::makeFile("memory.current", "77777777\n"),
            F::makeFile("memory.low", "3\n")})}));
  CgroupPath p1(tempdir_, "biggest");
  CgroupPath p2(tempdir_, "smallest");
  CgroupPath p3(tempdir_, "asdf");
  CgroupPath p4(tempdir_, "fdsa");

  auto sorted =
      ctx.reverseSort({p1, p2, p3, p4}, [](const CgroupContext& cgroup_ctx) {
        return cgroup_ctx.current_usage().value_or(0);
      });

  auto cg1 = ctx.addToCacheAndGet(p1);
  auto cg2 = ctx.addToCacheAndGet(p2);
  auto cg3 = ctx.addToCacheAndGet(p3);
  auto cg4 = ctx.addToCacheAndGet(p4);
  ASSERT_TRUE(cg1);
  ASSERT_TRUE(cg2);
  ASSERT_TRUE(cg3);
  ASSERT_TRUE(cg4);

  EXPECT_THAT(sorted, ElementsAre(*cg1, *cg3, *cg4, *cg2));

  CgroupPath p5(tempdir_, "*est");
  CgroupPath p6(tempdir_, "{biggest,smallest,asdf,fdsa}");
  CgroupPath p7(tempdir_, "NOT_EXIST");
  sorted = ctx.reverseSort({p5, p6, p7}, [](const CgroupContext& cgroup_ctx) {
    return cgroup_ctx.memory_low().value_or(0);
  });

  EXPECT_THAT(sorted, ElementsAre(*cg2, *cg4, *cg3, *cg1));
}
