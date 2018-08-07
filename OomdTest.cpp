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

#include "oomd/OomDetector.h"
#include "oomd/OomKiller.h"
#include "oomd/Oomd.h"

using namespace Oomd;
using namespace testing;

constexpr auto kCgroupDataDir = "oomd/fixtures/cgroup";

class OomdTest : public ::testing::Test {
 public:
  OomdTest() {
    auto tunables = std::make_shared<Tunables>();
    std::unique_ptr<OomDetector> detector{nullptr};
    std::unique_ptr<OomKiller> killer{nullptr};

    oomd.setTunables(tunables);
    oomd.addCgroup(std::move(detector), std::move(killer));
    oomd.prepareRun();
  }

  std::string cgroup_path{kCgroupDataDir};
  OomdContext ctx;
  ::Oomd::Oomd oomd;
};

TEST_F(OomdTest, OomdContextUpdate) {
  EXPECT_EQ(ctx.cgroups().size(), 0);

  oomd.updateContext(cgroup_path, ctx);

  EXPECT_EQ(ctx.cgroups().size(), 5);

  EXPECT_TRUE(ctx.hasCgroupContext("service1.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("service2.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("service3.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("service4.service"));
  EXPECT_TRUE(ctx.hasCgroupContext("slice1.slice"));
}

TEST_F(OomdTest, OomdContextMultipleUpdates) {
  oomd.updateContext(cgroup_path, ctx);

  for (int i = 0; i < 3; i++) {
    int64_t average = ctx.getCgroupContext("service1.service").average_usage;
    oomd.updateContext(cgroup_path, ctx);

    // We expect the avg usage to slowly converge from 0 -> true avg
    // b/c of AVERAGE_SIZE_DECAY
    EXPECT_GT(ctx.getCgroupContext("service1.service").average_usage, average);
  }
}
