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

#include <memory>
#include <vector>

#include "oomd/OomKiller.h"
#include "oomd/shared/KillList.h"
#include "oomd/shared/OomdContext.h"
#include "oomd/shared/Tunables.h"

using namespace Oomd;
using namespace testing;

static constexpr auto kCgroupDataDir = "oomd/fixtures/cgroup";

namespace Oomd {
class MockOomKiller : public OomKiller {
 public:
  explicit MockOomKiller(PluginArgs& args) : OomKiller(args) {}
  MOCK_METHOD1(tryToKillCgroup, bool(const std::string&));
  MOCK_METHOD1(tryToKillPids, int(const std::vector<int>& procs));
};
} // namespace Oomd

class OomKillerTest : public ::testing::Test {
 public:
  OomKillerTest() {
    std::string cgroup_path(kCgroupDataDir);
    auto kl = std::make_shared<KillList>();
    kl->emplace_back(KillListEntry{"service1.service", 0, 0});
    auto tunables = std::make_shared<Tunables>();
    PluginArgs args{cgroup_path, kl, tunables, false};

    killer = std::make_unique<MockOomKiller>(args);
  }

  std::unique_ptr<MockOomKiller> killer{nullptr};
};

class OomKillerBlacklistTest : public ::testing::Test {
 public:
  OomKillerBlacklistTest() {
    std::string cgroup_path(kCgroupDataDir);
    auto kl = std::make_shared<KillList>();
    kl->emplace_back(
        KillListEntry{"service1.service", 0, std::numeric_limits<int>::max()});
    kl->emplace_back(KillListEntry{"service2.service", 0, 0});
    kl->emplace_back(
        KillListEntry{"service3.service", 0, std::numeric_limits<int>::max()});
    kl->emplace_back(
        KillListEntry{"service4.service", 0, std::numeric_limits<int>::max()});
    auto tunables = std::make_shared<Tunables>();
    PluginArgs args{cgroup_path, kl, tunables, false};

    killer = std::make_unique<MockOomKiller>(args);
  }

  std::unique_ptr<MockOomKiller> killer{nullptr};
};

TEST_F(OomKillerTest, TriesToKillCgroup) {
  // We force tryToKillPids to return 0. This forces the test to go through
  // all the different kill heuristics in tryToKillSomething
  ON_CALL(*killer, tryToKillPids(SizeIs(Eq(2)))).WillByDefault(Return(0));
  EXPECT_CALL(*killer, tryToKillCgroup(StrEq("service1.service")))
      .Times(AtLeast(1));

  OomdContext ctx;
  ctx.setCgroupContext(
      "service1.service", {{88.8, 88.8, 88.8}, 999999999, 999999999, 0});

  // We have forced tryToKillPids(..) to return 0, indicating
  // failure to kill. This means tryToKillSomething should fail
  // too
  EXPECT_FALSE(killer->tryToKillSomething(ctx));
}

TEST_F(OomKillerTest, TriesToKillCgroupRecursively) {
  ON_CALL(*killer, tryToKillCgroup(_)).WillByDefault(Return(true));
  EXPECT_CALL(*killer, tryToKillCgroup(StrEq("slice1.slice"))).Times(1);
  EXPECT_CALL(*killer, tryToKillCgroup(StrEq("slice1.slice/service1.service")))
      .Times(1);
  EXPECT_CALL(*killer, tryToKillCgroup(StrEq("slice1.slice/service2.service")))
      .Times(1);
  EXPECT_TRUE(killer->tryToKillCgroupRecursively("slice1.slice"));
}

TEST_F(OomKillerTest, TriesToKillCgroupRecursivelyOnLeaf) {
  ON_CALL(*killer, tryToKillCgroup(_)).WillByDefault(Return(true));
  EXPECT_CALL(*killer, tryToKillCgroup(StrEq("service1.service"))).Times(1);
  EXPECT_TRUE(killer->tryToKillCgroupRecursively("service1.service"));
}

TEST_F(OomKillerBlacklistTest, Blacklist) {
  // We force tryToKillPids to return 0. This forces the test to go through
  // all the different kill heuristics in tryToKillSomething
  ON_CALL(*killer, tryToKillPids(SizeIs(Eq(2)))).WillByDefault(Return(0));
  EXPECT_CALL(*killer, tryToKillCgroup(StrEq("service2.service")))
      .Times(AtLeast(1));

  OomdContext ctx;
  ctx.setCgroupContext("service1.service", {{88.8, 88.8, 88.8}, 555, 555, 0});
  ctx.setCgroupContext("service2.service", {{88.8, 88.8, 88.8}, 555, 555, 0});
  ctx.setCgroupContext("service3.service", {{88.8, 88.8, 88.8}, 555, 555, 0});
  ctx.setCgroupContext("service4.service", {{88.8, 88.8, 88.8}, 555, 555, 0});

  // Run the loop a few times to reduce variance on it being a lucky pick
  for (int i = 0; i < 15; ++i) {
    // We have forced tryToKillPids(..) to return 0, indicating
    // failure to kill. This means tryToKillSomething should fail
    // too
    EXPECT_FALSE(killer->tryToKillSomething(ctx));
  }
}
