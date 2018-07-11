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

TEST_F(OomKillerTest, TriesToKillCgroup) {
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
