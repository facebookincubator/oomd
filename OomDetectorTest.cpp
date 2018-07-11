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

using namespace Oomd;
using namespace testing;

constexpr auto kCgroupDataDir = "oomd/fixtures/cgroup";

class OomDetectorTest : public ::testing::Test {
 public:
  OomDetectorTest() {
    auto kill_list = std::make_shared<KillList>();
    auto tunables = std::make_shared<Tunables>();
    std::string cgroup_path(kCgroupDataDir);
    PluginArgs args{cgroup_path, kill_list, tunables, true};
    detector = std::make_shared<OomDetector>(args);
  }

  OomdContext ctx;
  std::shared_ptr<OomDetector> detector{nullptr};
};

TEST_F(OomDetectorTest, CheckNoInstantOOM) {
  EXPECT_FALSE(detector->isOOM(ctx));
}
