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

#include "oomd/dropin/FsDropInService.h"
#include "oomd/util/Fixture.h"
#include "oomd/util/TestHelper.h"

namespace Oomd {

namespace {

// Defines MockPlugin class registered with name "FsDropInTest"
DEFINE_MOCK_PLUGIN(FsDropInTest);

using namespace Config2::IR;
const Root root{
    .rulesets = {
        Ruleset{.name = "drop in ruleset",
                .dgs = {DetectorGroup{
                    .name = "detector group 0",
                    .detectors = {{MockPlugin::createIR("RegularDetector")}}}},
                .acts = {{MockPlugin::createIR("RegularAction")}},
                .dropin = DropIn{.disable_on_drop_in = true,
                                 .detectorgroups_enabled = true,
                                 .actiongroup_enabled = true}}}};
constexpr auto drop_in_action = R"JSON({
  "rulesets": [
    {
      "name": "drop in ruleset",
      "actions": [
        {
          "name": "FsDropInTest",
          "args": {
            "id": "DropInAction"
          }
        }
      ]
    }
  ]
})JSON";

} // namespace

class FsDropInServiceTest : public ::testing::Test {
 protected:
  void SetUp() override {
    MockPlugin::runCounts().clear();
    expectedRunCounts_.clear();

    PluginConstructionContext ctx("/sys/fs/cgroup");

    ctx_ = OomdContext();
    engine_ = Config2::compile(root, ctx);
    ASSERT_TRUE(engine_ != nullptr);

    drop_in_dir_ = Fixture::mkdtempChecked();
    drop_in_service_ =
        FsDropInService::create("/sys/fs/cgroup", root, *engine_, drop_in_dir_);
    ASSERT_TRUE(drop_in_service_ != nullptr);
  }

  void TearDown() override {
    if (drop_in_dir_.size()) {
      Fixture::rmrChecked(drop_in_dir_);
    }
  }

  /*
   * FsDropInService uses epoll_wait on inotify to monitor drop in configs.
   * There is no easy way to tell if FS operations have been processed, so let's
   * sleep for a few jiffies.
   */
  void wait_for_inotify() {
    /* sleep override */
    std::this_thread::sleep_for(std::chrono::milliseconds(50));
  }

  OomdContext ctx_;
  std::unique_ptr<Engine::Engine> engine_;
  std::string drop_in_dir_;
  std::unique_ptr<FsDropInService> drop_in_service_;
  std::unordered_map<std::string, int> expectedRunCounts_;
};

TEST_F(FsDropInServiceTest, AddRemove) {
  Fixture::materialize(
      Fixture::makeFile("drop_in_action.json", drop_in_action), drop_in_dir_);
  wait_for_inotify();
  drop_in_service_->updateDropIns();
  engine_->runOnce(ctx_);
  expectedRunCounts_ = {{"RegularDetector", 1}, {"DropInAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
  MockPlugin::runCounts().clear();

  Fixture::rmrChecked(drop_in_dir_ + "/drop_in_action.json");
  wait_for_inotify();
  drop_in_service_->updateDropIns();
  engine_->runOnce(ctx_);
  expectedRunCounts_ = {{"RegularDetector", 1}, {"RegularAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
  MockPlugin::runCounts().clear();
}

TEST_F(FsDropInServiceTest, LoadExisting) {
  // Recreate drop in service after creating drop in configs to simulate oomd
  // restarts.
  drop_in_service_ = nullptr;
  Fixture::materialize(
      Fixture::makeFile("drop_in_action.json", drop_in_action), drop_in_dir_);
  drop_in_service_ =
      FsDropInService::create("/sys/fs/cgroup", root, *engine_, drop_in_dir_);
  ASSERT_TRUE(drop_in_service_ != nullptr);

  drop_in_service_->updateDropIns();
  engine_->runOnce(ctx_);
  expectedRunCounts_ = {{"RegularDetector", 1}, {"DropInAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
  MockPlugin::runCounts().clear();
}

TEST_F(FsDropInServiceTest, RecreateDir) {
  // Remove and recreate drop in dir
  Fixture::rmrChecked(drop_in_dir_);
  Fixture::materialize(Fixture::makeDir(drop_in_dir_));

  wait_for_inotify();
  // tick() should setup watcher for drop in dir again
  drop_in_service_->updateDropIns();

  // Adding drop in config to recreated drop in dir should work
  Fixture::materialize(
      Fixture::makeFile("drop_in_action.json", drop_in_action), drop_in_dir_);
  wait_for_inotify();
  drop_in_service_->updateDropIns();
  engine_->runOnce(ctx_);
  expectedRunCounts_ = {{"RegularDetector", 1}, {"DropInAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
}
} // namespace Oomd
