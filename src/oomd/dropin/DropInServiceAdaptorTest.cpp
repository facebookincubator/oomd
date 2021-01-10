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

#include "oomd/PluginConstructionContext.h"
#include "oomd/config/ConfigCompiler.h"
#include "oomd/dropin/DropInServiceAdaptor.h"
#include "oomd/util/TestHelper.h"

namespace Oomd {

namespace {

// Defines MockPlugin class registered with name "AdaptorTest"
DEFINE_MOCK_PLUGIN(AdaptorTest)

using namespace Config2::IR;
const Root root{
    .rulesets = {Ruleset{
        .name = "drop in ruleset",
        .dgs = {DetectorGroup{
            .name = "detector group 0",
            .detectors = {{MockPlugin::createIR("RegularDetector")}}}},
        .acts = {{MockPlugin::createIR("RegularAction")}},
        .dropin = DropIn{
            .disable_on_drop_in = true,
            .detectorgroups_enabled = true,
            .actiongroup_enabled = true}}}};
const Root drop_in_detector{
    .rulesets = {Ruleset{
        .name = "drop in ruleset",
        .dgs = {DetectorGroup{
            .name = "drop in detector group 0",
            .detectors = {{MockPlugin::createIR("DropInDetector")}}}}}}};
const Root drop_in_action{
    .rulesets = {Ruleset{
        .name = "drop in ruleset",
        .acts = {{MockPlugin::createIR("DropInAction")}}}}};

class MockAdaptor : public DropInServiceAdaptor {
 public:
  MockAdaptor(
      const std::string& cgroup_fs,
      const Root& root,
      Engine::Engine& engine)
      : DropInServiceAdaptor(cgroup_fs, root, engine) {}

  bool scheduleDropInAdd(const std::string& tag, const Root& drop_in) {
    return DropInServiceAdaptor::scheduleDropInAdd(tag, drop_in);
  }
  void scheduleDropInRemove(const std::string& tag) {
    DropInServiceAdaptor::scheduleDropInRemove(tag);
  }

  MOCK_METHOD0(tick, void());
  MOCK_METHOD2(handleDropInAddResult, void(const std::string&, bool));
  MOCK_METHOD2(handleDropInRemoveResult, void(const std::string&, bool));
};
} // namespace

class DropInServiceAdaptorTest : public ::testing::Test {
 protected:
  void SetUp() override {
    MockPlugin::runCounts().clear();
    expectedRunCounts_.clear();

    PluginConstructionContext ctx("/sys/fs/cgroup");

    ctx_ = OomdContext();
    engine_ = Config2::compile(root, ctx);
    ASSERT_TRUE(engine_ != nullptr);
    adaptor_ = std::make_unique<MockAdaptor>("/sys/fs/cgroup", root, *engine_);
  }

  OomdContext ctx_;
  std::unique_ptr<Engine::Engine> engine_;
  std::unique_ptr<MockAdaptor> adaptor_;
  std::unordered_map<std::string, int> expectedRunCounts_;
};

TEST_F(DropInServiceAdaptorTest, AddRemove) {
  // Add drop in detector
  EXPECT_TRUE(
      adaptor_->scheduleDropInAdd("drop_in_detector.json", drop_in_detector));

  EXPECT_CALL(*adaptor_, tick());
  EXPECT_CALL(*adaptor_, handleDropInAddResult("drop_in_detector.json", true));
  adaptor_->updateDropIns();
  ::testing::Mock::VerifyAndClearExpectations(&*adaptor_);

  engine_->runOnce(ctx_);
  expectedRunCounts_ = {{"DropInDetector", 1}, {"RegularAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
  MockPlugin::runCounts().clear();

  // Add drop in action (now with two drop ins)
  EXPECT_TRUE(
      adaptor_->scheduleDropInAdd("drop_in_action.json", drop_in_action));

  EXPECT_CALL(*adaptor_, tick());
  EXPECT_CALL(*adaptor_, handleDropInAddResult("drop_in_action.json", true));
  adaptor_->updateDropIns();
  ::testing::Mock::VerifyAndClearExpectations(&*adaptor_);

  engine_->runOnce(ctx_);
  expectedRunCounts_ = {
      {"DropInDetector", 1},
      {"RegularAction", 1},
      {"RegularDetector", 1},
      {"DropInAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
  MockPlugin::runCounts().clear();

  // Remove drop in detector (with drop in action left)
  adaptor_->scheduleDropInRemove("drop_in_detector.json");

  EXPECT_CALL(*adaptor_, tick());
  EXPECT_CALL(
      *adaptor_, handleDropInRemoveResult("drop_in_detector.json", true));
  adaptor_->updateDropIns();
  ::testing::Mock::VerifyAndClearExpectations(&*adaptor_);

  engine_->runOnce(ctx_);
  expectedRunCounts_ = {{"RegularDetector", 1}, {"DropInAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
  MockPlugin::runCounts().clear();

  // Remove drop in action (no drop in left)
  adaptor_->scheduleDropInRemove("drop_in_action.json");

  EXPECT_CALL(*adaptor_, tick());
  EXPECT_CALL(*adaptor_, handleDropInRemoveResult("drop_in_action.json", true));
  adaptor_->updateDropIns();
  ::testing::Mock::VerifyAndClearExpectations(&*adaptor_);

  engine_->runOnce(ctx_);
  expectedRunCounts_ = {{"RegularDetector", 1}, {"RegularAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
}

TEST_F(DropInServiceAdaptorTest, QueuedAddRemove) {
  // Add and then remove processed in order
  EXPECT_TRUE(
      adaptor_->scheduleDropInAdd("drop_in_action.json", drop_in_action));
  adaptor_->scheduleDropInRemove("drop_in_action.json");

  EXPECT_CALL(*adaptor_, tick());
  EXPECT_CALL(*adaptor_, handleDropInAddResult("drop_in_action.json", true));
  EXPECT_CALL(*adaptor_, handleDropInRemoveResult("drop_in_action.json", true));
  adaptor_->updateDropIns();
  ::testing::Mock::VerifyAndClearExpectations(&*adaptor_);

  engine_->runOnce(ctx_);
  // No drop in
  expectedRunCounts_ = {{"RegularDetector", 1}, {"RegularAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
  MockPlugin::runCounts().clear();

  // Remove and then add processed in order
  adaptor_->scheduleDropInRemove("drop_in_action.json");
  EXPECT_TRUE(
      adaptor_->scheduleDropInAdd("drop_in_action.json", drop_in_action));

  EXPECT_CALL(*adaptor_, tick());
  EXPECT_CALL(*adaptor_, handleDropInAddResult("drop_in_action.json", true));
  // Currently remove never fails, even if it's no-op
  EXPECT_CALL(*adaptor_, handleDropInRemoveResult("drop_in_action.json", true));
  adaptor_->updateDropIns();
  ::testing::Mock::VerifyAndClearExpectations(&*adaptor_);

  engine_->runOnce(ctx_);
  // Drop in action injected
  expectedRunCounts_ = {{"RegularDetector", 1}, {"DropInAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
}

TEST_F(DropInServiceAdaptorTest, AddFail) {
  Root bad_drop_in_action{
      .rulesets = {Ruleset{
          .name = "drop in ruleset",
          .acts = {Action{Plugin{.name = "BadPluginName"}}}}}};

  EXPECT_FALSE(
      adaptor_->scheduleDropInAdd("drop_in_action.json", bad_drop_in_action));

  Root bad_drop_in_ruleset{
      .rulesets = {Ruleset{
          .name = "bad drop in ruleset",
          .acts = {Action{Plugin{.name = "AdaptorTest"}}}}}};
  EXPECT_FALSE(
      adaptor_->scheduleDropInAdd("drop_in_action.json", bad_drop_in_ruleset));

  adaptor_->updateDropIns();
  engine_->runOnce(ctx_);
  // No drop in added
  expectedRunCounts_ = {{"RegularDetector", 1}, {"RegularAction", 1}};
  EXPECT_EQ(MockPlugin::runCounts(), expectedRunCounts_);
}
} // namespace Oomd
