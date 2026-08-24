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

#include <initializer_list>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "oomd/CgroupContext.h"
#include "oomd/OomdContext.h"
#include "oomd/PluginConstructionContext.h"
#include "oomd/PluginRegistry.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/DetectorGroup.h"
#include "oomd/engine/Ruleset.h"
#include "oomd/util/Fixture.h"

using namespace Oomd;
using namespace Oomd::Engine;

namespace Oomd {
namespace {

using CountMap = std::unordered_map<std::string, int>;

struct RunCounts {
  CountMap detector;
  CountMap prerun;
  CountMap pause;
  std::unordered_map<std::string, CountMap> action;
};

RunCounts& runCounts() {
  static RunCounts counts;
  return counts;
}

constexpr auto kTrackingDetectorName = "RulesetTestTrackingDetector";
constexpr auto kSecondRunDetectorName = "RulesetTestSecondRunDetector";
constexpr auto kRecordingActionName = "RulesetTestRecordingAction";
constexpr auto kPauseOnceActionName = "RulesetTestPauseOnceAction";

std::string currentRulesetCgroup(const OomdContext& context) {
  const auto cgroup = context.getRulesetCgroup();
  return cgroup ? cgroup->relativePath() : "";
}

void resetCounts() {
  runCounts().detector.clear();
  runCounts().prerun.clear();
  runCounts().pause.clear();
  runCounts().action.clear();
}

class RulesetTestTrackingDetector final : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 0;
  }

  void prerun(OomdContext& context) override {
    ++runCounts().prerun[currentRulesetCgroup(context)];
  }

  PluginRet run(OomdContext& context) override {
    ++runCounts().detector[currentRulesetCgroup(context)];
    return PluginRet::CONTINUE;
  }

  static RulesetTestTrackingDetector* create() {
    return new RulesetTestTrackingDetector();
  }
};

class RulesetTestSecondRunDetector final : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    runs_ = 0;
    return 0;
  }

  void prerun(OomdContext& context) override {
    ++runCounts().prerun[currentRulesetCgroup(context)];
  }

  PluginRet run(OomdContext& context) override {
    ++runCounts().detector[currentRulesetCgroup(context)];
    ++runs_;
    return runs_ >= 2 ? PluginRet::CONTINUE : PluginRet::STOP;
  }

  static RulesetTestSecondRunDetector* create() {
    return new RulesetTestSecondRunDetector();
  }

 private:
  int runs_{0};
};

class RulesetTestRecordingAction final : public BasePlugin {
 public:
  int init(
      const PluginArgs& args,
      const PluginConstructionContext& /* unused */) override {
    const auto label = args.find("label");
    if (label == args.end()) {
      return 1;
    }
    label_ = label->second;
    return 0;
  }

  PluginRet run(OomdContext& context) override {
    ++runCounts().action[label_][currentRulesetCgroup(context)];
    return PluginRet::CONTINUE;
  }

  static RulesetTestRecordingAction* create() {
    return new RulesetTestRecordingAction();
  }

 private:
  std::string label_;
};

class RulesetTestPauseOnceAction final : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    paused_ = false;
    return 0;
  }

  PluginRet run(OomdContext& context) override {
    ++runCounts().pause[currentRulesetCgroup(context)];
    if (!paused_) {
      paused_ = true;
      return PluginRet::ASYNC_PAUSED;
    }
    return PluginRet::CONTINUE;
  }

  static RulesetTestPauseOnceAction* create() {
    return new RulesetTestPauseOnceAction();
  }

 private:
  bool paused_{false};
};

REGISTER_PLUGIN(
    RulesetTestTrackingDetector,
    RulesetTestTrackingDetector::create);
REGISTER_PLUGIN(
    RulesetTestSecondRunDetector,
    RulesetTestSecondRunDetector::create);
REGISTER_PLUGIN(RulesetTestRecordingAction, RulesetTestRecordingAction::create);
REGISTER_PLUGIN(RulesetTestPauseOnceAction, RulesetTestPauseOnceAction::create);

} // namespace
} // namespace Oomd

class RulesetCgroupLifecycleTest : public ::testing::Test {
 protected:
  using F = Fixture;
  using PluginSpec = std::pair<std::string, PluginArgs>;

  void SetUp() override {
    tempdir_ = F::mkdtempChecked();
    resetCounts();
  }

  void TearDown() override {
    F::rmrChecked(tempdir_);
  }

  CgroupPath cgroup(const std::string& name) const {
    return CgroupPath(tempdir_, name);
  }

  void createCgroups(std::initializer_list<std::string> names) {
    for (const auto& name : names) {
      F::materialize(
          F::makeDir(name, {F::makeFile("cgroup.controllers")}), tempdir_);
    }
  }

  void removeCgroups(std::initializer_list<std::string> names) {
    for (const auto& name : names) {
      F::rmrChecked(cgroup(name).absolutePath());
    }
    context_.refresh();
  }

  std::unique_ptr<BasePlugin> makePlugin(
      const std::string& name,
      const PluginArgs& args = {}) {
    auto plugin = std::unique_ptr<BasePlugin>(getPluginRegistry().create(name));
    EXPECT_NE(plugin, nullptr);
    if (!plugin) {
      return nullptr;
    }

    plugin->setName(name);
    EXPECT_EQ(plugin->initPlugin(args, PluginConstructionContext(tempdir_)), 0);
    return plugin;
  }

  std::unique_ptr<Ruleset> makeRuleset(
      const std::string& detector_name,
      const std::vector<PluginSpec>& action_specs = {}) {
    std::vector<std::unique_ptr<BasePlugin>> detectors;
    detectors.emplace_back(makePlugin(detector_name));

    std::vector<std::unique_ptr<DetectorGroup>> detector_groups;
    detector_groups.emplace_back(
        std::make_unique<DetectorGroup>(
            "detector-group", std::move(detectors)));

    std::vector<std::unique_ptr<BasePlugin>> actions;
    actions.reserve(action_specs.size());
    for (const auto& [name, args] : action_specs) {
      actions.emplace_back(makePlugin(name, args));
    }

    return std::make_unique<Ruleset>(
        "ruleset",
        std::move(detector_groups),
        std::move(actions),
        /*disable_on_drop_in=*/false,
        /*detectorgroups_dropin_enabled=*/false,
        /*actiongroup_dropin_enabled=*/false,
        /*silence_logs=*/0,
        /*post_action_delay=*/0,
        /*prekill_hook_timeout=*/DEFAULT_PREKILL_HOOK_TIMEOUT,
        std::unordered_set<CgroupPath>{CgroupPath(tempdir_, "*")});
  }

  void expectEachOnce(
      const CountMap& actual,
      std::initializer_list<std::string> expected) {
    ASSERT_EQ(actual.size(), expected.size());
    for (const auto& name : expected) {
      const auto count = actual.find(name);
      ASSERT_NE(count, actual.end());
      EXPECT_EQ(count->second, 1);
    }
  }

  void expectRunnableCgroups(
      Ruleset& ruleset,
      std::initializer_list<std::string> expected) {
    runCounts().prerun.clear();
    ruleset.prerun(context_);
    expectEachOnce(runCounts().prerun, expected);
  }

  int actionRunCount(const std::string& label, const std::string& cgroup_name)
      const {
    const auto label_counts = runCounts().action.find(label);
    if (label_counts == runCounts().action.end()) {
      return 0;
    }
    const auto count = label_counts->second.find(cgroup_name);
    return count == label_counts->second.end() ? 0 : count->second;
  }

  OomdContext context_;
  std::string tempdir_;
};

TEST_F(RulesetCgroupLifecycleTest, RemovesOneDisappearedCgroup) {
  createCgroups({"A", "B", "C"});
  auto ruleset = makeRuleset(kTrackingDetectorName);
  ruleset->runOnce(context_);

  removeCgroups({"B"});
  runCounts().detector.clear();
  ruleset->runOnce(context_);

  expectEachOnce(runCounts().detector, {"A", "C"});
  expectRunnableCgroups(*ruleset, {"A", "C"});
}

TEST_F(RulesetCgroupLifecycleTest, RemovesAdjacentDisappearedCgroups) {
  createCgroups({"A", "B", "C", "D"});
  auto ruleset = makeRuleset(kTrackingDetectorName);
  ruleset->runOnce(context_);

  removeCgroups({"B", "C"});
  runCounts().detector.clear();
  ruleset->runOnce(context_);

  expectEachOnce(runCounts().detector, {"A", "D"});
  expectRunnableCgroups(*ruleset, {"A", "D"});
}

TEST_F(RulesetCgroupLifecycleTest, RemovesAllDisappearedCgroups) {
  createCgroups({"A", "B", "C"});
  auto ruleset = makeRuleset(kTrackingDetectorName);
  ruleset->runOnce(context_);

  removeCgroups({"A", "B", "C"});
  runCounts().detector.clear();
  ruleset->runOnce(context_);

  expectEachOnce(runCounts().detector, {});
  expectRunnableCgroups(*ruleset, {});
}

TEST_F(RulesetCgroupLifecycleTest, RecreatedPathGetsFreshDetectorState) {
  createCgroups({"task"});
  const auto path = cgroup("task");
  auto old_identity = CgroupContext::make(context_, path);
  ASSERT_TRUE(old_identity);
  const auto old_id = old_identity->id();
  ASSERT_TRUE(old_id);

  auto ruleset = makeRuleset(
      kSecondRunDetectorName, {{kRecordingActionName, {{"label", "record"}}}});
  ruleset->runOnce(context_);
  EXPECT_EQ(actionRunCount("record", "task"), 0);

  removeCgroups({"task"});
  ruleset->runOnce(context_);
  expectRunnableCgroups(*ruleset, {});

  createCgroups({"task"});
  auto new_identity = CgroupContext::make(context_, path);
  ASSERT_TRUE(new_identity);
  const auto new_id = new_identity->id();
  ASSERT_TRUE(new_id);
  EXPECT_NE(*old_id, *new_id);

  ruleset->runOnce(context_);
  EXPECT_EQ(actionRunCount("record", "task"), 0);
  ruleset->runOnce(context_);
  EXPECT_EQ(actionRunCount("record", "task"), 1);
}

TEST_F(
    RulesetCgroupLifecycleTest,
    DroppingAsyncPausedCgroupDoesNotResumeOnRecreatedPath) {
  createCgroups({"task"});
  auto ruleset = makeRuleset(
      kTrackingDetectorName,
      {{kRecordingActionName, {{"label", "before"}}},
       {kPauseOnceActionName, {}},
       {kRecordingActionName, {{"label", "after"}}}});

  ruleset->runOnce(context_);
  EXPECT_EQ(actionRunCount("before", "task"), 1);
  EXPECT_EQ(runCounts().pause.at("task"), 1);
  EXPECT_EQ(actionRunCount("after", "task"), 0);

  removeCgroups({"task"});
  ruleset->runOnce(context_);
  expectRunnableCgroups(*ruleset, {});

  createCgroups({"task"});
  ruleset->runOnce(context_);
  EXPECT_EQ(actionRunCount("before", "task"), 2);
  EXPECT_EQ(runCounts().pause.at("task"), 2);
  EXPECT_EQ(actionRunCount("after", "task"), 0);
}
