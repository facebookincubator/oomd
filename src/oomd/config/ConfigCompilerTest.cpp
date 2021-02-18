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
#include <unordered_map>

#include "oomd/PluginRegistry.h"
#include "oomd/config/ConfigCompiler.h"
#include "oomd/config/ConfigTypes.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/engine/PrekillHook.h"

using namespace Oomd;
using namespace Oomd::Config2;
using namespace Oomd::Engine;
using namespace testing;

namespace {
int prerun_count;
int prerun_stored_count;
int count;
int stored_count;
bool controlled_detector_on;
} // namespace

static constexpr auto kRandomCgroupFs = "/some/random/fs";

namespace Oomd {

class ContinuePlugin : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 0;
  }

  void prerun(OomdContext& /* unused */) override {
    ++prerun_count;
  }

  PluginRet run(OomdContext& /* unused */) override {
    return PluginRet::CONTINUE;
  }

  static ContinuePlugin* create() {
    return new ContinuePlugin();
  }

  ~ContinuePlugin() override = default;
};

class StopPlugin : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 0;
  }

  void prerun(OomdContext& /* unused */) override {
    ++prerun_count;
  }

  PluginRet run(OomdContext& /* unused */) override {
    return PluginRet::STOP;
  }

  static StopPlugin* create() {
    return new StopPlugin();
  }

  ~StopPlugin() override = default;
};

class IncrementCountPlugin : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 0;
  }

  void prerun(OomdContext& /* unused */) override {
    ++prerun_count;
  }

  PluginRet run(OomdContext& /* unused */) override {
    ++count;
    return PluginRet::CONTINUE;
  }

  static IncrementCountPlugin* create() {
    return new IncrementCountPlugin();
  }

  ~IncrementCountPlugin() override = default;
};

class StoreCountPlugin : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 0;
  }

  void prerun(OomdContext& /* unused */) override {
    ++prerun_count;
  }

  PluginRet run(OomdContext& /* unused */) override {
    stored_count = count;
    prerun_stored_count = prerun_count;
    return PluginRet::CONTINUE;
  }

  static StoreCountPlugin* create() {
    return new StoreCountPlugin();
  }

  ~StoreCountPlugin() override = default;
};

class ControlledDetectorPlugin : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 0;
  }

  void prerun(OomdContext& /* unused */) override {
    ++prerun_count;
  }

  PluginRet run(OomdContext& /* unused */) override {
    return controlled_detector_on ? PluginRet::CONTINUE : PluginRet::STOP;
  }

  static ControlledDetectorPlugin* create() {
    return new ControlledDetectorPlugin();
  }

  ~ControlledDetectorPlugin() override = default;
};

class AsyncPausePlugin : public BasePlugin {
  int pause_count_{3};
  int pauses_left_;

 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    pauses_left_ = pause_count_;
    return 0;
  }

  void prerun(OomdContext& /* unused */) override {
    ++prerun_count;
  }

  PluginRet run(OomdContext& /* unused */) override {
    bool will_pause = pauses_left_ > 0;
    if (will_pause) {
      pauses_left_--;
      return PluginRet::ASYNC_PAUSED;
    } else {
      pauses_left_ = pause_count_;
      return PluginRet::CONTINUE;
    }
  }

  static AsyncPausePlugin* create() {
    return new AsyncPausePlugin();
  }

  ~AsyncPausePlugin() override = default;
};

class NoInitPlugin : public BasePlugin {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 1;
  }

  void prerun(OomdContext& /* unused */) override {
    ++prerun_count;
  }

  PluginRet run(OomdContext& /* unused */) override {
    return PluginRet::CONTINUE;
  }

  static NoInitPlugin* create() {
    return new NoInitPlugin();
  }

  ~NoInitPlugin() override = default;
};

class NoOpPrekillHook : public PrekillHook {
 public:
  int init(
      const PluginArgs& /* unused */,
      const PluginConstructionContext& /* unused */) override {
    return 0;
  }

  static NoOpPrekillHook* create() {
    return new NoOpPrekillHook();
  }

  ~NoOpPrekillHook() override = default;
};

REGISTER_PLUGIN(Continue, ContinuePlugin::create);
REGISTER_PLUGIN(Stop, StopPlugin::create);
REGISTER_PLUGIN(IncrementCount, IncrementCountPlugin::create);
REGISTER_PLUGIN(StoreCount, StoreCountPlugin::create);
REGISTER_PLUGIN(ControlledDetector, ControlledDetectorPlugin::create);
REGISTER_PLUGIN(AsyncPause, AsyncPausePlugin::create);
REGISTER_PLUGIN(NoInit, NoInitPlugin::create);
REGISTER_PREKILL_HOOK(NoOpPrekillHook, NoOpPrekillHook::create);

} // namespace Oomd

class CompilerTest : public ::testing::Test {
 public:
  CompilerTest() {
    prerun_count = 0;
    prerun_stored_count = 0;
    count = 0;
    stored_count = 0;
    controlled_detector_on = false;
  }

  std::unique_ptr<::Oomd::Engine::Engine> compile() {
    const PluginConstructionContext compile_context(kRandomCgroupFs);
    return Config2::compile(root, compile_context);
  }

  OomdContext context;
  IR::Root root;
};

class DropInCompilerTest : public ::testing::Test {
 public:
  DropInCompilerTest() {
    prerun_count = 0;
    prerun_stored_count = 0;
    count = 0;
    stored_count = 0;
  }

  std::unique_ptr<::Oomd::Engine::Engine> compileBase() {
    const PluginConstructionContext compile_context(kRandomCgroupFs);
    return compile(root, compile_context);
  }

  std::optional<DropInUnit> compileDropIn() {
    const PluginConstructionContext compile_context(kRandomCgroupFs);
    return ::Oomd::Config2::compileDropIn(root, dropin_ir, compile_context);
  }

  OomdContext context;
  IR::Root root;
  IR::Root dropin_ir;
};

TEST_F(CompilerTest, IncrementCount) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action increment;
  increment.name = "IncrementCount";
  IR::DetectorGroup dgroup{"group1", {std::move(cont)}};
  IR::Ruleset ruleset{"ruleset1", {std::move(dgroup)}, {std::move(increment)}};
  root.rulesets.emplace_back(std::move(ruleset));

  auto engine = compile();
  ASSERT_TRUE(engine);
  for (int i = 0; i < 3; ++i) {
    engine->runOnce(context);
  }

  EXPECT_EQ(count, 3);
}

TEST_F(CompilerTest, MultiGroupIncrementCount) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Detector stop;
  stop.name = "Stop";
  IR::Action increment;
  increment.name = "IncrementCount";
  IR::DetectorGroup dgroup1{"group1", {cont}};
  IR::DetectorGroup dgroup2{"group2", {stop}};
  IR::DetectorGroup dgroup3{"group3", {cont}};
  IR::Ruleset ruleset1{"ruleset1", {dgroup1}, {increment}};
  IR::Ruleset ruleset2{"ruleset2", {dgroup2}, {increment}};
  IR::Ruleset ruleset3{"ruleset3", {dgroup3}, {increment}};
  root.rulesets.emplace_back(std::move(ruleset1));
  root.rulesets.emplace_back(std::move(ruleset2));
  root.rulesets.emplace_back(std::move(ruleset3));

  auto engine = compile();
  ASSERT_TRUE(engine);
  for (int i = 0; i < 3; ++i) {
    engine->runOnce(context);
  }

  EXPECT_EQ(count, 6);
}

TEST_F(CompilerTest, AsyncAction) {
  IR::Action cont{IR::Plugin{.name = "Continue"}};
  IR::Action inc{IR::Plugin{.name = "IncrementCount"}};
  IR::Action stop{IR::Plugin{.name = "Stop"}};
  IR::Action pause{IR::Plugin{.name = "AsyncPause"}};

  IR::DetectorGroup dg{
      .name = "dg",
      .detectors = {IR::Detector{IR::Plugin{.name = "Continue"}}}};

  // Each enabled plugin will prerun() once, including action that's not taken.
  root.rulesets = {
      IR::Ruleset{
          .name = "async_pausing",
          .dgs = {dg},
          .acts =
              {inc,
               inc,
               pause, // (1)
               inc,
               inc,
               inc,
               pause, // (2)
               inc,
               stop}}, // (3)
      IR::Ruleset{
          .name = "concurrently_always_running", .dgs = {dg}, .acts = {inc}},
  };

  auto engine = compile();
  ASSERT_TRUE(engine);

  auto run_once = [&] {
    const int count_before = count;
    engine->prerun(context);
    engine->runOnce(context);
    int delta_count = count - count_before;
    return delta_count;
  };

  // Run 3 times to check for ruleset's state is reset when finished executing
  // action chain. behavior. AsyncPausePlugin resets itself.
  for (int i = 0; i < 3; i++) {
    // reset count on each run
    count = 0;

    EXPECT_EQ(run_once(), 3); // (1) + 1 from concurrently_always_running
    // 2 more pauses in 3-run pause, each w/ 1 from concurrently_always_running
    EXPECT_EQ(run_once(), 1);
    EXPECT_EQ(run_once(), 1);
    EXPECT_EQ(run_once(), 4); // (2) + 1 from concurrently_always_running
    // 2 more pauses in 3-run pause
    EXPECT_EQ(run_once(), 1);
    EXPECT_EQ(run_once(), 1);
    EXPECT_EQ(run_once(), 2); // (3) + 1 from concurrently_always_running
  }
}

TEST_F(CompilerTest, TwoChainsIndependentlyPaused) {
  IR::Action cont{IR::Plugin{.name = "Continue"}};
  IR::Action inc{IR::Plugin{.name = "IncrementCount"}};
  IR::Action stop{IR::Plugin{.name = "Stop"}};
  IR::Action pause{IR::Plugin{.name = "AsyncPause"}};

  IR::DetectorGroup always_yes{
      .name = "dg",
      .detectors = {IR::Detector{IR::Plugin{.name = "Continue"}}}};

  // Each enabled plugin will prerun() once, including action that's not taken.
  root.rulesets = {
      IR::Ruleset{
          .name = "A",
          .dgs = {always_yes},
          .acts =
              {inc,
               inc,
               pause, // (1)
               inc,
               inc,
               inc,
               pause, // (2)
               inc,
               stop}}, // (3)
      IR::Ruleset{
          .name = "B",
          .dgs = {IR::DetectorGroup{
              .name = "ctld",
              .detectors = {IR::Detector{
                  IR::Plugin{.name = "ControlledDetector"}}}}},
          .acts = {
              inc,
              inc,
              inc,
              pause, // (4)
              inc,
              inc}}}; // (5)

  auto engine = compile();
  ASSERT_TRUE(engine);

  auto run_once = [&] {
    const int count_before = count;
    engine->prerun(context);
    engine->runOnce(context);
    int delta_count = count - count_before;
    return delta_count;
  };

  // Run 3 times to check for ruleset's state is reset when finished executing
  // action chain. behavior. AsyncPausePlugin resets itself.
  for (int i = 0; i < 1; i++) {
    // reset count on each run
    count = 0;

    EXPECT_EQ(run_once(), 2); // A.1, B idle
    EXPECT_EQ(run_once(), 0); // A.1 pause 2, B idle
    controlled_detector_on = true; // while A is paused, start B
    EXPECT_EQ(run_once(), 3); // A.1 pause 3, B.4
    controlled_detector_on = false;
    EXPECT_EQ(run_once(), 3); // A.2, B.4 pause 2
    EXPECT_EQ(run_once(), 0); // A.2 pause 2, B.4 pause 3
    EXPECT_EQ(run_once(), 2); // A.2 pause 3, B.5
    EXPECT_EQ(run_once(), 1); // A.3, B idle
  }
}

TEST_F(CompilerTest, IncrementCountNoop) {
  IR::Detector stop;
  stop.name = "Stop";
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action increment;
  increment.name = "IncrementCount";
  IR::DetectorGroup dgroup{"group1", {std::move(cont), std::move(stop)}};
  IR::Ruleset ruleset{"ruleset1", {std::move(dgroup)}, {std::move(increment)}};
  root.rulesets.emplace_back(std::move(ruleset));

  auto engine = compile();
  ASSERT_TRUE(engine);
  for (int i = 0; i < 3; ++i) {
    engine->runOnce(context);
  }

  EXPECT_EQ(count, 0);
}

TEST_F(CompilerTest, PrekillHook) {
  IR::PrekillHook hook{IR::Plugin{.name = "NoOpPrekillHook"}};
  root.prekill_hooks.push_back(std::move(hook));
  auto engine = compile();
  ASSERT_TRUE(engine);
  ASSERT_EQ(engine->getPrekillHooks().size(), 1);
}

TEST_F(DropInCompilerTest, PrerunCount) {
  IR::Plugin cont{.name = "Continue"};
  IR::Plugin stop{.name = "Stop"};
  // Each enabled plugin will prerun() once, including action that's not taken.
  root.rulesets = {
      // 2 / 0 plugins with/without dropin
      IR::Ruleset{
          .name = "disabled_dropin_target",
          .dgs = {IR::DetectorGroup{
              .name = "group1", .detectors = {IR::Detector{cont}}}},
          .acts = {IR::Action{cont}},
          .dropin =
              IR::DropIn{
                  .disable_on_drop_in = true, .actiongroup_enabled = true}},
      // 3 plugins (action won't be taken as we stop early)
      IR::Ruleset{
          .name = "enabled_dropin_target",
          .dgs = {IR::DetectorGroup{
              .name = "group1",
              .detectors = {IR::Detector{stop}, IR::Detector{cont}}}},
          .acts = {IR::Action{cont}},
          .dropin =
              IR::DropIn{
                  .disable_on_drop_in = false, .actiongroup_enabled = true}},
      // 2 plugins (StoreCount stores prerun_count)
      IR::Ruleset{
          .name = "rs",
          .dgs = {IR::DetectorGroup{
              .name = "group1", .detectors = {IR::Detector{cont}}}},
          .acts = {IR::Action{IR::Plugin{.name = "StoreCount"}}}},
  };
  // Plugins from dropin should also be prerun()
  dropin_ir.rulesets = {
      // 1 + 2 = 3 plugins
      IR::Ruleset{
          .name = "disabled_dropin_target",
          .acts =
              {IR::Action{IR::Plugin{.name = "IncrementCount"}},
               IR::Action{IR::Plugin{.name = "IncrementCount"}}}},
      // 2 + 3 = 5 plugins
      IR::Ruleset{
          .name = "enabled_dropin_target",
          .acts =
              {IR::Action{IR::Plugin{.name = "IncrementCount"}},
               IR::Action{IR::Plugin{.name = "IncrementCount"}},
               IR::Action{IR::Plugin{.name = "IncrementCount"}}}},
  };

  auto engine = compileBase();
  ASSERT_TRUE(engine);
  engine->prerun(context);
  engine->runOnce(context);
  EXPECT_EQ(prerun_count, 2 + 3 + 2);

  auto dropin = compileDropIn();
  ASSERT_TRUE(dropin.has_value());
  EXPECT_EQ(dropin->rulesets.size(), 2);

  EXPECT_TRUE(engine->addDropInConfig("0", std::move(dropin->rulesets.at(0))));
  EXPECT_TRUE(engine->addDropInConfig("1", std::move(dropin->rulesets.at(1))));
  prerun_count = 0;
  engine->prerun(context);
  engine->runOnce(context);
  EXPECT_EQ(prerun_count, 0 + 3 + 2 + 3 + 5);
}

TEST_F(CompilerTest, NoInitPlugin) {
  IR::Detector noinit;
  noinit.name = "NoInit";
  IR::Action reg;
  reg.name = "Register";
  IR::DetectorGroup dgroup{"group1", {std::move(noinit)}};
  IR::Ruleset ruleset{"ruleset1", {std::move(dgroup)}, {std::move(reg)}};
  root.rulesets.emplace_back(std::move(ruleset));

  auto engine = compile();
  EXPECT_FALSE(engine);
}

TEST_F(CompilerTest, SilenceLogsParse) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action cont_act;
  cont_act.name = "Continue";
  IR::DetectorGroup dgroup{.name = "group1", .detectors = {cont}};
  IR::Ruleset ruleset{
      .name = "ruleset1",
      .dgs = {std::move(dgroup)},
      .acts = {cont_act},
      .silence_logs = "engine,plugins",
  };
  root.rulesets.emplace_back(std::move(ruleset));

  auto engine = compile();
  EXPECT_TRUE(engine);

  root.rulesets[0].silence_logs = "  engine, plugins \n";
  engine = compile();
  EXPECT_TRUE(engine);

  root.rulesets[0].silence_logs = "engine,asdf";
  engine = compile();
  EXPECT_FALSE(engine);
}

TEST_F(DropInCompilerTest, DropInConfig) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action noop;
  noop.name = "Continue";
  IR::DetectorGroup dg{"dg", {cont}};
  IR::Ruleset rs{"rs", {dg}, {noop}, IR::DropIn{.actiongroup_enabled = true}};
  root.rulesets.emplace_back(std::move(rs));

  IR::Ruleset dropin_rs;
  dropin_rs.name = "rs";
  IR::Action increment;
  increment.name = "IncrementCount";
  dropin_rs.acts.emplace_back(std::move(increment));
  dropin_ir.rulesets.emplace_back(std::move(dropin_rs));

  auto engine = compileBase();
  ASSERT_TRUE(engine);
  engine->runOnce(context);
  EXPECT_EQ(count, 0);

  auto dropin = compileDropIn();
  ASSERT_TRUE(dropin.has_value());
  EXPECT_EQ(dropin->rulesets.size(), 1);

  EXPECT_TRUE(engine->addDropInConfig("0", std::move(dropin->rulesets.at(0))));
  engine->runOnce(context);
  EXPECT_EQ(count, 1);
}

TEST_F(DropInCompilerTest, MultipleDropInConfigOrdering) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action increment;
  increment.name = "IncrementCount";
  IR::DetectorGroup dg{"dg", {cont}};
  IR::Ruleset rs{
      "rs", {dg}, {increment}, IR::DropIn{.actiongroup_enabled = true}};
  root.rulesets.emplace_back(std::move(rs));

  // First drop in config
  IR::Ruleset dropin_rs;
  dropin_rs.name = "rs";
  dropin_rs.acts.emplace_back(increment);
  dropin_ir.rulesets.emplace_back(std::move(dropin_rs));

  // Compile the base config
  auto engine = compileBase();
  ASSERT_TRUE(engine);

  // Add the first drop in config in
  auto dropin = compileDropIn();
  ASSERT_TRUE(dropin.has_value());
  EXPECT_EQ(dropin->rulesets.size(), 1);
  EXPECT_TRUE(engine->addDropInConfig("0", std::move(dropin->rulesets.at(0))));

  // Second drop in config
  dropin_rs = {};
  dropin_rs.name = "rs";
  IR::Action store;
  store.name = "StoreCount";
  dropin_rs.acts.emplace_back(std::move(store));
  dropin_ir.rulesets[0] = std::move(dropin_rs);

  // Now add the second drop in config in.
  //
  // We expect this to be run before the previous drop in config
  auto dropin2 = compileDropIn();
  ASSERT_TRUE(dropin2.has_value());
  EXPECT_EQ(dropin2->rulesets.size(), 1);
  EXPECT_TRUE(engine->addDropInConfig("1", std::move(dropin2->rulesets.at(0))));

  engine->runOnce(context);
  EXPECT_EQ(count, 2);
  EXPECT_EQ(stored_count, 0);
}

TEST_F(DropInCompilerTest, DisablesBase) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action increment;
  increment.name = "IncrementCount";
  IR::DetectorGroup dg{"dg", {cont}};
  IR::Ruleset rs{
      "rs",
      {dg},
      {increment},
      IR::DropIn{
          .disable_on_drop_in = true,
          .actiongroup_enabled = true,
      }};
  root.rulesets.emplace_back(std::move(rs));

  IR::Ruleset dropin_rs;
  dropin_rs.name = "rs";
  IR::Action noop;
  noop.name = "Continue";
  dropin_rs.acts.emplace_back(noop);
  dropin_ir.rulesets.emplace_back(std::move(dropin_rs));

  auto engine = compileBase();
  ASSERT_TRUE(engine);
  engine->runOnce(context);
  EXPECT_EQ(count, 1);

  auto dropin = compileDropIn();
  ASSERT_TRUE(dropin.has_value());
  EXPECT_EQ(dropin->rulesets.size(), 1);

  EXPECT_TRUE(engine->addDropInConfig("0", std::move(dropin->rulesets.at(0))));
  engine->runOnce(context);
  EXPECT_EQ(count, 1);
}

TEST_F(DropInCompilerTest, PermissionDenied) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action increment;
  increment.name = "IncrementCount";
  IR::DetectorGroup dg{"dg", {cont}};
  IR::Ruleset rs{
      "rs",
      {dg},
      {increment},
      IR::DropIn{
          .disable_on_drop_in = false,
          .detectorgroups_enabled = false,
          .actiongroup_enabled = false,
      }};
  root.rulesets.emplace_back(std::move(rs));

  IR::Ruleset dropin_rs;
  dropin_rs.name = "rs";
  IR::Action noop;
  noop.name = "Continue";
  dropin_rs.acts.emplace_back(noop);
  dropin_ir.rulesets.emplace_back(std::move(dropin_rs));

  auto engine = compileBase();
  ASSERT_TRUE(engine);

  auto dropin = compileDropIn();
  EXPECT_FALSE(dropin.has_value());
}

TEST_F(DropInCompilerTest, RemoveDropIn) {
  // Base ruleset increments twice
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action increment;
  increment.name = "IncrementCount";
  IR::DetectorGroup dg{"dg", {cont}};
  IR::Ruleset rs{
      "rs",
      {dg},
      {increment, increment},
      IR::DropIn{.actiongroup_enabled = true}};
  root.rulesets.emplace_back(std::move(rs));

  // Drop in ruleset increments once
  IR::Ruleset dropin_rs;
  dropin_rs.name = "rs";
  dropin_rs.acts.emplace_back(increment);
  dropin_ir.rulesets.emplace_back(std::move(dropin_rs));

  auto engine = compileBase();
  ASSERT_TRUE(engine);
  engine->runOnce(context);
  // Only the base
  EXPECT_EQ(count, 2);

  auto dropin = compileDropIn();
  ASSERT_TRUE(dropin.has_value());
  EXPECT_EQ(dropin->rulesets.size(), 1);

  EXPECT_TRUE(engine->addDropInConfig("0", std::move(dropin->rulesets.at(0))));
  engine->runOnce(context);
  // Base and drop in
  EXPECT_EQ(count, 5);

  engine->removeDropInConfig("0");
  engine->runOnce(context);
  // Only the base
  EXPECT_EQ(count, 7);
}

TEST_F(DropInCompilerTest, MultipleRulesetDropin) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action noop;
  noop.name = "Continue";
  IR::DetectorGroup dg{"dg", {cont}};
  IR::Ruleset rs{
      "rs",
      {dg},
      {noop},
      IR::DropIn{
          .disable_on_drop_in = true,
          .actiongroup_enabled = true,
      }};
  IR::Ruleset rs2{
      "rs2",
      {dg},
      {noop},
      IR::DropIn{
          .disable_on_drop_in = true,
          .actiongroup_enabled = true,
      }};
  root.rulesets.emplace_back(std::move(rs));
  root.rulesets.emplace_back(std::move(rs2));

  // Drop-in ruleset 1
  IR::Ruleset dropin_rs;
  dropin_rs.name = "rs";
  IR::Action increment;
  increment.name = "IncrementCount";
  dropin_rs.acts.emplace_back(increment);

  // Drop-in ruleset 2
  IR::Ruleset dropin_rs2;
  dropin_rs2.name = "rs2";
  dropin_rs2.acts.emplace_back(increment);

  dropin_ir.rulesets.emplace_back(std::move(dropin_rs));
  dropin_ir.rulesets.emplace_back(std::move(dropin_rs2));

  auto engine = compileBase();
  ASSERT_TRUE(engine);
  engine->runOnce(context);
  EXPECT_EQ(count, 0);

  auto dropin = compileDropIn();
  ASSERT_TRUE(dropin.has_value());
  EXPECT_EQ(dropin->rulesets.size(), 2);

  EXPECT_TRUE(engine->addDropInConfig("0", std::move(dropin->rulesets.at(0))));
  EXPECT_TRUE(engine->addDropInConfig("0", std::move(dropin->rulesets.at(1))));
  engine->runOnce(context);
  EXPECT_EQ(count, 2);

  // Now see if both are removed
  engine->removeDropInConfig("0");
  engine->runOnce(context);
  EXPECT_EQ(count, 2);
}
