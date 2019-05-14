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

using namespace Oomd;
using namespace Oomd::Config2;
using namespace Oomd::Engine;
using namespace testing;

namespace {
int count;
int stored_count;
} // namespace

static constexpr auto kRandomCgroupFs = "/some/random/fs";
static constexpr auto kRandomCgroupDependency = "/some/random/cgroup";

namespace Oomd {

class ContinuePlugin : public BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& /* unused */,
      const PluginArgs& /* unused */) override {
    return 0;
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
      Engine::MonitoredResources& /* unused */,
      const PluginArgs& /* unused */) override {
    return 0;
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
      Engine::MonitoredResources& /* unused */,
      const PluginArgs& /* unused */) override {
    return 0;
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
      Engine::MonitoredResources& /* unused */,
      const PluginArgs& /* unused */) override {
    return 0;
  }

  PluginRet run(OomdContext& /* unused */) override {
    stored_count = count;
    return PluginRet::CONTINUE;
  }

  static StoreCountPlugin* create() {
    return new StoreCountPlugin();
  }

  ~StoreCountPlugin() override = default;
};

class RegistrationPlugin : public BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& resources,
      const PluginArgs& /* unused */) override {
    resources.emplace(kRandomCgroupFs, kRandomCgroupDependency);
    return 0;
  }

  PluginRet run(OomdContext& /* unused */) override {
    return PluginRet::CONTINUE;
  }

  static RegistrationPlugin* create() {
    return new RegistrationPlugin();
  }

  ~RegistrationPlugin() override = default;
};

class NoInitPlugin : public BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& /* unused */,
      const PluginArgs& /* unused */) override {
    return 1;
  }

  PluginRet run(OomdContext& /* unused */) override {
    return PluginRet::CONTINUE;
  }

  static NoInitPlugin* create() {
    return new NoInitPlugin();
  }

  ~NoInitPlugin() override = default;
};

REGISTER_PLUGIN(Continue, ContinuePlugin::create);
REGISTER_PLUGIN(Stop, StopPlugin::create);
REGISTER_PLUGIN(IncrementCount, IncrementCountPlugin::create);
REGISTER_PLUGIN(StoreCount, StoreCountPlugin::create);
REGISTER_PLUGIN(Registration, RegistrationPlugin::create);
REGISTER_PLUGIN(NoInit, NoInitPlugin::create);

} // namespace Oomd

class CompilerTest : public ::testing::Test {
 public:
  CompilerTest() {
    count = 0;
    stored_count = 0;
  }

  std::unique_ptr<::Oomd::Engine::Engine> compile() {
    return Config2::compile(root);
  }

  OomdContext context;
  IR::Root root;
};

class DropInCompilerTest : public ::testing::Test {
 public:
  DropInCompilerTest() {
    count = 0;
    stored_count = 0;
  }

  std::unique_ptr<::Oomd::Engine::Engine> compileBase() {
    return compile(root);
  }

  std::optional<DropInUnit> compileDropIn() {
    return ::Oomd::Config2::compileDropIn(root, dropin_ir);
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

TEST_F(CompilerTest, MonitoredResources) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action reg;
  reg.name = "Registration";
  IR::DetectorGroup dgroup{"group1", {std::move(cont)}};
  IR::Ruleset ruleset{"ruleset1", {std::move(dgroup)}, {std::move(reg)}};
  root.rulesets.emplace_back(std::move(ruleset));

  auto engine = compile();
  ASSERT_TRUE(engine);
  EXPECT_THAT(
      engine->getMonitoredResources(),
      Contains(CgroupPath(kRandomCgroupFs, kRandomCgroupDependency)));
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

  EXPECT_TRUE(engine->addDropInConfig(0, std::move(dropin->rulesets.at(0))));
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
  EXPECT_TRUE(engine->addDropInConfig(0, std::move(dropin->rulesets.at(0))));

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
  EXPECT_TRUE(engine->addDropInConfig(1, std::move(dropin2->rulesets.at(0))));

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
      IR::DropIn{.actiongroup_enabled = true, .disable_on_drop_in = true}};
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

  EXPECT_TRUE(engine->addDropInConfig(0, std::move(dropin->rulesets.at(0))));
  engine->runOnce(context);
  EXPECT_EQ(count, 1);
}

TEST_F(DropInCompilerTest, PermissionDenied) {
  IR::Detector cont;
  cont.name = "Continue";
  IR::Action increment;
  increment.name = "IncrementCount";
  IR::DetectorGroup dg{"dg", {cont}};
  IR::Ruleset rs{"rs",
                 {dg},
                 {increment},
                 IR::DropIn{.detectorgroups_enabled = false,
                            .actiongroup_enabled = false,
                            .disable_on_drop_in = false}};
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
  IR::Ruleset rs{"rs",
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

  EXPECT_TRUE(engine->addDropInConfig(0, std::move(dropin->rulesets.at(0))));
  engine->runOnce(context);
  // Base and drop in
  EXPECT_EQ(count, 5);

  engine->removeDropInConfig(0);
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
      IR::DropIn{.actiongroup_enabled = true, .disable_on_drop_in = true}};
  IR::Ruleset rs2{
      "rs2",
      {dg},
      {noop},
      IR::DropIn{.actiongroup_enabled = true, .disable_on_drop_in = true}};
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

  EXPECT_TRUE(engine->addDropInConfig(0, std::move(dropin->rulesets.at(0))));
  EXPECT_TRUE(engine->addDropInConfig(0, std::move(dropin->rulesets.at(1))));
  engine->runOnce(context);
  EXPECT_EQ(count, 2);

  // Now see if both are removed
  engine->removeDropInConfig(0);
  engine->runOnce(context);
  EXPECT_EQ(count, 2);
}
