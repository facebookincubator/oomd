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
} // namespace

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

class RegistrationPlugin : public BasePlugin {
 public:
  int init(
      Engine::MonitoredResources& resources,
      const PluginArgs& /* unused */) override {
    resources.emplace(kRandomCgroupDependency);
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
REGISTER_PLUGIN(Registration, RegistrationPlugin::create);
REGISTER_PLUGIN(NoInit, NoInitPlugin::create);

} // namespace Oomd

class CompilerTest : public ::testing::Test {
 public:
  CompilerTest() {
    count = 0;
    root.version = "1.0.0";
  }

  std::unique_ptr<::Oomd::Engine::Engine> compile() {
    return Config2::compile(root);
  }

  OomdContext context;
  IR::Root root;
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
      engine->getMonitoredResources(), Contains(kRandomCgroupDependency));
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
