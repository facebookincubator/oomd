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

#include <stdlib.h>

#include <limits>
#include <memory>

#include <json/value.h>

#include "oomd/Config.h"
#include "oomd/OomDetector.h"
#include "oomd/OomKiller.h"
#include "oomd/Oomd.h"
#include "oomd/plugins/Plugins.h"
#include "oomd/shared/Tunables.h"

using namespace Oomd;
using namespace testing;

constexpr auto kConfig_0_1_0 = "oomd/fixtures/oomd_0_1_0.json";
constexpr auto kConfig_0_1_1 = "oomd/fixtures/oomd_0_1_1.json";
constexpr auto kConfig_0_2_0 = "oomd/fixtures/oomd_0_2_0.json";
constexpr auto kConfigTunablesOverride = "oomd/fixtures/oomd_tunables.override";
constexpr auto kConfigTunablesOverrideMissing =
    "oomd/fixtures/asdfasdf.override";

namespace Oomd {
class MockOomd : public Oomd {
 public:
  MOCK_METHOD1(setTunables, void(std::shared_ptr<Tunables>));
  MOCK_METHOD2(
      addCgroup,
      void(std::unique_ptr<OomDetector>, std::unique_ptr<OomKiller>));
};
} // namespace Oomd

class ConfigTest : public ::testing::Test {
 public:
  ConfigTest() {
    Tunables t;
    for (const auto& e : t.knobs) {
      unsetenv(e.env_name.c_str());
    }
  }
};

class ConfigTest_0_1_0 : public ConfigTest {
 public:
  ConfigTest_0_1_0() {
    std::string conf_file(kConfig_0_1_0);
    conf = std::make_unique<Config>(conf_file);
    cgroup = std::make_unique<Json::Value>(conf->parseJson(conf_file));
  }

  std::unique_ptr<Json::Value> cgroup{nullptr};
  std::unique_ptr<Config> conf{nullptr};
};

class ConfigTest_0_1_1 : public ConfigTest {
 public:
  ConfigTest_0_1_1() {
    std::string conf_file(kConfig_0_1_1);
    conf = std::make_unique<Config>(conf_file);
    cgroup = std::make_unique<Json::Value>(conf->parseJson(conf_file));
  }

  std::unique_ptr<Json::Value> cgroup{nullptr};
  std::unique_ptr<Config> conf{nullptr};
};

class ConfigTest_0_2_0 : public ConfigTest {
 public:
  ConfigTest_0_2_0() {
    std::string conf_file(kConfig_0_2_0);
    conf = std::make_unique<Config>(conf_file);
    cgroup = std::make_unique<Json::Value>(conf->parseJson(conf_file));
  }

  std::unique_ptr<Json::Value> cgroup{nullptr};
  std::unique_ptr<Config> conf{nullptr};
};

TEST_F(ConfigTest_0_1_1, CallsOomdMethods) {
  MockOomd oomd;
  EXPECT_CALL(oomd, setTunables(NotNull())).Times(Exactly(1));
  EXPECT_CALL(oomd, addCgroup(NotNull(), NotNull())).Times(Exactly(1));

  conf->apply(oomd);
}

TEST_F(ConfigTest_0_1_1, LoadTunablesEnv) {
  EXPECT_EQ(setenv("OOMD_INTERVAL", "99", 1), 0);
  EXPECT_EQ(setenv("OOMD_POST_KILL_DELAY", "100", 1), 0);
  EXPECT_EQ(setenv("OOMD_THRESHOLD", "101", 1), 0);
  EXPECT_EQ(setenv("OOMD_HIGH_THRESHOLD", "102", 1), 0);
  EXPECT_EQ(setenv("OOMD_HIGH_THRESHOLD_DURATION", "103", 1), 0);
  EXPECT_EQ(setenv("OOMD_LARGER_THAN", "104", 1), 0);
  EXPECT_EQ(setenv("OOMD_GROWTH_ABOVE", "105", 1), 0);
  EXPECT_EQ(setenv("OOMD_AVERAGE_SIZE_DECAY", "106", 1), 0);
  EXPECT_EQ(setenv("OOMD_FAST_FALL_RATIO", "0.99", 1), 0);

  auto tunables = std::make_shared<Tunables>();
  tunables->parseEnvVars();

  EXPECT_EQ(tunables->get<int>(Tunables::Tunable::INTERVAL), 99);
  EXPECT_EQ(tunables->get<int>(Tunables::Tunable::POST_KILL_DELAY), 100);
  EXPECT_EQ(tunables->get<int>(Tunables::Tunable::THRESHOLD), 101);
  EXPECT_EQ(tunables->get<int>(Tunables::Tunable::HIGH_THRESHOLD), 102);
  EXPECT_EQ(
      tunables->get<int>(Tunables::Tunable::HIGH_THRESHOLD_DURATION), 103);
  EXPECT_EQ(tunables->get<int>(Tunables::Tunable::LARGER_THAN), 104);
  EXPECT_EQ(tunables->get<int>(Tunables::Tunable::GROWTH_ABOVE), 105);
  EXPECT_EQ(tunables->get<int>(Tunables::Tunable::AVERAGE_SIZE_DECAY), 106);
  EXPECT_FLOAT_EQ(
      tunables->get<float>(Tunables::Tunable::FAST_FALL_RATIO), 0.99);
}

TEST_F(ConfigTest_0_1_1, LoadTunablesOverride) {
  auto tunables = std::make_shared<Tunables>();

  ASSERT_NE(tunables->get<int>(Tunables::Tunable::INTERVAL), 99);
  ASSERT_NE(
      tunables->get<float>(Tunables::Tunable::FAST_FALL_RATIO),
      0.01f); // Not ASSERT_FLOAT_NE b/c that doesn't exist

  tunables->loadOverrides(kConfigTunablesOverride);

  EXPECT_EQ(tunables->get<int>(Tunables::Tunable::INTERVAL), 99);
  EXPECT_FLOAT_EQ(
      tunables->get<float>(Tunables::Tunable::FAST_FALL_RATIO), 0.01);

  // Now "delete" the override file and see if we fall back to defaults
  tunables->loadOverrides(kConfigTunablesOverrideMissing);

  EXPECT_NE(tunables->get<int>(Tunables::Tunable::INTERVAL), 99);
  EXPECT_NE(
      tunables->get<float>(Tunables::Tunable::FAST_FALL_RATIO),
      0.01f); // Not ASSERT_FLOAT_NE b/c that doesn't exist
}

TEST_F(ConfigTest_0_1_1, ParseKillList) {
  auto kill_list = conf->parseKillList(*cgroup);
  EXPECT_EQ(kill_list->at(0).service, "chef.service");
  EXPECT_EQ(kill_list->at(0).max_usage, 100);
  EXPECT_EQ(kill_list->at(0).kill_pressure, 60);
  EXPECT_EQ(kill_list->at(1).service, "sshd.service");
  EXPECT_EQ(
      kill_list->at(1).max_usage,
      std::numeric_limits<decltype(kill_list->at(1).max_usage)>::max());
  EXPECT_EQ(
      kill_list->at(1).kill_pressure,
      std::numeric_limits<decltype(kill_list->at(1).kill_pressure)>::max());
  EXPECT_EQ(kill_list->at(2).service, "apple.service");
  EXPECT_EQ(kill_list->at(2).max_usage, 0);
  EXPECT_EQ(
      kill_list->at(2).kill_pressure,
      std::numeric_limits<decltype(kill_list->at(1).kill_pressure)>::max());
}

TEST_F(ConfigTest_0_1_1, ParseBasePath) {
  EXPECT_EQ(conf->parseBasePath(*cgroup), "/sys/fs/cgroup/system.slice");
}

TEST_F(ConfigTest_0_1_1, ParseKillerPlugin) {
  std::string path("asdf");
  auto kl = std::make_shared<KillList>();
  auto tunables = std::make_shared<Tunables>();
  PluginArgs args{path, kl, tunables, true};

  auto killer = conf->parseKillerPluginAndFactory(*cgroup, args);

  // Now verify that the returned plugin is the default OomKiller
  OomKiller* ptr = killer.get();
  EXPECT_NE(dynamic_cast<OomKiller*>(ptr), nullptr);
  EXPECT_NE(dynamic_cast<OomKillerNoop*>(ptr), nullptr);
}

TEST_F(ConfigTest_0_1_0, ParseKillList) {
  auto kill_list = conf->parseKillList(*cgroup);
  EXPECT_EQ(kill_list->at(0).service, "chef.service");
  EXPECT_EQ(kill_list->at(0).max_usage, 100);
  EXPECT_EQ(kill_list->at(1).service, "sshd.service");
  EXPECT_EQ(
      kill_list->at(1).max_usage,
      std::numeric_limits<decltype(kill_list->at(1).max_usage)>::max());
}

TEST_F(ConfigTest_0_1_0, ParseKillerPlugin) {
  std::string path("asdf");
  auto kl = std::make_shared<KillList>();
  auto tunables = std::make_shared<Tunables>();
  PluginArgs args{path, kl, tunables, true};

  auto killer = conf->parseKillerPluginAndFactory(*cgroup, args);

  // Now verify that the returned plugin is the default OomKiller
  OomKiller* ptr = killer.get();
  EXPECT_NE(dynamic_cast<OomKiller*>(ptr), nullptr);
  EXPECT_EQ(dynamic_cast<OomKillerNoop*>(ptr), nullptr);
}

/*
 * It's not really necessary to do more than check addCgroup() was called
 * more than once. The rest of the config parsing functionality is already
 * being tested in the other config tests
 */
TEST_F(ConfigTest_0_2_0, AddMultiCgroup) {
  MockOomd oomd;
  EXPECT_CALL(oomd, setTunables(NotNull())).Times(Exactly(2));
  EXPECT_CALL(oomd, addCgroup(NotNull(), NotNull())).Times(Exactly(2));

  conf->apply(oomd);
}
