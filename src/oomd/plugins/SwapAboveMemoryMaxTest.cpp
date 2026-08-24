/*
 * Copyright (C) 2026-present, Meta Platforms, Inc. and affiliates
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

#include <cstdint>
#include <limits>
#include <memory>
#include <optional>
#include <string>

#include "oomd/OomdContext.h"
#include "oomd/PluginRegistry.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/util/Fixture.h"
#include "oomd/util/TestHelper.h"

using namespace Oomd;
using namespace testing;

namespace {

std::unique_ptr<Engine::BasePlugin> createPlugin(const std::string& name) {
  return std::unique_ptr<Engine::BasePlugin>(
      Oomd::getPluginRegistry().create(name));
}

class SwapAboveMemoryMaxTest : public ::testing::Test {
 protected:
  using CgroupData = TestHelper::CgroupData;
  using F = Fixture;

  void SetUp() override {
    tempdir_ = F::mkdtempChecked();
    F::materialize(
        F::makeDir(tempdir_, {F::makeDir("cgroup1"), F::makeDir("cgroup2")}));
  }

  void TearDown() override {
    F::rmrChecked(tempdir_);
  }

  std::unique_ptr<Engine::BasePlugin> createSwapAboveMemoryMax(
      const std::string& threshold_pct,
      const std::string& cgroups = "cgroup1",
      bool debug = false) {
    auto plugin = createPlugin("swap_above_memory_max");
    if (!plugin) {
      ADD_FAILURE() << "swap_above_memory_max is not registered";
      return nullptr;
    }

    Engine::PluginArgs args;
    args["cgroup"] = cgroups;
    args["threshold_pct"] = threshold_pct;
    if (debug) {
      args["debug"] = "true";
    }
    const PluginConstructionContext compile_context(tempdir_);
    if (plugin->init(args, compile_context) != 0) {
      ADD_FAILURE() << "swap_above_memory_max failed to initialize";
      return nullptr;
    }
    return plugin;
  }

  void setSwapAndMax(
      const std::string& cgroup,
      std::optional<int64_t> swap_bytes,
      std::optional<int64_t> memory_max_bytes) {
    CgroupData data;
    data.swap_usage = swap_bytes;
    data.memory_max = memory_max_bytes;
    TestHelper::setCgroupData(ctx_, CgroupPath(tempdir_, cgroup), data);
  }

  std::string tempdir_;
  OomdContext ctx_;
};

TEST_F(SwapAboveMemoryMaxTest, StrictBoundaryAtOneHundredPercent) {
  auto plugin = createSwapAboveMemoryMax("100");
  ASSERT_NE(plugin, nullptr);

  setSwapAndMax("cgroup1", 99, 100);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", 100, 100);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", 101, 100);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(SwapAboveMemoryMaxTest, StrictBoundaryAtTwoHundredPercent) {
  auto plugin = createSwapAboveMemoryMax("200");
  ASSERT_NE(plugin, nullptr);

  setSwapAndMax("cgroup1", 199, 100);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", 200, 100);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", 201, 100);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(SwapAboveMemoryMaxTest, StrictBoundaryAtFractionalByte) {
  auto plugin = createSwapAboveMemoryMax("150");
  ASSERT_NE(plugin, nullptr);

  setSwapAndMax("cgroup1", 4, 3);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", 5, 3);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(SwapAboveMemoryMaxTest, ComparesHugeValuesWithoutOverflow) {
  auto plugin = createSwapAboveMemoryMax("200");
  ASSERT_NE(plugin, nullptr);
  const int64_t memory_max = std::numeric_limits<int64_t>::max() / 3;

  setSwapAndMax("cgroup1", memory_max * 2, memory_max);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", memory_max * 2 + 1, memory_max);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(SwapAboveMemoryMaxTest, RejectsUnlimitedZeroNegativeAndMissingMax) {
  auto plugin = createSwapAboveMemoryMax("1");
  ASSERT_NE(plugin, nullptr);

  setSwapAndMax("cgroup1", 100, std::numeric_limits<int64_t>::max());
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", 100, 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", 100, -1);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", 100, std::nullopt);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(SwapAboveMemoryMaxTest, RejectsMissingAndNegativeSwap) {
  auto plugin = createSwapAboveMemoryMax("100");
  ASSERT_NE(plugin, nullptr);

  setSwapAndMax("cgroup1", std::nullopt, 100);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup1", -1, 100);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(SwapAboveMemoryMaxTest, KeepsOperandsWithinEachCgroup) {
  auto plugin = createSwapAboveMemoryMax("100", "cgroup1,cgroup2");
  ASSERT_NE(plugin, nullptr);

  setSwapAndMax("cgroup1", 300, 1000);
  setSwapAndMax("cgroup2", 1, 1);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  setSwapAndMax("cgroup2", 2, 1);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(SwapAboveMemoryMaxTest, ValidMatchWinsOverInvalidMatch) {
  auto plugin = createSwapAboveMemoryMax("200", "cgroup1,cgroup2");
  ASSERT_NE(plugin, nullptr);

  setSwapAndMax("cgroup1", 201, 100);
  setSwapAndMax("cgroup2", 1000, std::nullopt);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(SwapAboveMemoryMaxTest, DebugLogIncludesOperandsAndFailureReason) {
  auto plugin = createSwapAboveMemoryMax("100", "cgroup1", true);
  ASSERT_NE(plugin, nullptr);
  const auto unlimited_memory_max = std::numeric_limits<int64_t>::max();
  setSwapAndMax("cgroup1", 1000, unlimited_memory_max);

  testing::internal::CaptureStderr();
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  const auto log = testing::internal::GetCapturedStderr();

  EXPECT_THAT(log, HasSubstr("swap_above_memory_max"));
  EXPECT_THAT(log, HasSubstr("cgroup=\"cgroup1\""));
  EXPECT_THAT(log, HasSubstr("swap_bytes=1000"));
  EXPECT_THAT(
      log,
      HasSubstr("memory_max_bytes=" + std::to_string(unlimited_memory_max)));
  EXPECT_THAT(log, HasSubstr("ratio_pct=unavailable"));
  EXPECT_THAT(log, HasSubstr("threshold_pct=100"));
  EXPECT_THAT(log, HasSubstr("reason=unlimited_memory_max"));
}

TEST_F(SwapAboveMemoryMaxTest, DebugLogIncludesRatioForValidOperands) {
  auto plugin = createSwapAboveMemoryMax("100", "cgroup1", true);
  ASSERT_NE(plugin, nullptr);
  setSwapAndMax("cgroup1", 201, 100);

  testing::internal::CaptureStderr();
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
  const auto log = testing::internal::GetCapturedStderr();

  EXPECT_THAT(log, HasSubstr("swap_bytes=201"));
  EXPECT_THAT(log, HasSubstr("memory_max_bytes=100"));
  EXPECT_THAT(log, HasSubstr("ratio_pct=201.000000"));
  EXPECT_THAT(log, HasSubstr("reason=above_threshold"));
}

TEST_F(SwapAboveMemoryMaxTest, ThresholdMustBeANonNegativeInteger) {
  const PluginConstructionContext compile_context(tempdir_);

  for (const auto& threshold :
       {"-1", "1.5", "not_an_integer", "9223372036854775808"}) {
    auto plugin = createPlugin("swap_above_memory_max");
    ASSERT_NE(plugin, nullptr);
    Engine::PluginArgs args{
        {"cgroup", "cgroup1"}, {"threshold_pct", threshold}};
    EXPECT_NE(plugin->init(args, compile_context), 0);
  }
}

TEST_F(SwapAboveMemoryMaxTest, RulesetCgroupUsesStrictBoundary) {
  auto plugin = createPlugin("swap_above_memory_max");
  ASSERT_NE(plugin, nullptr);
  Engine::PluginArgs args{{"ruleset_cgroup", "."}, {"threshold_pct", "100"}};
  const PluginConstructionContext compile_context(tempdir_);
  ASSERT_EQ(plugin->init(args, compile_context), 0);

  ctx_.setRulesetCgroup(CgroupPath(tempdir_, "cgroup1"));
  const auto cgroup_path = CgroupPath(tempdir_, "cgroup1").absolutePath();
  const auto swap_path = cgroup_path + "/memory.swap.current";
  F::writeChecked(swap_path, "100\n");
  F::writeChecked(cgroup_path + "/memory.max", "100\n");
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  F::writeChecked(swap_path, "101\n");
  ctx_.refresh();
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

} // namespace
