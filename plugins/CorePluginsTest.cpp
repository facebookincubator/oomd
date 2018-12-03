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

#include "oomd/OomdContext.h"
#include "oomd/PluginRegistry.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/plugins/KillMemoryGrowth.h"

using namespace Oomd;
using namespace testing;

namespace {
std::unique_ptr<Engine::BasePlugin> createPlugin(const std::string& name) {
  return std::unique_ptr<Engine::BasePlugin>(
      Oomd::getPluginRegistry().create(name));
}
} // namespace

TEST(PresureRisingBeyond, DetectsHighMemPressure) {
  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_rising_beyond";
  args["cgroup"] = "high_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  args["fast_fall_ratio"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(PresureRisingBeyond, NoDetectLowMemPressure) {
  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_rising_beyond";
  args["cgroup"] = "low_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  args["fast_fall_ratio"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(PressureAbove, DetectsHighMemPressure) {
  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_above";
  args["cgroup"] = "high_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(PressureAbove, NoDetectLowMemPressure) {
  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_above";
  args["cgroup"] = "low_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(MemoryReclaim, InstantPgscan) {
  auto plugin = createPlugin("memory_reclaim");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["vmstat_location"] = "oomd/fixtures/plugins/memory_reclaim/vmstat";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(SwapFree, LowSwap) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["meminfo_location"] = "oomd/fixtures/plugins/swap_free/meminfo_low";
  args["threshold_pct"] = "20";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(SwapFree, EnoughSwap) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["meminfo_location"] = "oomd/fixtures/plugins/swap_free/meminfo_enough";
  args["threshold_pct"] = "20";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

namespace Oomd {
class KillMemoryGrowthMocked : public KillMemoryGrowth<> {
 public:
  int tryToKillPids(const std::vector<int>& pids) override {
    for (int pid : pids) {
      killed_.emplace_back(pid);
    }

    return pids.size();
  }

  std::vector<int> killed_;
};
} // namespace Oomd

TEST(KillMemoryGrowth, KillsBigCgroup) {
  auto plugin = std::make_shared<KillMemoryGrowthMocked>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_memory_size_or_growth";
  args["cgroup"] = "one_big";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext("one_big/cgroup1", CgroupContext{{}, {}, 60, 60, 0, 0});
  ctx.setCgroupContext("one_big/cgroup2", CgroupContext{{}, {}, 20, 20, 0, 0});
  ctx.setCgroupContext("one_big/cgroup3", CgroupContext{{}, {}, 20, 20, 0, 0});
  ctx.setCgroupContext("sibling/cgroup1", CgroupContext{{}, {}, 20, 20, 0, 0});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed_, Contains(123));
  EXPECT_THAT(plugin->killed_, Contains(456));
  EXPECT_THAT(plugin->killed_, Not(Contains(789)));
  EXPECT_THAT(plugin->killed_, Not(Contains(111)));
  EXPECT_THAT(
      plugin->killed_, Not(Contains(888))); // make sure there's no siblings
}

TEST(KillMemoryGrowth, DoesntKillBigCgroupInDry) {
  auto plugin = std::make_shared<KillMemoryGrowthMocked>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  std::unordered_map<std::string, std::string> args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_memory_size_or_growth";
  args["cgroup"] = "one_big";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext("one_big/cgroup1", CgroupContext{{}, {}, 60, 60, 0, 0});
  ctx.setCgroupContext("one_big/cgroup2", CgroupContext{{}, {}, 20, 20, 0, 0});
  ctx.setCgroupContext("one_big/cgroup3", CgroupContext{{}, {}, 20, 20, 0, 0});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed_.size(), 0);
}
