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

#include <algorithm>
#include <memory>
#include <unordered_set>

#include "oomd/OomdContext.h"
#include "oomd/PluginRegistry.h"
#include "oomd/engine/BasePlugin.h"
#include "oomd/plugins/BaseKillPlugin.h"
#include "oomd/plugins/KillIOCost.h"
#include "oomd/plugins/KillMemoryGrowth.h"
#include "oomd/plugins/KillPressure.h"
#include "oomd/plugins/KillSwapUsage.h"

using namespace Oomd;
using namespace testing;

namespace {
std::unique_ptr<Engine::BasePlugin> createPlugin(const std::string& name) {
  return std::unique_ptr<Engine::BasePlugin>(
      Oomd::getPluginRegistry().create(name));
}
} // namespace

namespace Oomd {
class BaseKillPluginMock : public BaseKillPlugin {
 public:
  int tryToKillPids(const std::vector<int>& pids) override {
    int ret = 0;
    killed.reserve(pids.size());

    for (int pid : pids) {
      if (killed.find(pid) == killed.end()) {
        killed.emplace(pid);
        ++ret;
      }
    }

    return ret;
  }

  std::unordered_set<int> killed;
};

/*
 * Use this concrete class to test BaseKillPlugin methods
 */
class BaseKillPluginShim : public BaseKillPluginMock {
 public:
  int init(
      Engine::MonitoredResources& /* unused */,
      const Engine::PluginArgs& /* unused */) override {
    return 0;
  }

  Engine::PluginRet run(OomdContext& /* unused */) override {
    return Engine::PluginRet::CONTINUE;
  }

  void removeSiblingCgroupsShim(
      const std::unordered_set<CgroupPath>& our_prefixes,
      std::vector<std::pair<CgroupPath, Oomd::CgroupContext>>& vec) {
    removeSiblingCgroups(our_prefixes, vec);
  }

  bool tryToKillCgroupShim(
      const std::string& cgroup_path,
      bool recursive,
      bool dry) {
    return BaseKillPluginMock::tryToKillCgroup(cgroup_path, recursive, dry);
  }
};
} // namespace Oomd

TEST(AdjustCgroupPlugin, AdjustCgroupMemory) {
  auto plugin = createPlugin("adjust_cgroup");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/cgroup";
  args["cgroup"] = "adjust_cgroup";
  args["memory_scale"] = "1.5";
  args["memory"] = "-8M";
  args["debug"] = "1";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  auto cgroup_path = CgroupPath(args["cgroup_fs"], "adjust_cgroup");
  ctx.setCgroupContext(
      cgroup_path,
      CgroupContext{.current_usage = 64 << 20, .memory_protection = 16 << 20});
  auto& cgroup_ctx = ctx.getCgroupContext(cgroup_path);

  EXPECT_EQ(cgroup_ctx.effective_usage(), (64 << 20) - (16 << 20));
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
  EXPECT_EQ(
      cgroup_ctx.effective_usage(),
      (int64_t)((64 << 20) * 1.5 - (16 << 20) - (8 << 20)));
}

TEST(BaseKillPlugin, TryToKillCgroupKillsNonRecursive) {
  BaseKillPluginShim plugin;
  EXPECT_EQ(
      plugin.tryToKillCgroupShim(
          "oomd/fixtures/plugins/base_kill_plugin/one_big", false, false),
      true);

  int expected_total = 0;
  for (int i = 1; i <= 30; ++i) {
    expected_total += i;
  }

  int received_total = 0;
  for (int i : plugin.killed) {
    received_total += i;
  }

  EXPECT_EQ(expected_total, received_total);
}

TEST(BaseKillPlugin, TryToKillCgroupKillsRecursive) {
  BaseKillPluginShim plugin;
  EXPECT_EQ(
      plugin.tryToKillCgroupShim(
          "oomd/fixtures/plugins/base_kill_plugin/one_big", true, false),
      true);

  int expected_total = 0;
  for (int i = 1; i <= 30; ++i) {
    expected_total += i;
  }
  expected_total += 1234;

  int received_total = 0;
  for (int i : plugin.killed) {
    received_total += i;
  }

  EXPECT_EQ(expected_total, received_total);
}

TEST(BaseKillPlugin, RemoveSiblingCgroups) {
  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath("/", "some/made_up/cgroup/path/here"), CgroupContext{});
  ctx.setCgroupContext(
      CgroupPath("/", "some/other/cgroup/path/here"), CgroupContext{});
  ctx.setCgroupContext(
      CgroupPath("/", "notavalidcgrouppath/here"), CgroupContext{});
  ctx.setCgroupContext(CgroupPath("/", "XXXXXXXX/here"), CgroupContext{});
  auto vec = ctx.reverseSort();

  auto plugin = std::make_shared<BaseKillPluginShim>();
  ASSERT_NE(plugin, nullptr);

  // Test wildcard support first
  plugin->removeSiblingCgroupsShim(
      {CgroupPath("/", "some/*/cgroup/path/*")}, vec);
  ASSERT_EQ(vec.size(), 2);
  EXPECT_TRUE(std::any_of(vec.begin(), vec.end(), [&](const auto& pair) {
    return pair.first.relativePath() == "some/made_up/cgroup/path/here";
  }));
  EXPECT_TRUE(std::any_of(vec.begin(), vec.end(), [&](const auto& pair) {
    return pair.first.relativePath() == "some/other/cgroup/path/here";
  }));

  // Now test non-wildcard
  plugin->removeSiblingCgroupsShim(
      {CgroupPath("/", "some/other/cgroup/path/*")}, vec);
  ASSERT_EQ(vec.size(), 1);
  EXPECT_EQ(vec[0].first.relativePath(), "some/other/cgroup/path/here");
}

TEST(BaseKillPlugin, RemoveSiblingCgroupsMultiple) {
  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath("/", "some/made_up/cgroup/path/here"), CgroupContext{});
  ctx.setCgroupContext(
      CgroupPath("/", "some/other/cgroup/path/here"), CgroupContext{});
  ctx.setCgroupContext(
      CgroupPath("/", "notavalidcgrouppath/here"), CgroupContext{});
  ctx.setCgroupContext(CgroupPath("/", "XXXXXXXX/here"), CgroupContext{});
  auto vec = ctx.reverseSort();

  auto plugin = std::make_shared<BaseKillPluginShim>();
  ASSERT_NE(plugin, nullptr);

  plugin->removeSiblingCgroupsShim(
      {CgroupPath("/", "some/made_up/cgroup/path/*"),
       CgroupPath("/", "some/other/cgroup/path/*")},
      vec);
  ASSERT_EQ(vec.size(), 2);
  EXPECT_TRUE(std::any_of(vec.begin(), vec.end(), [&](const auto& pair) {
    return pair.first.relativePath() == "some/made_up/cgroup/path/here";
  }));
  EXPECT_TRUE(std::any_of(vec.begin(), vec.end(), [&](const auto& pair) {
    return pair.first.relativePath() == "some/other/cgroup/path/here";
  }));
}

TEST(PresureRisingBeyond, DetectsHighMemPressure) {
  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
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
  Engine::PluginArgs args;
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

TEST(PresureRisingBeyond, DetectsHighMemPressureMultiCgroup) {
  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_rising_beyond";
  args["cgroup"] = "low_pressure,high_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  args["fast_fall_ratio"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(PresureRisingBeyond, DetectsHighMemPressureWildcard) {
  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_rising_beyond";
  args["cgroup"] = "*_*";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  args["fast_fall_ratio"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(PressureAbove, DetectsHighMemPressure) {
  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
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
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_above";
  args["cgroup"] = "low_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(PressureAbove, DetectsHighMemPressureMultiCgroup) {
  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_above";
  args["cgroup"] = "high_pressure,low_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(PressureAbove, DetectsHighMemPressureWildcard) {
  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/pressure_above";
  args["cgroup"] = "*";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(MemoryAbove, DetectsHighMemUsage) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(MemoryAbove, NoDetectLowMemUsage) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{.current_usage = 1073741824});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(MemoryAbove, DetectsHighMemUsageCompat) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536"; // Should be interpreted as MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(MemoryAbove, NoDetectLowMemUsageCompat) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536"; // Should be interpreted as MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{.current_usage = 1073741824});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(MemoryAbove, DetectsHighMemUsagePercent) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "10%";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(MemoryAbove, NoDetectLowMemUsageMultiple) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536M";
  args["duration"] = "0";
  args["debug"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{.current_usage = 1073741824});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(MemoryAbove, DetectsHighMemUsageMultiple) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536M";
  args["duration"] = "0";
  args["debug"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{.current_usage = 1073741824});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(MemoryAbove, NoDetectLowMemUsagePercent) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "80%";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{.current_usage = 1073741824});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(MemoryAbove, DetectsHighAnonUsage) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold_anon"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{
          .swap_usage = 20,
          .anon_usage = 2147483648,
      });
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(MemoryAbove, NoDetectLowAnonUsage) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold_anon"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{
          .swap_usage = 20,
          .anon_usage = 1073741824,
      });
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(MemoryAbove, DetectsHighAnonUsageIgnoreLowMemUsage) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold_anon"] = "1536M";
  args["threshold"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{
          .current_usage = 1073741824,
          .swap_usage = 20,
          .anon_usage = 2147483648,
      });
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(MemoryAbove, NoDetectLowAnonUsageIgnoreHighMemUsage) {
  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_above";
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold_anon"] = "1536M";
  args["threshold"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{
          .current_usage = 2147483648,
          .swap_usage = 20,
          .anon_usage = 1073741824,
      });
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(MemoryReclaim, SingleCgroupReclaimSuccess) {
  auto plugin = createPlugin("memory_reclaim");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_reclaim/single_cgroup";
  args["cgroup"] = "cgroup1";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(CgroupPath(args["cgroup_fs"], "cgroup1"), {});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(MemoryReclaim, MultiCgroupReclaimSuccess) {
  auto plugin = createPlugin("memory_reclaim");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/memory_reclaim/multi_cgroup";
  args["cgroup"] = "cgroup1,cgroup2";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(CgroupPath(args["cgroup_fs"], "cgroup1"), {});
  ctx.setCgroupContext(CgroupPath(args["cgroup_fs"], "cgroup2"), {});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(SwapFree, LowSwap) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["threshold_pct"] = "20";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  SystemContext system_ctx;
  system_ctx.swaptotal = static_cast<uint64_t>(20971512) * 1024;
  system_ctx.swapused = static_cast<uint64_t>(20971440) * 1024;
  ctx.setSystemContext(system_ctx);
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(SwapFree, EnoughSwap) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["threshold_pct"] = "20";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  SystemContext system_ctx;
  system_ctx.swaptotal = static_cast<uint64_t>(20971512) * 1024;
  system_ctx.swapused = static_cast<uint64_t>(3310136) * 1024;
  ctx.setSystemContext(system_ctx);
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(SwapFree, SwapOff) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["threshold_pct"] = "20";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(Exists, Exists) {
  auto plugin = createPlugin("exists");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/cgroup";
  args["cgroup"] = "cgroup_A,cgroup_B,cgroup_C";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  auto cgroup_path_C = CgroupPath(args["cgroup_fs"], "cgroup_C");
  auto cgroup_path_D = CgroupPath(args["cgroup_fs"], "cgroup_D");

  ctx.setCgroupContext(cgroup_path_D, CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);

  ctx.setCgroupContext(cgroup_path_C, CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(Exists, NotExists) {
  auto plugin = createPlugin("exists");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/cgroup";
  args["cgroup"] = "cgroup_A,cgroup_B,cgroup_C";
  args["negate"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  auto cgroup_path_C = CgroupPath(args["cgroup_fs"], "cgroup_C");
  auto cgroup_path_D = CgroupPath(args["cgroup_fs"], "cgroup_D");

  ctx.setCgroupContext(cgroup_path_D, CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);

  ctx.setCgroupContext(cgroup_path_C, CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(KillIOCost, KillsHighestIOCost) {
  auto plugin = std::make_shared<KillIOCost<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_io_cost";
  args["cgroup"] = "one_high/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup1"),
      CgroupContext{.io_cost_cumulative = 10000, .io_cost_rate = 10});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{.io_cost_cumulative = 5000, .io_cost_rate = 30});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{.io_cost_cumulative = 6000, .io_cost_rate = 50});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{.io_cost_cumulative = 20000, .io_cost_rate = 100});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(111));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
}

TEST(KillIOCost, KillsHighestIOCostMultiCgroup) {
  auto plugin = std::make_shared<KillIOCost<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_io_cost";
  args["cgroup"] = "one_high/*,sibling/*";
  args["resource"] = "io";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup1"),
      CgroupContext{.io_cost_cumulative = 10000, .io_cost_rate = 10});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{.io_cost_cumulative = 5000, .io_cost_rate = 30});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{.io_cost_cumulative = 6000, .io_cost_rate = 50});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{.io_cost_cumulative = 20000, .io_cost_rate = 100});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(888));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
}

TEST(KillIOCost, DoesntKillsHighestIOCostDry) {
  auto plugin = std::make_shared<KillIOCost<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_pressure";
  args["cgroup"] = "one_high/*";
  args["resource"] = "io";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup1"),
      CgroupContext{.io_cost_cumulative = 10000, .io_cost_rate = 10});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{.io_cost_cumulative = 5000, .io_cost_rate = 30});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{.io_cost_cumulative = 6000, .io_cost_rate = 50});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{.io_cost_cumulative = 20000, .io_cost_rate = 100});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}

TEST(Exists, ExistsWildcard) {
  auto plugin = createPlugin("exists");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/cgroup";
  args["cgroup"] = "cgroup_PREFIX*";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  auto cgroup_path_notok = CgroupPath(args["cgroup_fs"], "cgroup_SOMETHING");
  auto cgroup_path_ok = CgroupPath(args["cgroup_fs"], "cgroup_PREFIXhere");

  ctx.setCgroupContext(cgroup_path_notok, CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);

  ctx.setCgroupContext(cgroup_path_ok, CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(Exists, NotExistsWildcard) {
  auto plugin = createPlugin("exists");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/cgroup";
  args["cgroup"] = "cgroup_PREFIX*";
  args["negate"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  auto cgroup_path_notok = CgroupPath(args["cgroup_fs"], "cgroup_SOMETHING");
  auto cgroup_path_ok = CgroupPath(args["cgroup_fs"], "cgroup_PREFIXhere");

  ctx.setCgroupContext(cgroup_path_notok, CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);

  ctx.setCgroupContext(cgroup_path_ok, CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
}

TEST(KillMemoryGrowth, KillsBigCgroup) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_memory_size_or_growth";
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup1"),
      CgroupContext{
          .current_usage = 60,
          .average_usage = 60,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(123));
  EXPECT_THAT(plugin->killed, Contains(456));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(
      plugin->killed, Not(Contains(888))); // make sure there's no siblings
}

TEST(KillMemoryGrowth, KillsBigCgroupGrowth) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_memory_size_or_growth";
  args["cgroup"] = "growth_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;

  // First test that we do the last ditch size killing.
  //
  // cgroup3 should be killed even though (30 / (21+20+30) < .5)
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "growth_big/cgroup1"),
      CgroupContext{
          .current_usage = 21,
          .average_usage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "growth_big/cgroup2"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "growth_big/cgroup3"),
      CgroupContext{
          .current_usage = 30,
          .average_usage = 30,
      });
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(111));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));

  // Now lower average usage to artificially "boost" growth rate to trigger
  // growth kill
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "growth_big/cgroup1"),
      CgroupContext{
          .current_usage = 21,
          .average_usage = 5,
      });

  // Do the same thing for a sibling cgroup, but set the growth higher. This
  // tests that sibling removal occurs for growth kills too.
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{
          .current_usage = 99,
          .average_usage = 5,
      });

  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
  EXPECT_THAT(plugin->killed, Contains(123));
  EXPECT_THAT(plugin->killed, Contains(456));
}

TEST(KillMemoryGrowth, KillsBigCgroupMultiCgroup) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_memory_size_or_growth";
  args["cgroup"] = "one_big/*,sibling/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup1"),
      CgroupContext{
          .current_usage = 60,
          .average_usage = 60,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{
          .current_usage = 100,
          .average_usage = 100,
      });
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(888));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
}

TEST(KillMemoryGrowth, DoesntKillBigCgroupInDry) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_memory_size_or_growth";
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup1"),
      CgroupContext{
          .current_usage = 60,
          .average_usage = 60,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}

TEST(KillSwapUsage, KillsBigSwapCgroup) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_swap_usage";
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup1"),
      CgroupContext{.swap_usage = 20});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{.swap_usage = 60});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{.swap_usage = 40});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(789));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
}

TEST(KillSwapUsage, KillsBigSwapCgroupMultiCgroup) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_swap_usage";
  args["cgroup"] = "one_big/*,sibling/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup1"),
      CgroupContext{.swap_usage = 20});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{.swap_usage = 60});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{.swap_usage = 40});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{.swap_usage = 70});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(555));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
}

TEST(KillSwapUsage, DoesntKillBigSwapCgroupDry) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_swap_usage";
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup1"),
      CgroupContext{.swap_usage = 20});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{.swap_usage = 60});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{.swap_usage = 40});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}

TEST(KillSwapUsage, DoesntKillNoSwap) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_swap_usage";
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup1"), CgroupContext{});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"), CgroupContext{});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"), CgroupContext{});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
  EXPECT_EQ(plugin->killed.size(), 0);
}

TEST(KillPressure, KillsHighestPressure) {
  auto plugin = std::make_shared<KillPressure<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_pressure";
  args["cgroup"] = "one_high/*";
  args["resource"] = "io";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup1"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 60,
                        .sec_60 = 60,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 50,
                        .sec_60 = 70,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 80,
                        .sec_60 = 80,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 99,
                        .sec_60 = 99,
                        .sec_600 = 99,
                    }});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(111));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
}

TEST(KillPressure, KillsHighestPressureMultiCgroup) {
  auto plugin = std::make_shared<KillPressure<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_pressure";
  args["cgroup"] = "one_high/*,sibling/*";
  args["resource"] = "io";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup1"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 60,
                        .sec_60 = 60,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 50,
                        .sec_60 = 70,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 80,
                        .sec_60 = 80,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 99,
                        .sec_60 = 99,
                        .sec_600 = 99,
                    }});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(888));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
}

TEST(KillPressure, DoesntKillsHighestPressureDry) {
  auto plugin = std::make_shared<KillPressure<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["cgroup_fs"] = "oomd/fixtures/plugins/kill_by_pressure";
  args["cgroup"] = "one_high/*";
  args["resource"] = "io";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup1"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 60,
                        .sec_60 = 60,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 50,
                        .sec_60 = 70,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 80,
                        .sec_60 = 80,
                    }});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{.io_pressure = ResourcePressure{
                        .sec_10 = 99,
                        .sec_60 = 99,
                        .sec_600 = 99,
                    }});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}
