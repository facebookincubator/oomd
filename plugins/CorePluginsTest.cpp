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
      CgroupPath("/", "some/made_up/cgroup/path/here"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath("/", "some/other/cgroup/path/here"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath("/", "notavalidcgrouppath/here"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath("/", "XXXXXXXX/here"), CgroupContext{{}, {}, 0, 0, 0, 0});
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
      CgroupPath("/", "some/made_up/cgroup/path/here"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath("/", "some/other/cgroup/path/here"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath("/", "notavalidcgrouppath/here"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath("/", "XXXXXXXX/here"), CgroupContext{{}, {}, 0, 0, 0, 0});
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
  args["threshold"] = "1536"; // in MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{{}, {}, 2147483648});
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
  args["threshold"] = "1536"; // in MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{{}, {}, 1073741824});
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
  args["threshold"] = "10%"; // in MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{{}, {}, 2147483648});
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
  args["threshold"] = "1536"; // in MB
  args["duration"] = "0";
  args["debug"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{{}, {}, 1073741824});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{{}, {}, 2147483648});
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
  args["threshold"] = "1536"; // in MB
  args["duration"] = "0";
  args["debug"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{{}, {}, 1073741824});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{{}, {}, 2147483648});
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
  args["threshold"] = "80%"; // in MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{{}, {}, 1073741824});
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
  args["threshold_anon"] = "1536"; // in MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{{}, {}, 0, 0, 0, 20, 2147483648});
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
  args["threshold_anon"] = "1536"; // in MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{{}, {}, 0, 0, 0, 20, 1073741824});
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
  args["threshold_anon"] = "1536"; // in MB
  args["threshold"] = "1536"; // in MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "high_memory"),
      CgroupContext{{}, {}, 1073741824, 0, 0, 20, 2147483648});
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
  args["threshold_anon"] = "1536"; // in MB
  args["threshold"] = "1536"; // in MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "low_memory"),
      CgroupContext{{}, {}, 2147483648, 0, 0, 20, 1073741824});
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
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::CONTINUE);
}

TEST(SwapFree, LowSwap) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
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
  Engine::PluginArgs args;
  args["meminfo_location"] = "oomd/fixtures/plugins/swap_free/meminfo_enough";
  args["threshold_pct"] = "20";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
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
          .protection_overage = 60,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
          .protection_overage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
          .protection_overage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
          .protection_overage = 20,
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
      CgroupContext{{}, {}, 21, 20, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "growth_big/cgroup2"),
      CgroupContext{{}, {}, 20, 20, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "growth_big/cgroup3"),
      CgroupContext{{}, {}, 30, 30, 0, 0});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(111));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));

  // Now lower average usage to artificially "boost" growth rate to trigger
  // growth kill
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "growth_big/cgroup1"),
      CgroupContext{{}, {}, 21, 5, 0, 0});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
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
          .protection_overage = 60,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
          .protection_overage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{
          .current_usage = 20,
          .average_usage = 20,
          .protection_overage = 20,
      });
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{
          .current_usage = 100,
          .average_usage = 100,
          .protection_overage = 100,
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
      CgroupContext{{}, {}, 60, 60, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{{}, {}, 20, 20, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{{}, {}, 20, 20, 0, 0});
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
      CgroupContext{{}, {}, 0, 0, 0, 20});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{{}, {}, 0, 0, 0, 60});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{{}, {}, 0, 0, 0, 40});
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
      CgroupContext{{}, {}, 0, 0, 0, 20});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{{}, {}, 0, 0, 0, 60});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{{}, {}, 0, 0, 0, 40});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{{}, {}, 0, 0, 0, 70});
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
      CgroupContext{{}, {}, 0, 0, 0, 20});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{{}, {}, 0, 0, 0, 60});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{{}, {}, 0, 0, 0, 40});
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
      CgroupPath(args["cgroup_fs"], "one_big/cgroup1"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup2"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_big/cgroup3"),
      CgroupContext{{}, {}, 0, 0, 0, 0});
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
      CgroupContext{{}, {ResourcePressure{60, 60, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{{}, {ResourcePressure{50, 70, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{{}, {ResourcePressure{80, 80, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{{}, {ResourcePressure{99, 99, 99}}, 0, 0, 0, 0});
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
      CgroupContext{{}, {ResourcePressure{60, 60, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{{}, {ResourcePressure{50, 70, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{{}, {ResourcePressure{80, 80, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{{}, {ResourcePressure{99, 99, 99}}, 0, 0, 0, 0});
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
      CgroupContext{{}, {ResourcePressure{60, 60, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup2"),
      CgroupContext{{}, {ResourcePressure{50, 70, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "one_high/cgroup3"),
      CgroupContext{{}, {ResourcePressure{80, 80, 0}}, 0, 0, 0, 0});
  ctx.setCgroupContext(
      CgroupPath(args["cgroup_fs"], "sibling/cgroup1"),
      CgroupContext{{}, {ResourcePressure{99, 99, 99}}, 0, 0, 0, 0});
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}
