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

#include <fcntl.h>
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
#include "oomd/plugins/KillPgScan.h"
#include "oomd/plugins/KillPressure.h"
#include "oomd/plugins/KillSwapUsage.h"
#include "oomd/util/Fixture.h"
#include "oomd/util/Fs.h"
#include "oomd/util/TestHelper.h"

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
  /*
   * Since we are just simulating killing, we don't need to freeze the cgroup
   */
  int freezeCgroup(const CgroupContext& target) override {
    return 0;
  }

  /*
   * We don't actually need to dump memory.stat since we won't
   * actually be killing any live processes
   */
  int dumpMemoryStat(const CgroupContext& target) override {
    return 0;
  }

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

  int tryToKillCgroup(
      const CgroupContext& target,
      const KillUuid& kill_uuid,
      bool dry) override {
    if (unkillable_cgroups.count(target.cgroup().absolutePath()) > 0) {
      OLOG << "tried to kill " << target.cgroup().absolutePath()
           << ", failed b/c it's in unkillable_cgroups";
      return false;
    }

    OLOG << "killed " << target.cgroup().absolutePath();
    killed_cgroup = target.cgroup().absolutePath();

    return BaseKillPlugin::tryToKillCgroup(target, kill_uuid, dry);
  }

  std::optional<std::string> killed_cgroup{std::nullopt};
  std::unordered_set<std::string> unkillable_cgroups;
  std::unordered_set<int> killed;
};

/*
 * Use this concrete class to test BaseKillPlugin methods
 */
class BaseKillPluginShim : public BaseKillPluginMock {
 public:
  int init(
      const Engine::PluginArgs& /* unused */,
      const PluginConstructionContext& context) override {
    return 0;
  }

  Engine::PluginRet run(OomdContext& /* unused */) override {
    return Engine::PluginRet::CONTINUE;
  }

  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& /* unused */,
      const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */)
      override {
    return std::vector<OomdContext::ConstCgroupContextRef>{};
  }

  void ologKillTarget(
      OomdContext& /* unused */,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */)
      override {
    OLOG << "Picked \"" << target.cgroup().relativePath() << "\"";
  }
};
} // namespace Oomd

class CorePluginsTest : public ::testing::Test {
 protected:
  using CgroupData = TestHelper::CgroupData;
  using memory_stat_t = decltype(CgroupData::memory_stat)::value_type;
  using F = Fixture;
  void SetUp() override {
    tempdir_ = F::mkdtempChecked();
  }
  void TearDown() override {
    F::rmrChecked(tempdir_);
  }
  std::string tempdir_;
  OomdContext ctx_;
};

class BaseKillPluginTest : public CorePluginsTest {};

TEST_F(BaseKillPluginTest, TryToKillCgroupKillsRecursive) {
  auto target = ASSERT_EXISTS(CgroupContext::make(
      ctx_, CgroupPath("oomd/fixtures/plugins/base_kill_plugin", "one_big")));

  BaseKillPluginShim plugin;
  EXPECT_EQ(plugin.tryToKillCgroup(target, "fake_kill_uuid", false), 31);

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

class BaseKillPluginXattrTest : public ::testing::Test,
                                public BaseKillPluginShim {
 protected:
  std::string getxattr(const std::string& path, const std::string& attr)
      override {
    return xattrs_[path][attr];
  }

  bool setxattr(
      const std::string& path,
      const std::string& attr,
      const std::string& val) override {
    xattrs_[path][attr] = val;
    return true;
  }

 private:
  std::unordered_map<std::string, std::unordered_map<std::string, std::string>>
      xattrs_;
};

TEST_F(BaseKillPluginXattrTest, XattrSetts) {
  auto cgroup_path = "/sys/fs/cgroup/test/test";

  static auto constexpr kOomdKillInitiationTrustedXattr = "trusted.oomd_ooms";
  static auto constexpr kOomdKillInitiationUserXattr = "user.oomd_ooms";
  static auto constexpr kOomdKillCompletionTrustedXattr = "trusted.oomd_kill";
  static auto constexpr kOomdKillCompletionUserXattr = "user.oomd_kill";
  static auto constexpr kOomdKillUuidTrustedXattr = "trusted.oomd_kill_uuid";
  static auto constexpr kOomdKillUuidUserXattr = "user.oomd_kill_uuid";

  static auto constexpr kKillUuid1 = "8c774f00-8202-4893-a58d-74bd1515660e";
  static auto constexpr kKillUuid2 = "9c774f00-8202-4893-a58d-74bd1515660e";

  // Kill Initiation increments on each kill
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillInitiationTrustedXattr), "");
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillInitiationUserXattr), "");
  reportKillInitiationToXattr(cgroup_path);
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillInitiationTrustedXattr), "1");
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillInitiationUserXattr), "1");
  reportKillInitiationToXattr(cgroup_path);
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillInitiationTrustedXattr), "2");
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillInitiationUserXattr), "2");

  // Kill Completion sums up for each kill
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillCompletionTrustedXattr), "");
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillCompletionUserXattr), "");
  reportKillCompletionToXattr(cgroup_path, 10);
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillCompletionTrustedXattr), "10");
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillCompletionUserXattr), "10");
  reportKillCompletionToXattr(cgroup_path, 10);
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillCompletionTrustedXattr), "20");
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillCompletionUserXattr), "20");

  // Kill Uuid resets on each kill
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillUuidTrustedXattr), "");
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillUuidUserXattr), "");
  reportKillUuidToXattr(cgroup_path, kKillUuid1);
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillUuidTrustedXattr), kKillUuid1);
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillUuidUserXattr), kKillUuid1);
  reportKillUuidToXattr(cgroup_path, kKillUuid2);
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillUuidTrustedXattr), kKillUuid2);
  EXPECT_EQ(getxattr(cgroup_path, kOomdKillUuidUserXattr), kKillUuid2);
}

// Test BaseKillPlugin's cgroup traversal with a subclass that chooses
// cgroups the simplest way: by name, alphabetically (descending).

class AlphabeticStandardKillPlugin : public BaseKillPluginMock {
 public:
  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& /* unused */,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) override {
    return OomdContext::sortDescWithKillPrefs(
        cgroups, [](const CgroupContext& cgroup_ctx) {
          return cgroup_ctx.cgroup().relativePathParts().back();
        });
  }

  void ologKillTarget(
      OomdContext& /* unused */,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */)
      override {
    OLOG << "Picked \"" << target.cgroup().relativePath() << "\"";
  }
};

class StandardKillRecursionTest : public CorePluginsTest {};

TEST_F(StandardKillRecursionTest, Recurses) {
  F::materialize(F::makeDir(
      tempdir_,
      {Fixture::makeDir(
           "A",
           {
               // Note "Z" and "X" are higher than the "F" in "A" below.
               // This tests that decisions are made locally: first choose B
               // from root's {A, B}, then choose F from B's {F}. If leaves
               // are compared across subtrees Z will win and break the
               // test.
               Fixture::makeDir("Z", {}),
               Fixture::makeDir("X", {}),
           }),
       Fixture::makeDir(
           "B",
           {
               Fixture::makeDir("F", {}),
           })}));

  auto plugin = std::make_shared<AlphabeticStandardKillPlugin>();
  ASSERT_NE(plugin, nullptr);
  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["recursive"] = "true";
  args["post_action_delay"] = "0";
  args["dry"] = "true";
  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_EQ(*plugin->killed_cgroup, CgroupPath(tempdir_, "B/F").absolutePath());
}

TEST_F(StandardKillRecursionTest, ConfigurableToNotRecurse) {
  // Same as StandardKillRecursionTest.Recurses but without args["recursive"]

  F::materialize(F::makeDir(
      tempdir_,
      {Fixture::makeDir(
           "A",
           {
               // Note "Z" and "X" are higher than the "F" in "A" below.
               // This tests that decisions are made locally: first choose B
               // from root's {A, B}, then choose F from B's {F}. If leaves
               // are compared across subtrees Z will win and break the
               // test.
               Fixture::makeDir("Z", {}),
               Fixture::makeDir("X", {}),
           }),
       Fixture::makeDir(
           "B",
           {
               Fixture::makeDir("F", {}),
           })}));

  auto plugin = std::make_shared<AlphabeticStandardKillPlugin>();
  ASSERT_NE(plugin, nullptr);
  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";
  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_EQ(*plugin->killed_cgroup, CgroupPath(tempdir_, "B").absolutePath());
}

TEST_F(StandardKillRecursionTest, BacktracksUpTreeOnFail) {
  /*
  Consider
      A - B - C
        \ D - E
  where C has no active processes. We should pick A, B, C, then fail. We
  should
  - backtrack to B,
  - finding no other children of B, backtrack to A
  - pick D, A's only other child
  - pick E
  and ultimately kill E.
  */

  auto plugin = std::make_shared<AlphabeticStandardKillPlugin>();
  ASSERT_NE(plugin, nullptr);

  F::materialize(F::makeDir(
      tempdir_,
      {F::makeDir(
          "test.slice",
          {Fixture::makeDir(
               "A",
               {
                   Fixture::makeDir("Z", {}),
               }),
           Fixture::makeDir(
               "P",
               {
                   Fixture::makeDir("Z", {}),
                   Fixture::makeDir("X", {}),
               }),
           Fixture::makeDir(
               "Q",
               {
                   Fixture::makeDir(
                       "F",
                       {
                           Fixture::makeDir("P", {}),

                       }),
               })})}));

  plugin->unkillable_cgroups.emplace(
      CgroupPath(tempdir_, "test.slice/Q/F/P").absolutePath());

  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["recursive"] = "true";
  args["post_action_delay"] = "0";
  args["dry"] = "true";
  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_EQ(
      *plugin->killed_cgroup,
      CgroupPath(tempdir_, "test.slice/P/Z").absolutePath());
}

TEST_F(StandardKillRecursionTest, RespectsMemoryOomGroup) {
  auto plugin = std::make_shared<AlphabeticStandardKillPlugin>();
  ASSERT_NE(plugin, nullptr);

  F::materialize(F::makeDir(
      tempdir_,
      {F::makeDir(
          "test.slice",
          {Fixture::makeDir(
               "A",
               {
                   Fixture::makeDir("Z", {}),
               }),
           Fixture::makeDir(
               "Y",
               {Fixture::makeDir(
                   "M",
                   {
                       // Without oom.group here, child Y/M/Z would be killed.
                       // With this, recursion will stop here, and Y/M will be
                       // killed.
                       Fixture::makeFile("memory.oom.group", "1\n"),

                       Fixture::makeDir("Z", {}),
                       Fixture::makeDir("X", {}),
                   })}),
           Fixture::makeDir(
               "Q",
               {
                   Fixture::makeDir(
                       "F",
                       {
                           Fixture::makeDir("P", {}),
                       }),
               })})}));

  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["recursive"] = "true";
  args["post_action_delay"] = "0";
  args["dry"] = "true";
  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_EQ(
      *plugin->killed_cgroup,
      CgroupPath(tempdir_, "test.slice/Y/M").absolutePath());
}

TEST_F(StandardKillRecursionTest, RespectsPreferAvoid) {
  // Technically not an aspect of BaseKillPlugin, and implemented
  // separately in each subclass with OomdContext::sortDescWithKillPrefs in
  // the subclass' rankForKilling.  It's expected that all subclasses will
  // use OomdContext::sortDescWithKillPrefs in the same way, so we test an ex
  // of it here.

  F::materialize(F::makeDir(
      tempdir_,
      {F::makeDir(
          "test.slice",
          {Fixture::makeDir(
               "A",
               {
                   Fixture::makeDir("Z", {}),
               }),
           Fixture::makeDir(
               "B",
               {Fixture::makeDir(
                   "M",
                   {
                       Fixture::makeDir("Z", {}),
                       Fixture::makeDir("V", {}),
                   })}),
           Fixture::makeDir(
               "Q",
               {
                   Fixture::makeDir(
                       "F",
                       {
                           Fixture::makeDir("P", {}),
                       }),
               })})}));

  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["recursive"] = "true";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  const auto& expect_to_kill =
      [&](const std::string& expected_victim,
          std::function<void(OomdContext&)> customizer) {
        OomdContext ctx;
        customizer(ctx);
        const PluginConstructionContext compile_context(tempdir_);
        auto plugin = std::make_shared<AlphabeticStandardKillPlugin>();
        ASSERT_NE(plugin, nullptr);
        ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);
        EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
        EXPECT_EQ(
            *plugin->killed_cgroup,
            CgroupPath(tempdir_, expected_victim).absolutePath());
      };

  expect_to_kill("test.slice/B/M/V", [&](auto& ctx) {
    TestHelper::setCgroupData(
        ctx,
        CgroupPath(tempdir_, "test.slice/Q"),
        CgroupData{.kill_preference = KillPreference::AVOID});
    TestHelper::setCgroupData(
        ctx,
        CgroupPath(tempdir_, "test.slice/B/M/V"),
        CgroupData{.kill_preference = KillPreference::PREFER});
  });

  // Test locality of prefs. Even though Y/M/X is PREFER, it's not chosen
  // because Q is chosen over Y in the first level of the tree.
  // Q/F/P is selected despite being the only AVOID in a tree with a PREFER.
  expect_to_kill("test.slice/Q/F/P", [&](auto& ctx) {
    TestHelper::setCgroupData(
        ctx,
        CgroupPath(tempdir_, "test.slice/Q/F/P"),
        CgroupData{.kill_preference = KillPreference::AVOID});
    TestHelper::setCgroupData(
        ctx,
        CgroupPath(tempdir_, "test.slice/B/M/V"),
        CgroupData{.kill_preference = KillPreference::PREFER});
  });

  // Test multiple prefs in a path
  expect_to_kill("test.slice/B/M/V", [&](auto& ctx) {
    TestHelper::setCgroupData(
        ctx,
        CgroupPath(tempdir_, "test.slice/B"),
        CgroupData{.kill_preference = KillPreference::PREFER});
    TestHelper::setCgroupData(
        ctx,
        CgroupPath(tempdir_, "test.slice/B/M/Z"),
        CgroupData{.kill_preference = KillPreference::AVOID});
  });
}

TEST_F(StandardKillRecursionTest, IgnoresDeadCgroup) {
  auto plugin = std::make_shared<AlphabeticStandardKillPlugin>();
  ASSERT_NE(plugin, nullptr);

  F::materialize(F::makeDir(
      tempdir_,
      {Fixture::makeDir(
           "A",
           {
               Fixture::makeDir("Z", {}),
               Fixture::makeDir("X", {}),
           }),
       Fixture::makeDir(
           "B",
           {
               Fixture::makeDir(
                   "F",
                   {
                       // Same as StandardKillRecursionTest.Recurses above,
                       // except cgroup.events' populated=0
                       F::makeFile(
                           "cgroup.events",
                           "populated 0\n"
                           "frozen 0\n"),
                   }),
           })}));

  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["recursive"] = "true";
  args["post_action_delay"] = "0";
  args["dry"] = "true";
  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_EQ(*plugin->killed_cgroup, CgroupPath(tempdir_, "A/Z").absolutePath());
}

TEST_F(StandardKillRecursionTest, IgnoresOutsideConfiguredCgroup) {
  F::materialize(F::makeDir(
      tempdir_,
      {Fixture::makeDir(
           "A",
           {
               Fixture::makeDir(
                   "Y",
                   {
                       Fixture::makeDir("P", {}),
                       Fixture::makeDir("Q", {}),
                   }),
               Fixture::makeDir("X", {}),
           }),
       Fixture::makeDir(
           "B",
           {
               Fixture::makeDir("F", {}),
           }),
       Fixture::makeDir(
           "C",
           {
               Fixture::makeDir("F", {}),
           }),
       Fixture::makeDir(
           "Z",
           {
               Fixture::makeDir("F", {}),
           })}));

  const auto& target_when_plugin_cgroup_arg_is =
      [&](const std::string& cgroup_arg) -> std::string {
    auto plugin = std::make_shared<AlphabeticStandardKillPlugin>();
    EXPECT_NE(plugin, nullptr);
    const PluginConstructionContext compile_context(tempdir_);
    Engine::PluginArgs args;
    args["cgroup"] = cgroup_arg;
    args["recursive"] = "true";
    args["post_action_delay"] = "0";
    args["dry"] = "true";
    args["debug"] = "true";
    EXPECT_EQ(plugin->init(std::move(args), compile_context), 0);
    EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
    return *plugin->killed_cgroup;
  };

  EXPECT_EQ(
      target_when_plugin_cgroup_arg_is("A,B"),
      CgroupPath(tempdir_, "B/F").absolutePath());
  EXPECT_EQ(
      target_when_plugin_cgroup_arg_is("A/*,B,C"),
      CgroupPath(tempdir_, "A/Y/Q").absolutePath());
  EXPECT_EQ(
      target_when_plugin_cgroup_arg_is("A/*,Z,B,C"),
      CgroupPath(tempdir_, "Z/F").absolutePath());
  EXPECT_EQ(
      target_when_plugin_cgroup_arg_is("*"),
      CgroupPath(tempdir_, "Z/F").absolutePath());
}

TEST_F(StandardKillRecursionTest, PrerunsRecursively) {
  F::materialize(F::makeDir(
      tempdir_,
      {Fixture::makeDir(
           "A",
           {
               Fixture::makeDir(
                   "Y",
                   {
                       // oom.group prevents prerun from running on P and Q,
                       // even if "recursive" is set
                       Fixture::makeFile("memory.oom.group", "1\n"),
                       Fixture::makeDir("P", {}),
                       Fixture::makeDir("Q", {}),
                   }),
               Fixture::makeDir("X", {}),
           }),
       Fixture::makeDir(
           "B",
           {
               Fixture::makeDir(
                   "F",
                   {
                       Fixture::makeDir("P", {}),
                       Fixture::makeDir("Q", {}),
                   }),
               Fixture::makeDir("X", {}),
           }),
       Fixture::makeDir(
           "C",
           {
               Fixture::makeDir("F", {}),
           }),
       Fixture::makeDir(
           "Z",
           {
               Fixture::makeDir("F", {}),
           })}));

  const auto& get_touched_cgroups =
      [&](bool recurse) -> std::unordered_set<std::string> {
    auto plugin = std::make_shared<AlphabeticStandardKillPlugin>();
    EXPECT_NE(plugin, nullptr);
    const PluginConstructionContext compile_context(tempdir_);
    Engine::PluginArgs args;
    args["cgroup"] = "B,Z,A/*";
    args["post_action_delay"] = "0";
    args["dry"] = "true";
    if (recurse) {
      args["recursive"] = "true";
    }
    EXPECT_EQ(plugin->init(std::move(args), compile_context), 0);

    std::unordered_set<std::string> touched_cgroup_paths;
    plugin->prerunOnCgroups(ctx_, [&](auto& cgroup_ctx) {
      touched_cgroup_paths.emplace(cgroup_ctx.cgroup().relativePath());
    });
    return touched_cgroup_paths;
  };

  EXPECT_EQ(
      get_touched_cgroups(true),
      (std::unordered_set<std::string>{
          "A/Y", "A/X", "B", "B/F", "B/F/P", "B/F/Q", "B/X", "Z", "Z/F"}));

  // Don't waste CPU walking down the cgroup tree in prerun if
  // recursive_=false
  EXPECT_EQ(
      get_touched_cgroups(false),
      (std::unordered_set<std::string>{"A/Y", "A/X", "B", "Z"}));
}

class KernelKillPlugin : public AlphabeticStandardKillPlugin {
 public:
  using BaseKillPlugin::tryToKillCgroup;
  using BaseKillPlugin::tryToKillPids;
};

class DoubleKillTest : public CorePluginsTest {};

TEST_F(DoubleKillTest, KillsTwice) {
  F::materialize(F::makeDir(
      tempdir_,
      {F::makeDir(
          "A",
          {F::makeFile("cgroup.events", "populated 1"),
           F::makeFile("pids.current", "1\n"),
           F::makeFile("cgroup.kill", "0")})}));

  // Should do nothing, since it will write to cgroup.kill with no side effect.
  // We simulate a delayed kernel kill by leaving cgroup.procs unchanged.
  // However, we should still expect the kill to return true, since we expect
  // cgroup.kill to always succeed. As long as it returns STOP, there will be
  // no double kill.
  auto plugin = std::make_shared<KernelKillPlugin>();
  ASSERT_NE(plugin, nullptr);
  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["recursive"] = "true";
  args["kernelkill"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

class NoPidControllerTest : public CorePluginsTest {};

TEST_F(NoPidControllerTest, KillsWithoutPidController) {
  // Despite not having a pid controller, we should still be able to kill
  F::materialize(F::makeDir(
      tempdir_,
      {F::makeDir(
          "A",
          {F::makeFile("cgroup.kill", "0"),
           F::makeFile("cgroup.events", "populated 1")})}));
  auto plugin = std::make_shared<KernelKillPlugin>();
  ASSERT_NE(plugin, nullptr);
  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["recursive"] = "true";
  args["kernelkill"] = "true";

  ASSERT_EQ(plugin->init(args, compile_context), 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

class PressureRisingBeyondTest : public CorePluginsTest {};

TEST_F(PressureRisingBeyondTest, DetectsHighMemPressure) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("high_pressure")}));

  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "high_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  args["fast_fall_ratio"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{
                  .sec_10 = 99.99, .sec_60 = 99.99, .sec_300 = 99.99},
          .current_usage = 987654321});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(PressureRisingBeyondTest, NoDetectLowMemPressure) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("low_pressure")}));

  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "low_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  args["fast_fall_ratio"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{.sec_10 = 1.11, .sec_60 = 1.11, .sec_300 = 1.11},
          .current_usage = 987654321});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(PressureRisingBeyondTest, DetectsHighMemPressureMultiCgroup) {
  F::materialize(F::makeDir(
      tempdir_, {F::makeDir("high_pressure"), F::makeDir("low_pressure")}));

  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "low_pressure,high_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  args["fast_fall_ratio"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{
                  .sec_10 = 99.99, .sec_60 = 99.99, .sec_300 = 99.99},
          .current_usage = 987654321});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{.sec_10 = 1.11, .sec_60 = 1.11, .sec_300 = 1.11},
          .current_usage = 987654321});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(PressureRisingBeyondTest, DetectsHighMemPressureWildcard) {
  F::materialize(F::makeDir(
      tempdir_, {F::makeDir("high_pressure"), F::makeDir("low_pressure")}));

  auto plugin = createPlugin("pressure_rising_beyond");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "*_*";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  args["fast_fall_ratio"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{
                  .sec_10 = 99.99, .sec_60 = 99.99, .sec_300 = 99.99},
          .current_usage = 987654321});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{.sec_10 = 1.11, .sec_60 = 1.11, .sec_300 = 1.11},
          .current_usage = 987654321});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

class PressureAboveTest : public CorePluginsTest {};

TEST_F(PressureAboveTest, DetectsHighMemPressure) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("high_pressure")}));

  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "high_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{
                  .sec_10 = 99.99, .sec_60 = 99.99, .sec_300 = 99.99},
          .current_usage = 987654321});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(PressureAboveTest, NoDetectLowMemPressure) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("low_pressure")}));

  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "low_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{.sec_10 = 1.11, .sec_60 = 1.11, .sec_300 = 1.11},
          .current_usage = 987654321});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(PressureAboveTest, DetectsHighMemPressureMultiCgroup) {
  F::materialize(F::makeDir(
      tempdir_, {F::makeDir("high_pressure"), F::makeDir("low_pressure")}));

  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "high_pressure,low_pressure";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{
                  .sec_10 = 99.99, .sec_60 = 99.99, .sec_300 = 99.99},
          .current_usage = 987654321});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{.sec_10 = 1.11, .sec_60 = 1.11, .sec_300 = 1.11},
          .current_usage = 987654321});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(PressureAboveTest, DetectsHighMemPressureWildcard) {
  F::materialize(F::makeDir(
      tempdir_, {F::makeDir("high_pressure"), F::makeDir("low_pressure")}));

  auto plugin = createPlugin("pressure_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "*";
  args["resource"] = "memory";
  args["threshold"] = "80";
  args["duration"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{
                  .sec_10 = 99.99, .sec_60 = 99.99, .sec_300 = 99.99},
          .current_usage = 987654321});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_pressure"),
      CgroupData{
          .mem_pressure =
              ResourcePressure{.sec_10 = 1.11, .sec_60 = 1.11, .sec_300 = 1.11},
          .current_usage = 987654321});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

class MemoryAboveTest : public CorePluginsTest {};

TEST_F(MemoryAboveTest, DetectsHighMemUsage) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("high_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536M";
  args["duration"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_memory"),
      CgroupData{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(MemoryAboveTest, NoDetectLowMemUsage) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("low_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536M";
  args["duration"] = "0";
  const PluginConstructionContext compile_context(tempdir_);

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_memory"),
      CgroupData{.current_usage = 1073741824});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(MemoryAboveTest, DetectsHighMemUsageCompat) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("high_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536"; // Should be interpreted as MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_memory"),
      CgroupData{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(MemoryAboveTest, NoDetectLowMemUsageCompat) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("low_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536"; // Should be interpreted as MB
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_memory"),
      CgroupData{.current_usage = 1073741824});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(MemoryAboveTest, DetectsHighMemUsagePercent) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("high_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "10%";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_memory"),
      CgroupData{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(MemoryAboveTest, NoDetectLowMemUsageMultiple) {
  F::materialize(F::makeDir(
      tempdir_, {F::makeDir("high_memory"), F::makeDir("low_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536M";
  args["duration"] = "0";
  args["debug"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_memory"),
      CgroupData{.current_usage = 1073741824});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_memory"),
      CgroupData{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(MemoryAboveTest, DetectsHighMemUsageMultiple) {
  F::materialize(F::makeDir(
      tempdir_, {F::makeDir("high_memory"), F::makeDir("low_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "1536M";
  args["duration"] = "0";
  args["debug"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_memory"),
      CgroupData{.current_usage = 1073741824});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_memory"),
      CgroupData{.current_usage = 2147483648});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(MemoryAboveTest, NoDetectLowMemUsagePercent) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("low_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold"] = "80%";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(MemoryAboveTest, DetectsHighAnonUsage) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("high_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold_anon"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_memory"),
      CgroupData{
          .memory_stat = memory_stat_t{{"anon", 2147483648}},
          .swap_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(MemoryAboveTest, NoDetectLowAnonUsage) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("low_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold_anon"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_memory"),
      CgroupData{
          .memory_stat = memory_stat_t{{"anon", 1073741824}},
          .swap_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(MemoryAboveTest, DetectsHighAnonUsageIgnoreLowMemUsage) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("high_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "high_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold_anon"] = "1536M";
  args["threshold"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "high_memory"),
      CgroupData{
          .memory_stat = memory_stat_t{{"anon", 2147483648}},
          .current_usage = 1073741824,
          .swap_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(MemoryAboveTest, NoDetectLowAnonUsageIgnoreHighMemUsage) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("low_memory")}));

  auto plugin = createPlugin("memory_above");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "low_memory";
  args["meminfo_location"] = "oomd/fixtures/plugins/memory_above/meminfo";
  args["threshold_anon"] = "1536M";
  args["threshold"] = "1536M";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "low_memory"),
      CgroupData{
          .memory_stat = memory_stat_t{{"anon", 1073741824}},
          .current_usage = 2147483648,
          .swap_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

class MemoryReclaimTest : public CorePluginsTest {};

TEST_F(MemoryReclaimTest, SingleCgroupReclaimSuccess) {
  auto plugin = createPlugin("memory_reclaim");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/memory_reclaim/single_cgroup");
  args["cgroup"] = "cgroup1";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_, CgroupPath(compile_context.cgroupFs(), "cgroup1"), {});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(MemoryReclaimTest, MultiCgroupReclaimSuccess) {
  auto plugin = createPlugin("memory_reclaim");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/memory_reclaim/multi_cgroup");
  args["cgroup"] = "cgroup1,cgroup2";
  args["duration"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_, CgroupPath(compile_context.cgroupFs(), "cgroup1"), {});
  TestHelper::setCgroupData(
      ctx_, CgroupPath(compile_context.cgroupFs(), "cgroup2"), {});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

class SwapFreeTest : public CorePluginsTest {};

TEST_F(SwapFreeTest, LowSwap) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["threshold_pct"] = "20";
  const PluginConstructionContext compile_context("/sys/fs/cgroup");

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  SystemContext system_ctx;
  system_ctx.swaptotal = static_cast<uint64_t>(20971512) * 1024;
  system_ctx.swapused = static_cast<uint64_t>(20971440) * 1024;
  ctx_.setSystemContext(system_ctx);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(SwapFreeTest, EnoughSwap) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["threshold_pct"] = "20";
  const PluginConstructionContext compile_context("/sys/fs/cgroup");

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  SystemContext system_ctx;
  system_ctx.swaptotal = static_cast<uint64_t>(20971512) * 1024;
  system_ctx.swapused = static_cast<uint64_t>(3310136) * 1024;
  ctx_.setSystemContext(system_ctx);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(SwapFreeTest, SwapOff) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["threshold_pct"] = "20";
  const PluginConstructionContext compile_context("/sys/fs/cgroup");

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(SwapFreeTest, SwapoutRate) {
  auto plugin = createPlugin("swap_free");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  args["threshold_pct"] = "20";
  args["swapout_bps_threshold"] = "1000000";
  const PluginConstructionContext compile_context("/sys/fs/cgroup");

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  SystemContext system_ctx;
  system_ctx.swaptotal = static_cast<uint64_t>(20971512) * 1024;
  system_ctx.swapused = static_cast<uint64_t>(20971440) * 1024;
  // Low swapout rate => OK
  system_ctx.swapout_bps = 800000;
  ctx_.setSystemContext(system_ctx);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  // High swapout rate => trigger
  system_ctx.swapout_bps = 1200000;
  ctx_.setSystemContext(system_ctx);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

class ExistsTest : public CorePluginsTest {};

TEST_F(ExistsTest, Exists) {
  auto plugin = createPlugin("exists");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "cgroup_A,cgroup_B,cgroup_C";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  F::materialize(F::makeDir(tempdir_, {F::makeDir("cgroup_D")}));
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  F::materialize(F::makeDir(tempdir_, {F::makeDir("cgroup_C")}));
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(ExistsTest, NotExists) {
  auto plugin = createPlugin("exists");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "cgroup_A,cgroup_B,cgroup_C";
  args["negate"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  F::materialize(F::makeDir(tempdir_, {F::makeDir("cgroup_D")}));
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);

  F::materialize(F::makeDir(tempdir_, {F::makeDir("cgroup_C")}));
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

class KillIOCostTest : public CorePluginsTest {};

TEST_F(KillIOCostTest, TemporalCounter) {
  auto plugin = std::make_shared<KillIOCost<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_io_cost");
  args["cgroup"] = "one_high/cgroup1";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  CgroupPath cgroup(compile_context.cgroupFs(), "one_high/cgroup1");
  TestHelper::setCgroupData(
      ctx_, cgroup, CgroupData{.io_cost_cumulative = 10000});
  plugin->prerun(ctx_);
  EXPECT_TRUE(
      TestHelper::getDataRef(*ctx_.addToCacheAndGet(cgroup)).io_cost_rate);
}

TEST_F(KillIOCostTest, KillsHighestIOCost) {
  auto plugin = std::make_shared<KillIOCost<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_io_cost");
  args["cgroup"] = "one_high/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.io_cost_cumulative = 10000, .io_cost_rate = 10});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.io_cost_cumulative = 5000, .io_cost_rate = 30});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.io_cost_cumulative = 6000, .io_cost_rate = 50});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.io_cost_cumulative = 20000, .io_cost_rate = 100});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(111));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
}

TEST_F(KillIOCostTest, KillsHighestIOCostMultiCgroup) {
  auto plugin = std::make_shared<KillIOCost<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_io_cost");
  args["cgroup"] = "one_high/*,sibling/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.io_cost_cumulative = 10000, .io_cost_rate = 10});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.io_cost_cumulative = 5000, .io_cost_rate = 30});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.io_cost_cumulative = 6000, .io_cost_rate = 50});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.io_cost_cumulative = 20000, .io_cost_rate = 100});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(888));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
}

TEST_F(KillIOCostTest, DoesntKillsHighestIOCostDry) {
  auto plugin = std::make_shared<KillIOCost<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_pressure");
  args["cgroup"] = "one_high/*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.io_cost_cumulative = 10000, .io_cost_rate = 10});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.io_cost_cumulative = 5000, .io_cost_rate = 30});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.io_cost_cumulative = 6000, .io_cost_rate = 50});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.io_cost_cumulative = 20000, .io_cost_rate = 100});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}

class KillPgScanTest : public CorePluginsTest {};

TEST_F(KillPgScanTest, KillsHighestPgScan) {
  auto plugin = std::make_shared<KillPgScan<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_pg_scan");
  args["cgroup"] = "one_high/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.pg_scan_cumulative = 10000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.pg_scan_cumulative = 5000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.pg_scan_cumulative = 6000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.pg_scan_cumulative = 20000});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::ASYNC_PAUSED);
  ctx_.refresh();
  ctx_.bumpCurrentTick();

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.pg_scan_cumulative = 10010});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.pg_scan_cumulative = 5030});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.pg_scan_cumulative = 6050});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.pg_scan_cumulative = 20100});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_THAT(plugin->killed, Contains(111));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
}

TEST_F(KillPgScanTest, KillsHighestPgScanMultiCgroup) {
  auto plugin = std::make_shared<KillPgScan<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_pg_scan");
  args["cgroup"] = "one_high/*,sibling/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.pg_scan_cumulative = 10000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.pg_scan_cumulative = 5000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.pg_scan_cumulative = 6000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.pg_scan_cumulative = 20000});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::ASYNC_PAUSED);
  ctx_.refresh();
  ctx_.bumpCurrentTick();

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.pg_scan_cumulative = 10010});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.pg_scan_cumulative = 5030});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.pg_scan_cumulative = 6050});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.pg_scan_cumulative = 20100});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_THAT(plugin->killed, Contains(888));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
}

TEST_F(KillPgScanTest, DoNotKillZeroPgScan) {
  auto plugin = std::make_shared<KillPgScan<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_pg_scan");
  args["cgroup"] = "one_high/*,sibling/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.pg_scan_cumulative = 10000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.pg_scan_cumulative = 5000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.pg_scan_cumulative = 6000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.pg_scan_cumulative = 20000});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::ASYNC_PAUSED);
  ctx_.refresh();
  ctx_.bumpCurrentTick();

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.pg_scan_cumulative = 10000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.pg_scan_cumulative = 5000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.pg_scan_cumulative = 6000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.pg_scan_cumulative = 20000});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);

  EXPECT_EQ(plugin->killed.size(), 0);
}

TEST_F(KillPgScanTest, DoesntKillsHighestPgScanDry) {
  auto plugin = std::make_shared<KillPgScan<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_pg_scan");
  args["cgroup"] = "one_high/*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.pg_scan_cumulative = 10000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.pg_scan_cumulative = 5000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.pg_scan_cumulative = 6000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.pg_scan_cumulative = 20000});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::ASYNC_PAUSED);
  ctx_.refresh();
  ctx_.bumpCurrentTick();

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{.pg_scan_cumulative = 10010});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{.pg_scan_cumulative = 5030});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{.pg_scan_cumulative = 6050});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.pg_scan_cumulative = 20100});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_EQ(plugin->killed.size(), 0);
}

TEST_F(KillPgScanTest, CanTargetRecursively) {
  // without cgroup.controllers, CgroupContext::refresh() thinks the cgroup was
  // removed, and gets itself removed from OomdContext's cache.
  auto controllers = F::makeFile("cgroup.controllers", "memory");

  F::materialize(F::makeDir(
      tempdir_,
      {controllers,
       Fixture::makeDir(
           "A",
           {
               controllers,
               Fixture::makeDir("Z", {controllers}),
               Fixture::makeDir("X", {controllers}),
           }),
       Fixture::makeDir(
           "B",
           {
               controllers,
               Fixture::makeDir("F", {controllers}),
           }),
       Fixture::makeDir(
           "sibling",
           {
               controllers,
               Fixture::makeDir("F", {controllers}),
           })}));

  auto plugin = std::make_shared<KillPgScan<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "A,B";
  args["recursive"] = "true";
  args["post_action_delay"] = "0";
  args["debug"] = "true";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A"),
      CgroupData{.pg_scan_cumulative = 20});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A/Z"),
      CgroupData{.pg_scan_cumulative = 10});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A/X"),
      CgroupData{.pg_scan_cumulative = 10});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "B"),
      CgroupData{.pg_scan_cumulative = 30});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling"),
      CgroupData{.pg_scan_cumulative = 50});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::ASYNC_PAUSED);
  ctx_.refresh();
  ctx_.bumpCurrentTick();

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A"),
      CgroupData{.pg_scan_cumulative = 1100});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A/Z"),
      CgroupData{.pg_scan_cumulative = 600});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A/X"),
      CgroupData{.pg_scan_cumulative = 500});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "B"),
      CgroupData{.pg_scan_cumulative = 1000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling"),
      CgroupData{.pg_scan_cumulative = 30000});

  plugin->prerun(ctx_);
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  EXPECT_EQ(*plugin->killed_cgroup, CgroupPath(tempdir_, "A/Z").absolutePath());
}

TEST_F(ExistsTest, ExistsWildcard) {
  auto plugin = createPlugin("exists");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "cgroup_PREFIX*";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  F::materialize(F::makeDir(tempdir_, {F::makeDir("cgroup_SOMETHING")}));
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  F::materialize(F::makeDir(tempdir_, {F::makeDir("cgroup_PREFIXhere")}));
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(ExistsTest, NotExistsWildcard) {
  auto plugin = createPlugin("exists");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "cgroup_PREFIX*";
  args["negate"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  F::materialize(F::makeDir(tempdir_, {F::makeDir("cgroup_SOMETHING")}));
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);

  F::materialize(F::makeDir(tempdir_, {F::makeDir("cgroup_PREFIXhere")}));
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

class NrDyingDescendantsTest : public CorePluginsTest {};

TEST_F(NrDyingDescendantsTest, SingleCgroupLte) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("cg")}));

  auto plugin = createPlugin("nr_dying_descendants");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "cg";
  args["debug"] = "true";
  args["lte"] = "true";
  args["count"] = "100";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "cg"),
      CgroupData{.nr_dying_descendants = 123});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "cg"),
      CgroupData{.nr_dying_descendants = 90});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(NrDyingDescendantsTest, SingleCgroupGt) {
  F::materialize(F::makeDir(tempdir_, {F::makeDir("cg")}));

  auto plugin = createPlugin("nr_dying_descendants");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "cg";
  args["debug"] = "true";
  args["lte"] = "false";
  args["count"] = "100";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "cg"),
      CgroupData{.nr_dying_descendants = 123});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "cg"),
      CgroupData{.nr_dying_descendants = 90});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}

TEST_F(NrDyingDescendantsTest, RootCgroup) {
  auto plugin = createPlugin("nr_dying_descendants");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "/";
  args["debug"] = "true";
  args["lte"] = "false"; // Greater than
  args["count"] = "29";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), ""),
      CgroupData{.nr_dying_descendants = 30});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

TEST_F(NrDyingDescendantsTest, MultiCgroupGt) {
  F::materialize(F::makeDir(
      tempdir_,
      {F::makeDir("above"), F::makeDir("above1"), F::makeDir("below")}));

  auto plugin = createPlugin("nr_dying_descendants");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "above,above1,below";
  args["debug"] = "true";
  args["lte"] = "true";
  args["count"] = "100";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "above"),
      CgroupData{.nr_dying_descendants = 200});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "above1"),
      CgroupData{.nr_dying_descendants = 300});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "below"),
      CgroupData{.nr_dying_descendants = 90});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
}

class KillMemoryGrowthTest : public CorePluginsTest {};

TEST_F(KillMemoryGrowthTest, InvalidArgs) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/cgroup1";
  args["growing_size_percentile"] = "100";
  ASSERT_EQ(plugin->init(args, compile_context), 1);

  args["growing_size_percentile"] = "99";
  ASSERT_EQ(plugin->init(args, compile_context), 0);
}

TEST_F(KillMemoryGrowthTest, TemporalCounter) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/cgroup1";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  CgroupPath cgroup(compile_context.cgroupFs(), "one_big/cgroup1");
  TestHelper::setCgroupData(ctx_, cgroup, CgroupData{.current_usage = 60});
  plugin->prerun(ctx_);
  EXPECT_TRUE(
      TestHelper::getDataRef(*ctx_.addToCacheAndGet(cgroup)).average_usage);
}

TEST_F(KillMemoryGrowthTest, KillsBigCgroup) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{
          .current_usage = 60,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(123));
  EXPECT_THAT(plugin->killed, Contains(456));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(
      plugin->killed, Not(Contains(888))); // make sure there's no siblings
}

TEST_F(KillMemoryGrowthTest, PreferredOverridesSize) {
  // Same as KillsBigCgroup, but with .kill_preferences set

  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{
          .current_usage = 60,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{
          .current_usage = 20,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(789));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(
      plugin->killed, Not(Contains(888))); // make sure there's no siblings
}

TEST_F(KillMemoryGrowthTest, AvoidedOverridesSize) {
  // Same as KillsBigCgroup, but with .kill_preferences set

  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{
          .current_usage = 60,
          .kill_preference = KillPreference::AVOID,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{
          .current_usage = 30,
          .average_usage = 30,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(789));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(
      plugin->killed, Not(Contains(888))); // make sure there's no siblings
}

TEST_F(KillMemoryGrowthTest, AvoidedNoEffect) {
  // Same as KillsBigCgroup, but with .kill_preferences set. Avoiding cgroups
  // we would not have killed anyway has no effect.

  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{
          .current_usage = 60,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{
          .current_usage = 30,
          .kill_preference = KillPreference::AVOID,
          .average_usage = 30,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{
          .current_usage = 20,
          .kill_preference = KillPreference::AVOID,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(123));
  EXPECT_THAT(plugin->killed, Contains(456));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(
      plugin->killed, Not(Contains(888))); // make sure there's no siblings
}

TEST_F(KillMemoryGrowthTest, PreferredNoEffect) {
  // Same as KillsBigCgroup, but with .kill_preferences set. Preferring all
  // cgroups is the same as preferring none of them.

  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{
          .current_usage = 60,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{
          .current_usage = 30,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 30,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{
          .current_usage = 20,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .current_usage = 20,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(123));
  EXPECT_THAT(plugin->killed, Contains(456));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(
      plugin->killed, Not(Contains(888))); // make sure there's no siblings
}

TEST_F(KillMemoryGrowthTest, KillsOneOfPreferred) {
  // Same as KillsBigCgroup, but with .kill_preferences set. When multiple
  // cgroups are preferred, we choose between them with the same rules we use
  // when no cgroups are preferred.

  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{
          .current_usage = 60,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{
          .current_usage = 30,
          .kill_preference = KillPreference::AVOID,
          .average_usage = 30,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{
          .current_usage = 20,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .current_usage = 20,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(123));
  EXPECT_THAT(plugin->killed, Contains(456));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(
      plugin->killed, Not(Contains(888))); // make sure there's no siblings
}

TEST_F(KillMemoryGrowthTest, KillsBigCgroupGrowth) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "growth_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  // First test that we do the last ditch size killing.
  //
  // cgroup3 should be killed even though (30 / (21+20+30) < .5)
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup1"),
      CgroupData{
          .current_usage = 21,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup2"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup3"),
      CgroupData{
          .current_usage = 30,
          .average_usage = 30,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(111));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));

  // Now lower average usage to artificially "boost" growth rate to trigger
  // growth kill
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup1"),
      CgroupData{
          .current_usage = 21,
          .average_usage = 5,
      });

  // Do the same thing for a sibling cgroup, but set the growth higher. This
  // tests that sibling removal occurs for growth kills too.
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .current_usage = 99,
          .average_usage = 5,
      });

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Contains(123));
  EXPECT_THAT(plugin->killed, Contains(456));
}

TEST_F(KillMemoryGrowthTest, DoesntGrowthKillBelowUsageThreshold) {
  const auto& target_with_args = [&](Engine::PluginArgs&& extra_args) {
    Engine::PluginArgs args;
    const PluginConstructionContext compile_context(
        "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
    args["cgroup"] = "growth_big/*";
    args["post_action_delay"] = "0";
    args["size_threshold"] = "50";
    args.merge(extra_args);

    TestHelper::setCgroupData(
        ctx_,
        CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup1"),
        CgroupData{
            .current_usage = 40,
            .average_usage = 7,
        });
    TestHelper::setCgroupData(
        ctx_,
        CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup2"),
        CgroupData{
            .current_usage = 50,
            .average_usage = 30,
        });
    TestHelper::setCgroupData(
        ctx_,
        CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup3"),
        CgroupData{
            .current_usage = 60,
            .average_usage = 60,
        });

    auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
    EXPECT_NE(plugin, nullptr);
    EXPECT_EQ(plugin->init(std::move(args), compile_context), 0);
    EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
    return plugin->killed;
  };

  // None are eligible for size kill at size_threshold=50, which translates to
  // current_usage > (60+50+40)*0.5 = 75.

  // At growing_size_percentile=50, cgroup2 and cgroup3 are qualified, and
  // at min_growth_ratio=1.25 cgroup1 and cgroup2 are qualified, so only
  // cgroup2 is eligible for growth kill.
  EXPECT_EQ(
      target_with_args(Engine::PluginArgs{
          {"growing_size_percentile", "50"}, {"min_growth_ratio", "1.25"}}),
      std::unordered_set<int>({789})); // cgroup2

  // At growing_size_percentile=20 all cgroups are eligible, and cgroup1 has
  // the highest growth.
  EXPECT_EQ(
      target_with_args(Engine::PluginArgs{
          {"growing_size_percentile", "20"}, {"min_growth_ratio", "1.25"}}),
      std::unordered_set<int>({123, 456})); // cgroup1

  // All cgroups are eligible when both restrictions set to zero.
  EXPECT_EQ(
      target_with_args(Engine::PluginArgs{
          {"growing_size_percentile", "0"}, {"min_growth_ratio", "0"}}),
      std::unordered_set<int>({123, 456})); // cgroup1

  // At growing_size_percentile=67 only cgroup3 is qualified, but it does not
  // meet the min_growth_ratio requirement. We skip the growth kill phase, and
  // fall through to kill by size (no threshold), which picks cgroup3.
  EXPECT_EQ(
      target_with_args(Engine::PluginArgs{
          {"growing_size_percentile", "67"}, {"min_growth_ratio", "1.25"}}),
      std::unordered_set<int>({111})); // cgroup3

  // All cgroups pass growing_size_percentile=20, but none pass
  // min_growth_ratio=10. We skip the growth kill phase, and fall through to
  // kill by size (no threshold), which picks cgroup3.
  EXPECT_EQ(
      target_with_args(Engine::PluginArgs{
          {"growing_size_percentile", "20"}, {"min_growth_ratio", "10"}}),
      std::unordered_set<int>({111})); // cgroup3
}

TEST_F(KillMemoryGrowthTest, KillsByPreferredGrowth) {
  // Preferred cgroups that wont be targeted until the growth kill phase are
  // killed before non-prefer cgroups targeted in the size w/ threshold phase.

  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "growth_big/*";
  args["post_action_delay"] = "0";
  args["debug"] = "1";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup1"),
      CgroupData{
          .current_usage = 21 << 20,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 5 << 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup2"),
      CgroupData{
          .current_usage = 99 << 20,
          .kill_preference = KillPreference::PREFER,
          .average_usage = 5 << 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup3"),
      CgroupData{
          .current_usage = 1000 << 20,
          .average_usage = 1000 << 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(789));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
}

// If without preferences we'd kill A from {A, B, C}, any additional
// preferences where A is PREFER should still target A.
// For example, if we PREFER A and B, we would not expect to start killing B,
// since A would have been killed without preferences, and A is not lower
// preference than anyone else.
class KillMemoryGrowthConsistentWithPreference
    : public CorePluginsTest,
      public WithParamInterface<KillPreference> {};

TEST_P(KillMemoryGrowthConsistentWithPreference, SizeThreshold) {
  KillPreference maybe_prefer = GetParam();

  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "growth_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup1"),
      CgroupData{
          .current_usage = 21,
          .kill_preference = maybe_prefer,
          .average_usage = 5,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup2"),
      CgroupData{
          .current_usage = 99,
          .kill_preference = maybe_prefer,
          .average_usage = 5,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "growth_big/cgroup3"),
      CgroupData{
          .current_usage = 30,
          .average_usage = 30,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(789));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
}

INSTANTIATE_TEST_CASE_P(
    NoPrefVsPrefer,
    KillMemoryGrowthConsistentWithPreference,
    Values(KillPreference::NORMAL, KillPreference::PREFER));

TEST_F(KillMemoryGrowthTest, KillsBigCgroupMultiCgroup) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/*,sibling/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{
          .current_usage = 60,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .current_usage = 100,
          .average_usage = 100,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(888));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
}

TEST_F(KillMemoryGrowthTest, CanTargetRecursively) {
  F::materialize(F::makeDir(
      tempdir_,
      {Fixture::makeDir(
           "A",
           {
               Fixture::makeDir("Z", {}),
               Fixture::makeDir("X", {}),
           }),
       Fixture::makeDir(
           "B",
           {
               Fixture::makeDir("F", {}),
           }),
       Fixture::makeDir(
           "sibling",
           {
               Fixture::makeDir("F", {}),
           })}));

  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(tempdir_);
  args["cgroup"] = "A,B";
  args["recursive"] = "true";
  args["post_action_delay"] = "0";
  args["debug"] = "true";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A"),
      CgroupData{
          .current_usage = 60,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A/Z"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "A/X"),
      CgroupData{
          .current_usage = 40,
          .average_usage = 40,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "B"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling"),
      CgroupData{
          .current_usage = 100,
          .average_usage = 100,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_EQ(*plugin->killed_cgroup, CgroupPath(tempdir_, "A/X").absolutePath());
}

TEST_F(KillMemoryGrowthTest, DoesntKillBigCgroupInDry) {
  auto plugin = std::make_shared<KillMemoryGrowth<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_memory_size_or_growth");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{
          .current_usage = 60,
          .average_usage = 60,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{
          .current_usage = 20,
          .average_usage = 20,
      });
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}

class KillSwapUsageTest : public CorePluginsTest {};

TEST_F(KillSwapUsageTest, KillsBigSwapCgroup) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_swap_usage");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{.swap_usage = 20});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{.swap_usage = 60});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{.swap_usage = 40});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(789));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
}

TEST_F(KillSwapUsageTest, BiasedSwapKillTest) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  F::materialize(F::makeDir(
      tempdir_,
      {F::makeFile("meminfo", "SwapTotal:\t25 kB\nMemTotal:\t100 kB"),
       F::makeDir(
           "cgroup1",
           {
               F::makeFile("cgroup.procs", "101\n"),
           }),
       F::makeDir(
           "cgroup2",
           {
               F::makeFile("cgroup.procs", "201\n"),
           }),
       F::makeDir(
           "cgroup3",
           {
               F::makeFile("cgroup.procs", "301\n"),
           })}));

  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["meminfo_location"] = tempdir_ + "/meminfo";
  args["cgroup"] = "./*";
  args["post_action_delay"] = "0";
  args["biased_swap_kill"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), ""),
      CgroupData{.swap_usage = 10000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "./cgroup1"),
      CgroupData{.swap_usage = 1900, .memory_protection = 1500});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "./cgroup2"),
      CgroupData{.swap_usage = 2000, .memory_protection = 2000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "./cgroup3"),
      CgroupData{.swap_usage = 1500, .memory_protection = 6000});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  for (auto i : plugin->killed) {
    std::cout << i << std::endl;
  }
  EXPECT_THAT(plugin->killed, Contains(101));
  EXPECT_THAT(plugin->killed, Not(Contains(201)));
  EXPECT_THAT(plugin->killed, Not(Contains(301)));
}

TEST_F(KillSwapUsageTest, BiasedSwapKillNoSwapExcessTest) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  F::materialize(F::makeDir(
      tempdir_,
      {F::makeFile("meminfo", "SwapTotal:\t25 kB\nMemTotal:\t100 kB"),
       F::makeDir(
           "cgroup1",
           {
               F::makeFile("cgroup.procs", "101\n"),
           }),
       F::makeDir(
           "cgroup2",
           {
               F::makeFile("cgroup.procs", "201\n"),
           }),
       F::makeDir(
           "cgroup3",
           {
               F::makeFile("cgroup.procs", "301\n"),
           })}));

  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["meminfo_location"] = tempdir_ + "/meminfo";
  args["cgroup"] = "./*";
  args["post_action_delay"] = "0";
  args["biased_swap_kill"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), ""),
      CgroupData{.swap_usage = 10000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "./cgroup1"),
      CgroupData{.swap_usage = 350, .memory_protection = 1500});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "./cgroup2"),
      CgroupData{.swap_usage = 350, .memory_protection = 2000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "./cgroup3"),
      CgroupData{.swap_usage = 1000, .memory_protection = 6000});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  for (auto i : plugin->killed) {
    std::cout << i << std::endl;
  }
  // The goal here is to check that despite that none of the cgroups is in
  // excess of its swap allowance, but still use some swap one cgroup will
  // still be killed.
  EXPECT_THAT(plugin->killed, SizeIs(1));
}

TEST_F(KillSwapUsageTest, BiasedSwapKillZeroSwapTest) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  F::materialize(F::makeDir(
      tempdir_,
      {
          F::makeFile("meminfo", "SwapTotal:\t25 kB\nMemTotal:\t100 kB"),
          F::makeDir(
              "cgroup1",
              {
                  F::makeFile("cgroup.procs", "101\n"),
              }),
      }));

  const PluginConstructionContext compile_context(tempdir_);
  Engine::PluginArgs args;
  args["meminfo_location"] = tempdir_ + "/meminfo";
  args["cgroup"] = "./*";
  args["post_action_delay"] = "0";
  args["biased_swap_kill"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), ""),
      CgroupData{.swap_usage = 10000});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "./cgroup1"),
      CgroupData{.swap_usage = 0, .memory_protection = 1500});

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
  for (auto i : plugin->killed) {
    std::cout << i << std::endl;
  }

  // We check that the implicit filtering threshold of 0 is applied.
  // Ultimately our goal is not to kill any cgroups that do not use
  // any swap because that's 100% a wrong decision.
  EXPECT_THAT(plugin->killed, SizeIs(0));
}

TEST_F(KillSwapUsageTest, ThresholdTest) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_swap_usage");
  args["meminfo_location"] = "oomd/fixtures/plugins/kill_by_swap_usage/meminfo";
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";
  args["threshold"] = "20%";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{.swap_usage = 1});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{.swap_usage = 2});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{.swap_usage = 3});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{.swap_usage = 20 << 10});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{.swap_usage = 60 << 10});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{.swap_usage = 40 << 10});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(789));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
}

TEST_F(KillSwapUsageTest, KillsBigSwapCgroupMultiCgroup) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_swap_usage");
  args["cgroup"] = "one_big/*,sibling/*";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{.swap_usage = 20});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{.swap_usage = 60});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{.swap_usage = 40});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{.swap_usage = 70});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(555));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
}

TEST_F(KillSwapUsageTest, DoesntKillBigSwapCgroupDry) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_swap_usage");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{.swap_usage = 20});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{.swap_usage = 60});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{.swap_usage = 40});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}

TEST_F(KillSwapUsageTest, DoesntKillNoSwap) {
  auto plugin = std::make_shared<KillSwapUsage<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_swap_usage");
  args["cgroup"] = "one_big/*";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup1"),
      CgroupData{});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup2"),
      CgroupData{});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_big/cgroup3"),
      CgroupData{});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::CONTINUE);
  EXPECT_EQ(plugin->killed.size(), 0);
}

class KillPressureTest : public CorePluginsTest {};

TEST_F(KillPressureTest, KillsHighestPressure) {
  auto plugin = std::make_shared<KillPressure<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_pressure");
  args["cgroup"] = "one_high/*";
  args["resource"] = "io";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 60,
              .sec_60 = 60,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 50,
              .sec_60 = 70,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 80,
              .sec_60 = 80,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 99,
              .sec_60 = 99,
              .sec_300 = 99,
          }});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(111));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
  EXPECT_THAT(plugin->killed, Not(Contains(888)));
}

TEST_F(KillPressureTest, KillsHighestPressureMultiCgroup) {
  auto plugin = std::make_shared<KillPressure<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_pressure");
  args["cgroup"] = "one_high/*,sibling/*";
  args["resource"] = "io";
  args["post_action_delay"] = "0";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 60,
              .sec_60 = 60,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 50,
              .sec_60 = 70,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 80,
              .sec_60 = 80,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 99,
              .sec_60 = 99,
              .sec_300 = 99,
          }});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_THAT(plugin->killed, Contains(888));
  EXPECT_THAT(plugin->killed, Not(Contains(111)));
  EXPECT_THAT(plugin->killed, Not(Contains(123)));
  EXPECT_THAT(plugin->killed, Not(Contains(456)));
  EXPECT_THAT(plugin->killed, Not(Contains(789)));
}

TEST_F(KillPressureTest, DoesntKillsHighestPressureDry) {
  auto plugin = std::make_shared<KillPressure<BaseKillPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context(
      "oomd/fixtures/plugins/kill_by_pressure");
  args["cgroup"] = "one_high/*";
  args["resource"] = "io";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup1"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 60,
              .sec_60 = 60,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup2"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 50,
              .sec_60 = 70,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "one_high/cgroup3"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 80,
              .sec_60 = 80,
          }});
  TestHelper::setCgroupData(
      ctx_,
      CgroupPath(compile_context.cgroupFs(), "sibling/cgroup1"),
      CgroupData{
          .io_pressure = ResourcePressure{
              .sec_10 = 99,
              .sec_60 = 99,
              .sec_300 = 99,
          }});
  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->killed.size(), 0);
}

class StopTest : public CorePluginsTest {};

TEST_F(StopTest, Stops) {
  auto plugin = createPlugin("stop");
  ASSERT_NE(plugin, nullptr);

  Engine::PluginArgs args;
  const PluginConstructionContext compile_context("/sys/fs/cgroup");

  ASSERT_EQ(plugin->init(std::move(args), compile_context), 0);

  EXPECT_EQ(plugin->run(ctx_), Engine::PluginRet::STOP);
}
