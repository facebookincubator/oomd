/*
 * Copyright (C) 2026-present, Meta Platforms, Inc. and affiliates
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 */

#include "oomd/plugins/XattrGlob.h"

#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <gtest/gtest.h>

#include "oomd/OomdContext.h"
#include "oomd/PluginConstructionContext.h"
#include "oomd/PluginRegistry.h"
#include "oomd/engine/DetectorGroup.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/util/Fixture.h"
#include "oomd/util/Fs.h"

using namespace Oomd;

namespace {

constexpr std::string_view kTestXattr{"user.oomd_value"};

class TrackingDetector final : public Engine::BasePlugin {
 public:
  explicit TrackingDetector(int& runCount) : run_count_(runCount) {}

  int init(
      const Engine::PluginArgs& /* args */,
      const PluginConstructionContext& /* context */) override {
    return 0;
  }

  Engine::PluginRet run(OomdContext& /* context */) override {
    ++run_count_;
    return Engine::PluginRet::CONTINUE;
  }

 private:
  int& run_count_;
};

Engine::PluginArgs matchArgs(
    std::string selector = ".",
    std::string allowlist = "cluster/owner/job/*",
    std::string denylist = "cluster/owner/blocked/*") {
  return {
      {"ruleset_cgroup", std::move(selector)},
      {"xattr", std::string(kTestXattr)},
      {"allowlist", std::move(allowlist)},
      {"denylist", std::move(denylist)},
  };
}

class XattrGlobTest : public ::testing::Test {
 protected:
  void SetUp() override {
    tempdir_ = Fixture::mkdtempChecked();
  }

  void TearDown() override {
    Fixture::rmrChecked(tempdir_);
  }

  void materialize(
      std::unordered_map<std::string, Fixture::DirEntry> children = {}) {
    children.insert(Fixture::makeFile("cgroup.procs", ""));
    children.insert(Fixture::makeFile("cgroup.type", "domain\n"));
    Fixture::materialize(Fixture::makeDir(tempdir_, std::move(children)));
  }

  void setXattr(const std::string& path, const std::string& value) {
    ASSERT_SYS_OK(Fs::setxattr(path, std::string(kTestXattr), value));
  }

  void selectRulesetCgroup() {
    context_.setRulesetCgroup(CgroupPath(tempdir_, ""));
  }

  std::unique_ptr<XattrGlob> detector(const Engine::PluginArgs& args) {
    auto result = std::make_unique<XattrGlob>();
    EXPECT_EQ(result->init(args, pluginContext()), 0);
    return result;
  }

  PluginConstructionContext pluginContext() const {
    return PluginConstructionContext(tempdir_);
  }

  OomdContext context_;
  std::string tempdir_;
};

} // namespace

TEST_F(XattrGlobTest, RegistersFactory) {
  std::unique_ptr<Engine::BasePlugin> plugin{
      getPluginRegistry().create("xattr_glob")};
  EXPECT_NE(plugin, nullptr);
}

TEST_F(XattrGlobTest, InitRequiresGenericBoundedSchema) {
  auto candidate = std::make_unique<XattrGlob>();
  EXPECT_EQ(candidate->init(matchArgs(), pluginContext()), 0);
  EXPECT_NE(candidate->init({}, pluginContext()), 0);

  auto missingSelector = matchArgs();
  missingSelector.erase("ruleset_cgroup");
  EXPECT_EQ(candidate->init(missingSelector, pluginContext()), 0);

  auto missingXattr = matchArgs();
  missingXattr.erase("xattr");
  EXPECT_NE(candidate->init(missingXattr, pluginContext()), 0);

  auto missingAllowlist = matchArgs();
  missingAllowlist.erase("allowlist");
  EXPECT_NE(candidate->init(missingAllowlist, pluginContext()), 0);

  auto emptyPattern = matchArgs(".", ",allowed");
  testing::internal::CaptureStderr();
  EXPECT_NE(candidate->init(emptyPattern, pluginContext()), 0);
  const auto invalidPatternLog = testing::internal::GetCapturedStderr();
  EXPECT_NE(
      invalidPatternLog.find("invalid xattr value match pattern"),
      std::string::npos);

  auto withDebug = matchArgs();
  withDebug["debug"] = "true";
  EXPECT_EQ(candidate->init(withDebug, pluginContext()), 0);

  auto extraArgument = matchArgs();
  extraArgument["unrecognized"] = "value";
  EXPECT_NE(candidate->init(extraArgument, pluginContext()), 0);
}

TEST_F(XattrGlobTest, InvalidReinitDoesNotRetainPolicy) {
  materialize();
  selectRulesetCgroup();
  setXattr(tempdir_, "cluster/owner/job/0");
  auto policy = detector(matchArgs());
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::CONTINUE);
  EXPECT_NE(policy->init({}, pluginContext()), 0);
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::STOP);
}

TEST_F(XattrGlobTest, MissingSelectionStopsTheDetectorGroup) {
  auto policy = detector(matchArgs());
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::STOP);
}

TEST_F(XattrGlobTest, DenialDoesNotStopPeerDetectors) {
  materialize();
  selectRulesetCgroup();
  int peerRunCount = 0;
  std::vector<std::unique_ptr<Engine::BasePlugin>> detectors;
  detectors.emplace_back(detector(matchArgs(".", "")));
  detectors.emplace_back(std::make_unique<TrackingDetector>(peerRunCount));
  Engine::DetectorGroup group("policy and peer", std::move(detectors));

  EXPECT_FALSE(group.check(context_, /* silenced_logs */ 0));
  EXPECT_EQ(peerRunCount, 1);
}

TEST_F(XattrGlobTest, MatchesWholeXattrValueAndDenyWins) {
  materialize();
  selectRulesetCgroup();
  setXattr(tempdir_, "cluster/owner/job/0");
  auto policy = detector(matchArgs());

  EXPECT_EQ(policy->run(context_), Engine::PluginRet::CONTINUE);
  setXattr(tempdir_, "cluster/owner/job");
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::STOP);
  setXattr(tempdir_, "cluster/owner/blocked/1");
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::STOP);
  setXattr(tempdir_, "other/owner/job/2");
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::STOP);
}

TEST_F(XattrGlobTest, RejectsInvalidXattrValue) {
  materialize();
  selectRulesetCgroup();
  setXattr(tempdir_, std::string("cluster/owner/job/0\0ignored", 27));
  auto policy = detector(matchArgs());
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::STOP);
}

TEST_F(XattrGlobTest, RulesetSelectorAllowsAnyMatchAndDenyWins) {
  materialize({Fixture::makeDir("a.service"), Fixture::makeDir("b.service")});
  selectRulesetCgroup();
  setXattr(tempdir_ + "/a.service", "cluster/owner/one/0");
  setXattr(tempdir_ + "/b.service", "cluster/owner/two/1");
  auto policy = detector(
      matchArgs("*.service", "cluster/owner/one/*", "cluster/owner/blocked/*"));

  EXPECT_EQ(policy->run(context_), Engine::PluginRet::CONTINUE);
  setXattr(tempdir_ + "/b.service", "cluster/owner/blocked/1");
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::STOP);
}

TEST_F(XattrGlobTest, RulesetSelectorRejectsMissingXattr) {
  materialize({Fixture::makeDir("a.service")});
  selectRulesetCgroup();
  auto policy = detector(matchArgs("*.service"));
  EXPECT_EQ(policy->run(context_), Engine::PluginRet::STOP);
}
