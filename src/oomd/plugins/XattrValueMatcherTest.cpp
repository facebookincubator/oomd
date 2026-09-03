/*
 * Copyright (C) 2026-present, Meta Platforms, Inc. and affiliates
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 */

#include "oomd/plugins/XattrValueMatcher.h" // @manual=:xattr_value_matcher

#include <memory>

#include <gtest/gtest.h>

#include "oomd/util/Fixture.h"
#include "oomd/util/Fs.h"
#include "oomd/util/TestHelper.h"

using namespace Oomd;

namespace {
constexpr std::string_view kTestXattr{"user.oomd_value"};

std::shared_ptr<const XattrValueMatcher> matcher(
    const std::vector<std::string>& allowlist,
    const std::vector<std::string>& denylist = {}) {
  return XattrValueMatcherProvider::compile(allowlist, denylist);
}
} // namespace

TEST(XattrValueMatcherTest, DefaultsFailClosedAndNamesUnknownReasons) {
  const XattrValueMatchDecision decision;
  EXPECT_FALSE(decision.allowed());
  EXPECT_EQ(decision.reason, XattrValueMatchReason::XATTR_READ_ERROR);
  EXPECT_EQ(
      xattrValueMatchReasonName(static_cast<XattrValueMatchReason>(255)),
      "unknown");
}

TEST(XattrValueMatcherTest, MatchesWholeValueAndDenyWins) {
  auto policy = matcher({"cluster/owner/job-?"}, {"cluster/owner/job-denied"});
  EXPECT_TRUE(policy->evaluateValue("cluster/owner/job-a").allowed());
  EXPECT_FALSE(policy->evaluateValue("prefix-cluster/owner/job-a").allowed());
  EXPECT_FALSE(policy->evaluateValue("cluster/owner/job-aa").allowed());

  auto denied = policy->evaluateValue("cluster/owner/job-denied");
  EXPECT_EQ(denied.reason, XattrValueMatchReason::DENYLIST_MATCH);
  EXPECT_EQ(denied.pattern_index, 0);
  EXPECT_EQ(
      policy->evaluateValue("cluster/other/job-ok").reason,
      XattrValueMatchReason::NOT_ALLOWLISTED);
}

TEST(XattrValueMatcherTest, GlobSyntaxIsGenericAndCaseSensitive) {
  auto policy = matcher({"cluster/*/job-?", "cluster/owner/job.*"});
  EXPECT_TRUE(policy->evaluateValue("cluster/team/child/job-a").allowed());
  EXPECT_FALSE(policy->evaluateValue("cluster/owner/job-aa").allowed());
  EXPECT_TRUE(policy->evaluateValue("cluster/owner/job.value").allowed());
  EXPECT_FALSE(policy->evaluateValue("cluster/owner/jobXvalue").allowed());
  EXPECT_FALSE(policy->evaluateValue("Cluster/owner/job-a").allowed());
  EXPECT_TRUE(policy->evaluateValue("cluster/owner/team/job-a").allowed());
}

TEST(XattrValueMatcherTest, IdenticalAllowAndDenyPatternUsesDeny) {
  auto policy = matcher({R"(cluster/owner/job)"}, {R"(cluster/owner/job)"});
  EXPECT_EQ(
      policy->evaluateValue("cluster/owner/job").reason,
      XattrValueMatchReason::DENYLIST_MATCH);
}

TEST(XattrValueMatcherTest, EmptyAllowlistIsDefaultOff) {
  EXPECT_EQ(
      matcher({})->evaluateValue("cluster/owner/job").reason,
      XattrValueMatchReason::NOT_ALLOWLISTED);
}

TEST(XattrValueMatcherTest, CommaSeparatedPatternListsPreserveOrder) {
  auto policy = XattrValueMatcherProvider::compileCommaSeparated(
      "cluster/*/job,cluster/owner/*", "cluster/owner/blocked");
  EXPECT_EQ(policy->evaluateValue("cluster/owner/blocked").pattern_index, 0);

  auto allowed = policy->evaluateValue("cluster/owner/allowed");
  EXPECT_TRUE(allowed.allowed());
  EXPECT_EQ(allowed.pattern_index, 1);
}

TEST(XattrValueMatcherTest, RejectsInvalidCommaSeparatedBounds) {
  EXPECT_THROW(
      XattrValueMatcherProvider::compileCommaSeparated(",allowed", ""),
      std::invalid_argument);
  EXPECT_THROW(
      XattrValueMatcherProvider::compileCommaSeparated("allowed,", ""),
      std::invalid_argument);
  EXPECT_THROW(
      XattrValueMatcherProvider::compileCommaSeparated(
          std::string(65537, 'a'), ""),
      std::invalid_argument);
  EXPECT_THROW(matcher({""}), std::invalid_argument);
  EXPECT_THROW(
      matcher({std::string("bad\0pattern", 11)}), std::invalid_argument);
  EXPECT_THROW(matcher({std::string(513, 'a')}), std::invalid_argument);
  EXPECT_THROW(
      matcher(std::vector<std::string>(257, "value")), std::invalid_argument);
}

class XattrValueMatcherScopeTest : public ::testing::Test {
 protected:
  void SetUp() override {
    tempdir_ = Fixture::mkdtempChecked();
  }

  void TearDown() override {
    Fixture::rmrChecked(tempdir_);
  }

  void materialize() {
    Fixture::materialize(Fixture::makeDir(tempdir_, {}));
  }

  void setXattr(const std::string& value) {
    ASSERT_SYS_OK(Fs::setxattr(tempdir_, std::string(kTestXattr), value));
  }

  SystemMaybe<Fs::DirFd> openRoot() {
    return Fs::DirFd::open(tempdir_);
  }

  std::string tempdir_;
};

TEST_F(XattrValueMatcherScopeTest, CgroupEvaluationFailsClosed) {
  materialize();
  auto root = ASSERT_SYS_OK(openRoot());
  auto policy = matcher({"cluster/owner/*"});
  EXPECT_EQ(
      policy->evaluateCgroup(root, kTestXattr).reason,
      XattrValueMatchReason::XATTR_MISSING);

  auto invalidXattr = XattrValueMatcher::readValueAt(root, "user.");
  ASSERT_FALSE(invalidXattr);
  ASSERT_EQ(invalidXattr.error().code().value(), EINVAL);
  EXPECT_EQ(
      policy->evaluateCgroup(root, "user.").reason,
      XattrValueMatchReason::XATTR_READ_ERROR);

  auto emptyXattr = XattrValueMatcher::readValueAt(root, "");
  ASSERT_FALSE(emptyXattr);
  ASSERT_EQ(emptyXattr.error().code().value(), EINVAL);

  std::string nulXattr("user.oomd\0value", 15);
  auto badXattr = XattrValueMatcher::readValueAt(root, nulXattr);
  ASSERT_FALSE(badXattr);
  ASSERT_EQ(badXattr.error().code().value(), EINVAL);

  setXattr("cluster/owner/job/0");
  EXPECT_TRUE(policy->evaluateCgroup(root, kTestXattr).allowed());

  setXattr(std::string("cluster/owner/job/0\0ignored", 27));
  EXPECT_EQ(
      policy->evaluateCgroup(root, kTestXattr).reason,
      XattrValueMatchReason::XATTR_VALUE_INVALID);
}
