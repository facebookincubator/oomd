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

#include <fstream>
#include <memory>

#include "oomd/config/JsonConfigParser.h"

using namespace Oomd::Config2;
using namespace testing;

constexpr auto kConfig_1_0_0 = "oomd/fixtures/oomd_1_0_0.json";

TEST(JsonConfigParserTest, LoadIR) {
  std::ifstream file(kConfig_1_0_0, std::ios::in);
  ASSERT_TRUE(file.is_open());
  std::ostringstream buffer;
  buffer << file.rdbuf();

  // Make sure the IR was generated
  JsonConfigParser parser;
  auto root = parser.parse(buffer.str());
  ASSERT_TRUE(root);

  // Check root values
  ASSERT_EQ(root->rulesets.size(), 2);
  EXPECT_EQ(root->version, "1.0.0");

  // Check first ruleset
  const auto& first = root->rulesets[0];
  EXPECT_EQ(first.name, "my first ruleset");
  ASSERT_EQ(first.dgs.size(), 1);

  // Check first ruleset's first DetectorGroup
  const auto& first_dg = first.dgs[0];
  EXPECT_EQ(first_dg.name, "group1");
  ASSERT_EQ(first_dg.detectors.size(), 2);
  EXPECT_EQ(first_dg.detectors[0].name, "pressure_rising_beyond");
  EXPECT_EQ(first_dg.detectors[0].args.at("cgroup"), "workload.slice");
  EXPECT_EQ(first_dg.detectors[0].args.at("resource"), "memory");
  EXPECT_EQ(first_dg.detectors[0].args.at("threshold"), "5");
  EXPECT_EQ(first_dg.detectors[1].name, "pressure_rising_beyond");
  EXPECT_EQ(first_dg.detectors[1].args.at("cgroup"), "system.slice");
  EXPECT_EQ(first_dg.detectors[1].args.at("resource"), "memory");
  EXPECT_EQ(first_dg.detectors[1].args.at("threshold"), "40");

  // Check first ruleset's actions
  ASSERT_EQ(first.acts.size(), 1);
  EXPECT_EQ(first.acts[0].name, "kill_by_memory_size_or_growth");
  EXPECT_EQ(first.acts[0].args.at("cgroup"), "system.slice");

  // Check second ruleset
  const auto& second = root->rulesets[1];
  EXPECT_EQ(second.name, "low swap ruleset");
  ASSERT_EQ(second.dgs.size(), 1);

  // Check second ruleset's first DetectorGroup
  const auto& second_dg = second.dgs[0];
  EXPECT_EQ(second_dg.name, "my other group");
  ASSERT_EQ(second_dg.detectors.size(), 1);
  EXPECT_EQ(second_dg.detectors[0].name, "swap_free");
  EXPECT_EQ(second_dg.detectors[0].args.at("threshold_pct"), "15");

  // Check second ruleset's actions
  ASSERT_EQ(second.acts.size(), 3);
  EXPECT_EQ(second.acts[0].name, "kill_by_swap_usage");
  EXPECT_EQ(second.acts[0].args.at("cgroup"), "system.slice");
  EXPECT_EQ(second.acts[1].name, "kill_by_swap_usage");
  EXPECT_EQ(
      second.acts[1].args.at("cgroup"), "workload.slice/workload-wdb.slice");
  EXPECT_EQ(second.acts[2].name, "kill_by_swap_usage");
  EXPECT_EQ(
      second.acts[2].args.at("cgroup"), "workload.slice/workload-tw.slice");
}

TEST(JsonConfigParserTest, LoadIRBadInput) {
  JsonConfigParser parser;
  ASSERT_THROW(parser.parse("not a json string"), std::runtime_error);
}
