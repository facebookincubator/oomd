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

#include "oomd/Stats.h"
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <json/reader.h>
#include <json/value.h>
#include <sys/socket.h>
#include <iostream>

#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

using namespace Oomd;
using namespace testing;

class StatsTest : public ::testing::Test {
 public:
  std::string socket_path;

 protected:
  std::unique_ptr<Stats> get_instance() {
    socket_path = "oomd/sample_dir/XXXXXX.socket";
    ::mkstemps(socket_path.data(), 7);
    return Stats::get_for_unittest(socket_path);
  }
};

TEST_F(StatsTest, BasicStats) {
  auto stats = get_instance();
  ASSERT_TRUE(stats);
  EXPECT_EQ(stats->getAll().size(), 0);

  // Increment and check
  EXPECT_EQ(stats->increment("one", 1), 0);
  EXPECT_EQ(stats->increment("two", 2), 0);
  EXPECT_EQ(stats->getAll().size(), 2);

  // Clear and check vals
  EXPECT_EQ(stats->reset(), 0);
  auto map = stats->getAll();
  EXPECT_EQ(map.size(), 2);
  for (auto& pair : map) {
    EXPECT_EQ(pair.second, 0);
  }

  // Increment with existing keys
  EXPECT_EQ(stats->increment("one", 1), 0);
  EXPECT_EQ(stats->set("two", 2), 0);
  EXPECT_EQ(stats->increment("two", 2), 0);
  map = stats->getAll();
  EXPECT_EQ(map.size(), 2);
  EXPECT_EQ(map["one"], 1);
  EXPECT_EQ(map["two"], 2 * 2);
}

TEST_F(StatsTest, InvalidSocketPath) {
  std::string invalid_path = "/var/";
  EXPECT_THROW(Oomd::Stats::get_for_unittest(invalid_path), std::runtime_error);
}

TEST_F(StatsTest, InvalidRequests) {
  auto stats_ptr = get_instance();
  ASSERT_NE(stats_ptr, nullptr);
  auto& stats = *(stats_ptr);
  {
    const std::string msg = "g";
    auto msg_opt = stats.msgSocket(msg);
    ASSERT_TRUE(msg_opt);
    auto& ret_msg = *msg_opt;
    Json::Value root;
    Json::Reader reader;
    ASSERT_TRUE(reader.parse(ret_msg, root));
    EXPECT_EQ(root["error"], 0);
  }
  {
    const std::string msg = "xlskdjfksdj";
    auto msg_opt = stats.msgSocket(msg);
    ASSERT_TRUE(msg_opt);
    auto& ret_msg = *msg_opt;
    Json::Value root;
    Json::Reader reader;
    ASSERT_TRUE(reader.parse(ret_msg, root));
    EXPECT_EQ(root["error"], 1);
  }
  {
    const std::string msg = "ysdf\nasdf";
    auto msg_opt = stats.msgSocket(msg);
    ASSERT_FALSE(msg_opt);
  }
  {
    const std::string msg = "zsdfasdfasdfasdfasdfasdfasdfadsfasdfasdf";
    auto msg_opt = stats.msgSocket(msg);
    ASSERT_FALSE(msg_opt);
  }
  {
    const std::string msg = "";
    auto msg_opt = stats.msgSocket(msg);
    ASSERT_TRUE(msg_opt);
    auto& ret_msg = *msg_opt;
    Json::Value root;
    Json::Reader reader;
    ASSERT_TRUE(reader.parse(ret_msg, root));
    EXPECT_EQ(root["error"], 1);
  }
}
