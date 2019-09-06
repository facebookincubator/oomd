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
#include "oomd/StatsClient.h"

#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

using namespace Oomd;
using namespace testing;

namespace {
std::optional<Json::Value> parseJson(const std::string& input) {
  Json::Value ret;
  std::string errs;
  Json::CharReaderBuilder rbuilder;
  std::istringstream sinput(input);

  bool ok = Json::parseFromStream(rbuilder, sinput, &ret, &errs);
  if (!ok) {
    std::cerr << "Unable to parse JSON: " << errs;
    return std::nullopt;
  }

  return ret;
}
} // namespace

class StatsTest : public ::testing::Test {
 public:
  std::string socket_path;

 protected:
  std::unique_ptr<Stats> get_instance() {
    socket_path = "/tmp/oomd-XXXXXX.socket";
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
  auto client = StatsClient(socket_path);
  {
    const std::string msg = "g";
    auto msg_opt = client.msgSocket(msg);
    ASSERT_TRUE(msg_opt);
    auto root = parseJson(*msg_opt);
    ASSERT_TRUE(root);
    EXPECT_EQ((*root)["error"], 0);
  }
  {
    const std::string msg = "xlskdjfksdj";
    auto msg_opt = client.msgSocket(msg);
    ASSERT_TRUE(msg_opt);
    auto root = parseJson(*msg_opt);
    ASSERT_TRUE(root);
    EXPECT_EQ((*root)["error"], 1);
  }
  {
    const std::string msg = "ysdf\nasdf";
    auto msg_opt = client.msgSocket(msg);
    ASSERT_FALSE(msg_opt);
  }
  {
    const std::string msg = "zsdfasdfasdfasdfasdfasdfasdfadsfasdfasdf";
    auto msg_opt = client.msgSocket(msg);
    ASSERT_FALSE(msg_opt);
  }
  {
    const std::string msg = "";
    auto msg_opt = client.msgSocket(msg);
    ASSERT_TRUE(msg_opt);
    auto root = parseJson(*msg_opt);
    ASSERT_TRUE(root);
    EXPECT_EQ((*root)["error"], 1);
  }
}

TEST_F(StatsTest, BasicGetAndClear) {
  auto stats = get_instance();

  auto client = StatsClient(socket_path);
  EXPECT_EQ(client.getStats()->size(), 0);
  int limit = 3;
  {
    // Stats increments, Client Gets and checks
    for (int i = 0; i < limit; i++) {
      EXPECT_EQ(stats->increment(std::to_string(i), i), 0);
    }
    EXPECT_EQ(stats->getAll().size(), limit);
    auto client_map_ptr = client.getStats();
    ASSERT_TRUE(client_map_ptr);
    auto& client_map = *client_map_ptr;
    EXPECT_EQ(client_map.size(), limit);
    for (int i = 0; i < limit; i++) {
      EXPECT_EQ(client_map[std::to_string(i)], i);
    }
  }
  {
    // Client Clears, Stats/Client Gets and checks
    EXPECT_EQ(client.resetStats(), 0);
    auto stats_map = stats->getAll();
    EXPECT_EQ(stats_map.size(), limit);
    for (int i = 0; i < limit; i++) {
      EXPECT_EQ(stats_map.at(std::to_string(i)), 0);
    }
    auto client_map_ptr = client.getStats();
    ASSERT_TRUE(client_map_ptr);
    auto& client_map = *client_map_ptr;
    EXPECT_EQ(client_map.size(), limit);
    for (int i = 0; i < limit; i++) {
      EXPECT_EQ(stats_map.at(std::to_string(i)), 0);
    }
  }
}

TEST_F(StatsTest, LongJson) {
  auto stats = get_instance();

  auto client = StatsClient(socket_path);
  EXPECT_EQ(client.getStats()->size(), 0);
  int limit = 10000;
  {
    // Stats increments, Client Gets and checks
    for (int i = 0; i < limit; i++) {
      EXPECT_EQ(stats->increment(std::to_string(i), i), 0);
    }
    EXPECT_EQ(stats->getAll().size(), limit);
    auto client_map_ptr = client.getStats();
    ASSERT_TRUE(client_map_ptr);
    auto& client_map = *client_map_ptr;
    EXPECT_EQ(client_map.size(), limit);
    for (int i = 0; i < limit; i++) {
      EXPECT_EQ(client_map[std::to_string(i)], i);
    }
  }
  {
    // Client Clears, Stats/Client Gets and checks
    EXPECT_EQ(client.resetStats(), 0);
    auto stats_map = stats->getAll();
    EXPECT_EQ(stats_map.size(), limit);
    for (int i = 0; i < limit; i++) {
      EXPECT_EQ(stats_map.at(std::to_string(i)), 0);
    }
    auto client_map_ptr = client.getStats();
    ASSERT_TRUE(client_map_ptr);
    auto& client_map = *client_map_ptr;
    EXPECT_EQ(client_map.size(), limit);
    for (int i = 0; i < limit; i++) {
      EXPECT_EQ(stats_map.at(std::to_string(i)), 0);
    }
  }
}
