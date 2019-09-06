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

#include "oomd/OomdContext.h"
#include "oomd/plugins/systemd/BaseSystemdPlugin.h"
#include "oomd/plugins/systemd/SystemdRestart.h"

using namespace Oomd;
using namespace testing;

namespace Oomd {
class BaseSystemdPluginMock : public BaseSystemdPlugin {
 public:
  bool restartService(const std::string& service) override {
    restarted = service;
    return true;
  }
  bool stopService(const std::string& service) override {
    stopped = service;
    return true;
  }
  bool talkToSystemdManager(
      const std::string& /* unused */,
      const std::string& /* unused */,
      const std::string& /* unused */) override {
    return true;
  }

  std::string restarted;
  std::string stopped;
};
} // namespace Oomd

TEST(SystemdRestart, RestartService) {
  auto plugin = std::make_shared<SystemdRestart<BaseSystemdPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["service"] = "some.service";
  args["post_action_delay"] = "0";
  args["dry"] = "false";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->restarted, "some.service");
}

TEST(SystemdRestart, RestartServiceDry) {
  auto plugin = std::make_shared<SystemdRestart<BaseSystemdPluginMock>>();
  ASSERT_NE(plugin, nullptr);

  Engine::MonitoredResources resources;
  Engine::PluginArgs args;
  args["service"] = "some.service";
  args["post_action_delay"] = "0";
  args["dry"] = "true";

  ASSERT_EQ(plugin->init(resources, std::move(args)), 0);

  OomdContext ctx;
  EXPECT_EQ(plugin->run(ctx), Engine::PluginRet::STOP);
  EXPECT_EQ(plugin->restarted.size(), 0);
}
