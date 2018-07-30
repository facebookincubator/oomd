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

#pragma once

#include <string>

#include <json/value.h>

#include "oomd/Oomd.h"
#include "oomd/plugins/Plugins.h"
#include "oomd/shared/Plugin.h"
#include "oomd/shared/Tunables.h"

namespace Oomd {

/*
 * This class parses the passed in JSON config file from @param
 * config_path as well as reading tunable environment variables.
 * @method apply can then be called to apply configuration settings
 * to an Oomd object.
 *
 * Important notes on backwards compatibility/versioning. To
 * simplify operation and rollouts, extreme care has been taken
 * to maintain config file backwards compatibility. Once the config
 * schema settles down, this extreme backwards compatibility can be
 * ripped out. As of now (5/18/18), it's crappy and I know. The versions
 * are as follows:
 *
 *    Version 0.1.0: Single-cgroup support with "target" and "kill_list"
 *                   attributes
 *    Version 0.1.1: Single-cgroup support with "target", "kill_list"
 *                   (with optional "kill_pressure"), "oomdetector", and
 *                   "oomkiller"
 *    Version 0.2.0: Multi-cgroup support with "cgroups" and "version",
 *                   where each cgroup config has the same attributes as
 *                   0.1.x.
 *
 * TODO: convert this hand rolled JSON parser to a thrift schema
 * so we have _more_ safety than this silly dynamically typed
 * Json::Value class.
 */
class Config {
 public:
  explicit Config(
      const std::string& config_path,
      bool dry = false,
      bool verbose = false);
  Json::Value parseJson(const std::string& config_path);

  void apply(Oomd& target);
  void applyCgroup(Oomd& target, Json::Value& cgroup);
  void apply_0_1_x(Oomd& target);
  void apply_0_2_x(Oomd& target);

  std::unique_ptr<Tunables> loadTunables();
  std::unique_ptr<KillList> parseKillList(Json::Value& cgroup);
  std::string parseBasePath(Json::Value& cgroup);

  std::unique_ptr<OomDetector> parseDetectorPluginAndFactory(
      const Json::Value& cgroup,
      const PluginArgs& args);
  std::unique_ptr<OomKiller> parseKillerPluginAndFactory(
      const Json::Value& cgroup,
      const PluginArgs& args);

 private:
  bool dry_{false};
  bool verbose_{false};
  Json::Value config_;
};

} // namespace Oomd
