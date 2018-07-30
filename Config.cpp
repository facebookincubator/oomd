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

#include "oomd/Config.h"

#include <fstream>
#include <limits>
#include <memory>
#include <sstream>
#include <utility>
#include <vector>

#include <json/reader.h>

#include "oomd/PluginRegistry.h"
#include "oomd/shared/KillList.h"

static auto constexpr kCgroupBase = "/sys/fs/cgroup/";

namespace Oomd {

// We have to register the default plugins here to avoid a circular
// dependency inside PluginRegistry. PluginRegistry depends on
// OomDetector and OomKiller. That means we cannot use PluginRegistry
// inside OomDetector and OomKiller.
REGISTER_DETECTOR_PLUGIN(default, OomDetector::create);
REGISTER_KILLER_PLUGIN(default, OomKiller::create);

Config::Config(const std::string& config_path, bool dry, bool verbose)
    : dry_(dry), verbose_(verbose) {
  config_ = parseJson(config_path);
}

Json::Value Config::parseJson(const std::string& config_path) {
  std::ifstream file(config_path, std::ios::in);
  if (!file.is_open()) {
    XLOG(FATAL) << "Unable to read config file at " << config_path;
  }
  std::stringstream buffer;
  buffer << file.rdbuf();
  Json::CharReaderBuilder rbuilder;
  rbuilder["allowComments"] = true;
  rbuilder["collectComments"] = false;
  Json::Value root;
  std::string errs;
  bool ok = Json::parseFromStream(rbuilder, buffer, &root, &errs);
  if (!ok) {
    XLOG(FATAL) << "Unable to parse JSON from config file at " << config_path;
  }
  return root;
}

void Config::apply(Oomd& target) {
  // Set cmd line args
  target.setVerbose(verbose_);

  // Parse and set config file
  std::string version_str;
  auto version = config_["version"];
  if (version != Json::nullValue) {
    version_str = config_["version"].asString();
    if (version_str == "0.2.0") {
      apply_0_2_x(target);
    } else {
      XLOG(FATAL) << "Unsupported version: " << version_str;
    }
  } else {
    // Default is 0.1.x config. Why? It's the way it is in prod.
    apply_0_1_x(target);
  }
}

void Config::applyCgroup(Oomd& target, Json::Value& cgroup) {
  auto target_cgroup = parseBasePath(cgroup);
  std::shared_ptr<Tunables> tunables(loadTunables());
  std::shared_ptr<KillList> kill_list(parseKillList(cgroup));
  PluginArgs args{target_cgroup, kill_list, tunables, dry_};

  auto detector = parseDetectorPluginAndFactory(cgroup, args);
  auto killer = parseKillerPluginAndFactory(cgroup, args);

  target.setTunables(tunables);
  target.addCgroup(std::move(detector), std::move(killer));
}

void Config::apply_0_1_x(Oomd& target) {
  applyCgroup(target, config_);
}

void Config::apply_0_2_x(Oomd& target) {
  for (auto& cgroup : config_["cgroups"]) {
    applyCgroup(target, cgroup);
  }
}

std::string Config::parseBasePath(Json::Value& cgroup) {
  try {
    auto target = std::string(kCgroupBase) + cgroup["target"].asString();
    XLOG(INFO) << "target_=" << target;
    return target;
  } catch (const std::exception& ex) {
    XLOG(FATAL) << "Failed to parse config file: " << ex.what();
  }
}

std::unique_ptr<KillList> Config::parseKillList(Json::Value& cgroup) {
  try {
    auto kill_list = std::make_unique<KillList>();
    for (auto& entry : cgroup["kill_list"]) {
      for (auto& svc : entry.getMemberNames()) {
        const auto& attrs = entry[svc];
        int kill_pressure{std::numeric_limits<int>::max()};
        int max_usage{0};

        // For config backwards compatibility. If .asString() throws an
        // exception, then we know that the new style config is being
        // used, the old style being the following:
        //    {"service_name" : "12345" }
        // This is because .asString() cannot be called on proper array or map
        // types, only values that are already booleans, numbers, or strings.
        //
        try {
          std::string max_usage_string = attrs.asString();
          // jsoncpp only allows .asInt() for integral values, which is
          // why we first cast to a string and use stoi() to parse.
          //
          max_usage = (max_usage_string == "inf")
              ? std::numeric_limits<int>::max()
              : std::stoi(max_usage_string);
          kill_list->emplace_back(KillListEntry{svc, kill_pressure, max_usage});
          break;
        } catch (const std::exception& ex) {
        }

        // Parse kill_pressure, default is disabled pressure killing (infinity)
        if (attrs.isMember("kill_pressure")) {
          std::string kill_pressure_string = attrs["kill_pressure"].asString();
          if (kill_pressure_string != "inf") {
            kill_pressure = std::stoi(kill_pressure_string);
          }
        }

        // Parse max_usage, default is always kill (0)
        if (attrs.isMember("max_usage")) {
          std::string max_usage_string = attrs["max_usage"].asString();
          if (max_usage_string == "inf") {
            max_usage = std::numeric_limits<int>::max();
          } else {
            max_usage = std::stoi(max_usage_string);
          }
        }

        kill_list->emplace_back(KillListEntry{svc, kill_pressure, max_usage});
        break; // only 1 entry in each kill_list dict
      }
    }

    for (auto& kl_entry : *kill_list) {
      XLOG(INFO) << "Kill list: service=" << kl_entry.service
                 << " kill_pressure= " << kl_entry.kill_pressure
                 << " max_usage=" << kl_entry.max_usage << "MB";
    }

    return kill_list;
  } catch (const std::exception& ex) {
    XLOG(FATAL) << "Failed to parse config file: " << ex.what();
  }
}

std::unique_ptr<Tunables> Config::loadTunables() {
  auto tunables = std::make_unique<Tunables>();
  tunables->parseEnvVars();
  tunables->loadOverrides();
  tunables->dump();
  return tunables;
}

std::unique_ptr<OomDetector> Config::parseDetectorPluginAndFactory(
    const Json::Value& cgroup,
    const PluginArgs& args) {
  std::string config_class = cgroup["oomdetector"].asString();
  const char* chosen_class = config_class.size() ? config_class.c_str() : "default";
  XLOG(INFO) << "OomDetector=" << chosen_class;
  OomDetector* d = getDetectorRegistry().create(chosen_class, args);
  CHECK(!!d);
  return std::unique_ptr<OomDetector>(d);
}

std::unique_ptr<OomKiller> Config::parseKillerPluginAndFactory(
    const Json::Value& cgroup,
    const PluginArgs& args) {
  std::string config_class = cgroup["oomkiller"].asString();
  const char* chosen_class = config_class.size() ? config_class.c_str() : "default";
  XLOG(INFO) << "OomKiller=" << chosen_class;
  OomKiller* k = getKillerRegistry().create(chosen_class, args);
  CHECK(!!k);
  return std::unique_ptr<OomKiller>(k);
}

} // namespace Oomd
