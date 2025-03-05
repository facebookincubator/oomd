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

#include "oomd/config/JsonConfigParser.h"

#include <optional>
#include <sstream>

#include <json/reader.h>

#include "oomd/Log.h"

namespace {

void getJson(Json::Value& root, const std::string& input) {
  std::string errs;
  Json::CharReaderBuilder rbuilder;
  rbuilder["allowComments"] = true;
  rbuilder["collectComments"] = false;
  std::istringstream sinput(input);

  bool ok = Json::parseFromStream(rbuilder, sinput, &root, &errs);
  if (!ok) {
    OLOG << "Unable to parse JSON: " << errs;
    throw std::runtime_error("Unable to parse JSON");
  }
}

template <typename T>
T parsePlugin(const Json::Value& plugin) {
  if (!plugin.isObject()) {
    return {};
  }

  const auto& name = plugin["name"];
  if (!name.isString()) {
    return {};
  }

  T ret;
  ret.name = name.asString();

  const auto& json_args = plugin["args"];
  if (!json_args.isObject()) {
    return ret;
  }

  for (const auto& key : json_args.getMemberNames()) {
    const auto& value = json_args[key];
    // Value has to be a string, number, or bool
    if (!value.isString() && !value.isNumeric() && !value.isBool()) {
      return ret;
    }
    ret.args[key] = value.asString();
  }

  return ret;
}

Oomd::Config2::IR::DetectorGroup parseDetectorGroup(
    const Json::Value& detector_group) {
  Oomd::Config2::IR::DetectorGroup ir_detectorgroup;
  if (!detector_group.isArray()) {
    return {};
  }

  for (Json::ArrayIndex i = 0; i < detector_group.size(); ++i) {
    if (i == 0 && detector_group[i].isString()) {
      ir_detectorgroup.name = detector_group[i].asString();
      continue;
    }

    // else: all other indicies (ie detectors)
    const auto& detector = detector_group[i];
    ir_detectorgroup.detectors.emplace_back(
        parsePlugin<Oomd::Config2::IR::Detector>(detector));
  }

  return ir_detectorgroup;
}

Oomd::Config2::IR::DropIn parseDropIn(const Json::Value& dropin) {
  Oomd::Config2::IR::DropIn ir_dropin;

  ir_dropin.disable_on_drop_in =
      dropin.get("disable-on-drop-in", false).asBool();
  ir_dropin.detectorgroups_enabled = dropin.get("detectors", false).asBool();
  ir_dropin.actiongroup_enabled = dropin.get("actions", false).asBool();

  return ir_dropin;
}

Oomd::Config2::IR::Ruleset parseRuleset(const Json::Value& ruleset) {
  Oomd::Config2::IR::Ruleset ir_ruleset;

  ir_ruleset.name = ruleset.get("name", "").asString();

  ir_ruleset.dropin = parseDropIn(ruleset.get("drop-in", {}));

  ir_ruleset.silence_logs = ruleset.get("silence-logs", {}).asString();

  ir_ruleset.post_action_delay =
      ruleset.get("post_action_delay", {}).asString();

  ir_ruleset.prekill_hook_timeout =
      ruleset.get("prekill_hook_timeout", {}).asString();

  for (const auto& detector_group : ruleset.get("detectors", {})) {
    ir_ruleset.dgs.emplace_back(parseDetectorGroup(detector_group));
  }

  for (const auto& action : ruleset.get("actions", {})) {
    ir_ruleset.acts.emplace_back(
        parsePlugin<Oomd::Config2::IR::Action>(action));
  }

  ir_ruleset.xattr_filter = ruleset.get("xattr_filter", {}).asString();
  ir_ruleset.cgroup = ruleset.get("cgroup", {}).asString();

  return ir_ruleset;
}

} // namespace

namespace Oomd {
namespace Config2 {

std::unique_ptr<IR::Root> JsonConfigParser::parse(const std::string& input) {
  Json::Value json_root;
  getJson(json_root, input);
  auto ir_root = std::make_unique<IR::Root>();

  for (const auto& ruleset : json_root.get("rulesets", {})) {
    ir_root->rulesets.emplace_back(parseRuleset(ruleset));
  }

  for (const auto& prekill_hook : json_root.get("prekill_hooks", {})) {
    ir_root->prekill_hooks.emplace_back(
        parsePlugin<Oomd::Config2::IR::PrekillHook>(prekill_hook));
  }

  IR::dumpIR(*ir_root);
  return ir_root;
}

} // namespace Config2
} // namespace Oomd
