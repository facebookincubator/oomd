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

#include <sstream>

#include <json/reader.h>

#include "oomd/Log.h"
#include "oomd/util/Fs.h"

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

Oomd::Config2::IR::Detector parseDetector(const Json::Value& detector) {
  Oomd::Config2::IR::Detector ir_detector;
  if (!detector.isArray()) {
    return {};
  }

  for (int j = 0; j < detector.size(); ++j) {
    if (j == 0 && detector[j].isString()) {
      ir_detector.name = detector[j].asString();
      continue;
    }

    // else: all other indicies (ie args)
    if (!detector[j].isString()) {
      continue;
    }

    auto args = Oomd::Fs::split(detector[j].asString(), '=');
    if (args.size() < 2) {
      continue;
    }
    ir_detector.args[args[0]] = std::move(args[1]);
  }

  return ir_detector;
}

Oomd::Config2::IR::DetectorGroup parseDetectorGroup(
    const Json::Value& detector_group) {
  Oomd::Config2::IR::DetectorGroup ir_detectorgroup;
  if (!detector_group.isArray()) {
    return {};
  }

  for (int i = 0; i < detector_group.size(); ++i) {
    if (i == 0 && detector_group[i].isString()) {
      ir_detectorgroup.name = detector_group[i].asString();
      continue;
    }

    // else: all other indicies (ie detectors)
    const auto& detector = detector_group[i];
    ir_detectorgroup.detectors.emplace_back(parseDetector(detector));
  }

  return ir_detectorgroup;
}

Oomd::Config2::IR::Action parseAction(const Json::Value& action) {
  Oomd::Config2::IR::Action ir_action;
  if (!action.isArray()) {
    return {};
  }

  for (int i = 0; i < action.size(); ++i) {
    if (i == 0 && action[i].isString()) {
      ir_action.name = action[i].asString();
      continue;
    }

    // else: all other indicies are args
    if (!action[i].isString()) {
      continue;
    }

    auto args = Oomd::Fs::split(action[i].asString(), '=');
    if (args.size() < 2) {
      continue;
    }
    ir_action.args[args[0]] = std::move(args[1]);
  }

  return ir_action;
}

Oomd::Config2::IR::Ruleset parseRuleset(const Json::Value& ruleset) {
  Oomd::Config2::IR::Ruleset ir_ruleset;

  ir_ruleset.name = ruleset.get("name", "").asString();

  for (const auto& detector_group : ruleset.get("detectors", {})) {
    ir_ruleset.dgs.emplace_back(parseDetectorGroup(detector_group));
  }

  for (const auto& action : ruleset.get("actions", {})) {
    ir_ruleset.acts.emplace_back(parseAction(action));
  }

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

  ir_root->version = json_root.get("version", "").asString();

  IR::dumpIR(*ir_root);
  return ir_root;
}

} // namespace Config2
} // namespace Oomd
