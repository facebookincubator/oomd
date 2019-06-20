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

#include "oomd/config/CompactConfigParser.h"

#include <cstring>
#include <exception>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "oomd/Log.h"
#include "oomd/include/Assert.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace {

enum class ParseState {
  INIT = 0,
  SECTION_HEADER,
  SECTION,
  SECTION_ARGUMENTS_HEADER,
  SECTION_ARGUMENTS,
};

struct ParsedSection {
  bool isInit{false}; // true if struct has been populated
  std::string name;
  std::string plugin;
  bool isDetector{true}; // false implies action
  std::unordered_map<std::string, std::string> args;
  std::vector<std::string> conditions; // only valid for actions
  std::string chainedafter; // only valid for actions
};

bool isArgumentHeader(const std::string& line) {
  auto l = line; // copy
  Oomd::Util::strip(l);
  return l == "[[args]]";
}

bool isSectionHeader(const std::string& line) {
  auto l = line; // copy
  Oomd::Util::strip(l);
  if (l.size() < 2) {
    return false;
  }

  if (l[0] == '[' && l[l.size() - 1] == ']' && !isArgumentHeader(line)) {
    return true;
  }

  return false;
}

/*
 * Precondition is that @param line is a well formed section header
 */
std::string parseSectionName(const std::string& line) {
  OCHECK_EXCEPT(line.size() >= 2, std::runtime_error("Invalid section header"));
  auto l = line;
  l.erase(0, 1);
  l.pop_back();
  return l;
}

std::optional<std::pair<std::string, std::string>> parseDirective(
    const std::string& line) {
  auto parts = Oomd::Util::split(line, '=');
  if (parts.size() != 2) {
    OLOG << "Invalid section directive=\"" << line << "\"";
    return std::nullopt;
  }

  for (auto& part : parts) {
    Oomd::Util::strip(part);
  }

  return std::make_pair<std::string, std::string>(
      std::move(parts[0]), std::move(parts[1]));
}

bool isSectionValid(const ParsedSection& section) {
  // Required fields
  if (!section.isInit) {
    OLOG << "Section=" << section.name << " is not initialized by parser";
    return false;
  }

  if (section.name.empty()) {
    OLOG << "Section mission name";
    return false;
  }

  if (section.plugin.empty()) {
    OLOG << "Section=" << section.name << " missing plugin";
    return false;
  }

  // Check that conditions are only used with actions
  if (section.isDetector && section.conditions.size()) {
    OLOG << "Cannot use conditions with detector in section=" << section.name;
    return false;
  }

  // Check that chainedafter diretives are only used with actions
  if (section.isDetector && section.chainedafter.size()) {
    OLOG << "Cannot use chainedafter with detector in section=" << section.name;
    return false;
  }

  // Check that either chainedafter or conditions directive is used
  if (!section.isDetector &&
      (section.chainedafter.empty() == section.conditions.empty())) {
    OLOG << "Must use directive (chainedafter XOR condition) in section="
         << section.name;
    return false;
  }

  return true;
}

std::optional<std::vector<ParsedSection>> parseConfig(
    const std::string& input) {
  std::vector<ParsedSection> ret;
  ParseState state = ParseState::INIT;
  auto lines = Oomd::Util::split(input, '\n');
  ParsedSection section;

  for (const auto& line : lines) {
    // Don't process blank lines
    if (Oomd::Util::isBlank(line)) {
      continue;
    }

    // Perform state transitions as necessary
    if (isSectionHeader(line)) {
      // Only valid to transition from INIT or SECTION_ARGUMENTS
      if (state != ParseState::INIT && state != ParseState::SECTION_ARGUMENTS) {
        OLOG << "Unexpected section header=\"" << line << "\". "
             << "Did you remember the section arguments?";
        return std::nullopt;
      }
      state = ParseState::SECTION_HEADER;
    } else if (isArgumentHeader(line)) {
      // Only valid to transition from SECTION state
      if (state != ParseState::SECTION) {
        OLOG << "Missing section header for argument list=\"" << line << "\"";
        return std::nullopt;
      }
      state = ParseState::SECTION_ARGUMENTS_HEADER;
    } else if (state == ParseState::SECTION_HEADER) {
      state = ParseState::SECTION;
    } else if (state == ParseState::SECTION_ARGUMENTS_HEADER) {
      state = ParseState::SECTION_ARGUMENTS;
    }

    // Handle each state.
    //
    // Don't do any state transitions down here. Do them above with the
    // other state transitions.
    switch (state) {
      case ParseState::INIT:
        // Invalid state
        OLOG << "Could not find initial section header";
        return std::nullopt;
        break;
      case ParseState::SECTION_HEADER:
        // Store previous section
        if (section.isInit) {
          ret.emplace_back(std::move(section));
        }
        section = {};
        section.isInit = true;
        section.name = parseSectionName(line);
        break;
      case ParseState::SECTION: {
        auto pair = parseDirective(line);
        if (!pair.has_value()) {
          OLOG << "Invalid directive format in section=" << section.name;
          return std::nullopt;
        }

        if (pair->first == "detector") {
          section.isDetector = true;
          section.plugin = std::move(pair->second);
        } else if (pair->first == "action") {
          section.isDetector = false;
          section.plugin = std::move(pair->second);
        } else if (pair->first == "condition") {
          section.conditions.emplace_back(std::move(pair->second));
        } else if (pair->first == "chainedafter") {
          section.chainedafter = std::move(pair->second);
        } else {
          OLOG << "Unrecognized directive=" << pair->first;
          return std::nullopt;
        }
      } break;
      case ParseState::SECTION_ARGUMENTS_HEADER:
        break;
      case ParseState::SECTION_ARGUMENTS: {
        auto pair = parseDirective(line);
        if (!pair.has_value()) {
          OLOG << "Invalid argument format=\"" << line
               << "\" in section=" << section.name;
          return std::nullopt;
        }

        section.args[std::move(pair->first)] = std::move(pair->second);
      } break;

        // No default to protect against any future states
    }
  }

  // Grab final section
  ret.emplace_back(std::move(section));

  return ret;
}

std::unique_ptr<Oomd::Config2::IR::Root> constructIR(
    const std::vector<ParsedSection>& sections) {
  auto ir_root = std::make_unique<Oomd::Config2::IR::Root>();

  // First create a handy factory map that we can use to create more detectors
  std::unordered_map<std::string, Oomd::Config2::IR::Detector> detectors;
  for (const auto& section : sections) {
    if (!section.isDetector) {
      continue;
    }

    // Error check that we have no duplicate detector section names
    if (detectors.find(section.name) != detectors.end()) {
      OLOG << "Duplicate section=" << section.name << " detected";
      return nullptr;
    }

    Oomd::Config2::IR::Detector detector;
    detector.name = section.plugin;
    detector.args = section.args;
    detectors[section.name] = std::move(detector);
  }

  // Now start processing actions
  std::unordered_map<std::string, Oomd::Config2::IR::Ruleset> actions;
  for (const auto& section : sections) {
    if (section.isDetector) {
      continue;
    }

    // Error check that we have no duplicate names
    if (actions.find(section.name) != actions.end()) {
      OLOG << "Duplicate section=" << section.name << " detected";
      return nullptr;
    }

    if (section.chainedafter.size()) {
      if (actions.find(section.chainedafter) == actions.end()) {
        OLOG << "Could not find section=" << section.chainedafter
             << " to chain section=" << section.name << " after";
        return nullptr;
      }

      Oomd::Config2::IR::Action action;
      action.name = section.plugin;
      action.args = section.args;
      actions[section.chainedafter].acts.emplace_back(std::move(action));
    } else {
      Oomd::Config2::IR::Ruleset ruleset;
      ruleset.name = section.name;

      Oomd::Config2::IR::DetectorGroup dg;
      dg.name = "default";
      for (const auto& cond : section.conditions) {
        dg.detectors.emplace_back(detectors[cond]); // copy
      }
      ruleset.dgs.emplace_back(dg);

      Oomd::Config2::IR::Action action;
      action.name = section.plugin;
      action.args = section.args;
      ruleset.acts.emplace_back(std::move(action));

      actions[section.name] = std::move(ruleset);
    }
  }

  // Populate root node
  for (auto& pair : actions) {
    ir_root->rulesets.emplace_back(std::move(pair.second));
  }

  return ir_root;
}

} // namespace

namespace Oomd {
namespace Config2 {

std::unique_ptr<IR::Root> CompactConfigParser::parse(const std::string& input) {
  auto parsed_sections = parseConfig(input);
  if (!parsed_sections.has_value()) {
    return nullptr;
  }

  for (const auto& s : *parsed_sections) {
    if (!isSectionValid(s)) {
      return nullptr;
    }
  }

  auto ir_root = constructIR(*parsed_sections);
  if (!ir_root) {
    return nullptr;
  }

  IR::dumpIR(*ir_root);
  return ir_root;
}

} // namespace Config2
} // namespace Oomd
