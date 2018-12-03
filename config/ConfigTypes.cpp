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

#include <cassert>

#include "oomd/Log.h"
#include "oomd/config/ConfigTypes.h"

namespace {
std::string getIndentSpaces(int depth) {
  return std::string(depth * 2, ' ');
}
} // namespace

namespace Oomd {
namespace Config2 {
namespace IR {

void dumpIR(const Root& root) {
  int indent = 0;

  OLOG << getIndentSpaces(indent) << root.rulesets.size() << " Rulesets=";
  ++indent;

  for (const auto& ruleset : root.rulesets) {
    OLOG << getIndentSpaces(indent) << "Ruleset=" << ruleset.name;
    ++indent;

    // Print DetectorGroup's
    for (const auto& dg : ruleset.dgs) {
      OLOG << getIndentSpaces(indent) << "DetectorGroup=" << dg.name;
      ++indent;

      // Print Detectors
      for (const auto& d : dg.detectors) {
        OLOG << getIndentSpaces(indent) << "Detector=" << d.name;
        ++indent;
        OLOG << getIndentSpaces(indent) << "Args=";
        ++indent;

        // Print arguments
        for (const auto& pair : d.args) {
          OLOG << getIndentSpaces(indent) << pair.first << "=" << pair.second;
        }
        indent -= 2;
      }
      --indent;
    }

    // Print Action's
    for (const auto& act : ruleset.acts) {
      OLOG << getIndentSpaces(indent) << "Action=" << act.name;
      ++indent;
      OLOG << getIndentSpaces(indent) << "Args=";
      ++indent;

      // Print arguments
      for (const auto& pair : act.args) {
        OLOG << getIndentSpaces(indent) << pair.first << "=" << pair.second;
      }
      indent -= 2;
    }

    --indent;
  }
  --indent;

  OLOG << getIndentSpaces(indent) << "Version=" << root.version;
}

} // namespace IR
} // namespace Config2
} // namespace Oomd
