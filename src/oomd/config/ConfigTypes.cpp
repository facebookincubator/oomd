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

#include <string.h>
#include "oomd/Log.h"
#include "oomd/config/ConfigTypes.h"

namespace {
/*
 * Make sure all log lines get consistent indentation, even if OLOG prefixes
 * vary because some lines numbers are 3 digits and some are 2.
 */
Oomd::LogStream::Offset getIndentSpaces(uint64_t depth) {
  return Oomd::LogStream::Offset{.n = ::strlen(FILENAME) + 7 + depth * 2};
}
} // namespace

namespace Oomd {
namespace Config2 {
namespace IR {

void dumpIR(const Root& root) {
  int indent = 0;

  // Prekill hooks
  OLOG << getIndentSpaces(indent) << root.prekill_hooks.size()
       << " PrekillHooks=";
  ++indent;

  for (const auto& hook : root.prekill_hooks) {
    OLOG << getIndentSpaces(indent) << "Hook=" << hook.name;
    ++indent;

    // Print arguments
    OLOG << getIndentSpaces(indent) << "Args=";
    ++indent;
    for (const auto& pair : hook.args) {
      OLOG << getIndentSpaces(indent) << pair.first << "=" << pair.second;
    }
    indent -= 2;
  }
  --indent;

  // Rulesets
  OLOG << getIndentSpaces(indent) << root.rulesets.size() << " Rulesets=";
  ++indent;

  for (const auto& ruleset : root.rulesets) {
    OLOG << getIndentSpaces(indent) << "Ruleset=" << ruleset.name;
    ++indent;

    // Print DropIn config
    OLOG << getIndentSpaces(indent) << "DropIn=";
    ++indent;
    OLOG << getIndentSpaces(indent)
         << "Detectors=" << ruleset.dropin.detectorgroups_enabled;
    OLOG << getIndentSpaces(indent)
         << "Actions=" << ruleset.dropin.actiongroup_enabled;
    OLOG << getIndentSpaces(indent)
         << "DisableOnDrop=" << ruleset.dropin.disable_on_drop_in;
    --indent;

    OLOG << getIndentSpaces(indent) << "SilenceLogs=" << ruleset.silence_logs;

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
}

} // namespace IR
} // namespace Config2
} // namespace Oomd
