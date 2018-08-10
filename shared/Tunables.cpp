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

#include "oomd/shared/Tunables.h"

#include <cstdlib>

namespace Oomd {

Tunables::Tunables() {
  for (auto& k : knobs) {
    k.value = k.default_value;
  }
}

void Tunables::dump() {
  for (auto& k : knobs) {
    OLOG << k.env_name << "=" << k.value;
  }
}

void Tunables::parseEnvVars() {
  for (auto& knob : knobs) {
    const char* env_val = std::getenv(knob.env_name.c_str());
    if (env_val) {
      knob.value = env_val;
    } else {
      knob.value = knob.default_value;
    }
  }
}

void Tunables::loadOverrides(const std::string& fname) {
  // If we have been told to reload and the override file is
  // either empty or missing, we go back to initial values
  auto lines = Fs::readFileByLine(fname);
  if (lines.size() == 0) {
    parseEnvVars();
    return;
  }

  for (const auto& line : lines) {
    auto toks = Fs::split(line, '=');
    if (toks.size() != 2) {
      OLOG << "Tunables override line is malformed: " << line;
      continue;
    }

    for (auto& k : knobs) {
      if (k.env_name == toks[0]) {
        k.value = toks[1];
      }
    }
  }
}

} // namespace Oomd
