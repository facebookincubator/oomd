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
#include <unordered_map>
#include <vector>

namespace Oomd {
namespace Config2 {
namespace IR {

// The IR (intermediate representation) namespace holds the fundamental
// datastructures responsible for oomd runtime behavior. The IR was created to
// decouple the config language (eg JSON) from the generic behavior of oomd. To
// add another config frontend, for example an iptables-like syntax, one only
// needs to create a parsing function that can construct an struct IR::Root.
// The IR::Root can then be passed to the ConfigCompiler.
//
// In the most general sense, oomd is configured to wait for DetectorGroups to
// fire. If a DetectorGroup fires, then corresponding Actions are taken. A
// DetectorGroup is made up of one or more Detectors.
//
// Both Detector and Action derive from the base Plugin class. A Plugin can
// return CONTINUE or STOP. If all Detectors in a DetectorGroup return
// CONTINUE, that is to say it does not terminate the Detector chain with a
// STOP, then the corresponding Action chain is run. An Action chain runs until
// a plugin in the chain returns STOP.

struct Plugin {
  std::string name;
  std::unordered_map<std::string, std::string> args;
};

struct Detector : Plugin {};

struct Action : Plugin {};

struct DetectorGroup {
  std::string name;
  std::vector<Detector> detectors;
};

struct Ruleset {
  std::string name;
  std::vector<DetectorGroup> dgs;
  std::vector<Action> acts;
};

struct Root {
  std::vector<Ruleset> rulesets;
};

void dumpIR(const Root& root);

} // namespace IR
} // namespace Config2
} // namespace Oomd
