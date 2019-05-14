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

#include <optional>

#include "oomd/config/ConfigTypes.h"
#include "oomd/engine/Engine.h"

namespace Oomd {
namespace Config2 {

struct DropInUnit {
  Engine::MonitoredResources resources;
  std::vector<std::unique_ptr<Engine::Ruleset>> rulesets;
};

/*
 * Compiles IR into Oomd::Engine data structures. Also performs full
 * validation on the IR. If the IR is invalid, @method compile will
 * print error messages and return a nullptr.
 */
std::unique_ptr<Engine::Engine> compile(const IR::Root& root);

/*
 * Compiles a drop in ruleset against a @class IR::Root config. Has
 * the same semantics as @method compile. The compiled ruleset and
 * @class Engine::MonitoredResources must be injected into an
 * existing @class Engine::Engine.
 */
std::optional<DropInUnit> compileDropIn(
    const IR::Root& root,
    const IR::Root& dropin);

} // namespace Config2
} // namespace Oomd
