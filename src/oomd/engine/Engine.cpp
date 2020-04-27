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

#include "oomd/engine/Engine.h"

#include <algorithm>

#include "oomd/Log.h"
#include "oomd/Stats.h"
#include "oomd/include/CoreStats.h"

namespace Oomd {
namespace Engine {

Engine::Engine(
    MonitoredResources resources,
    std::vector<std::unique_ptr<Ruleset>> rulesets)
    : resources_(std::move(resources)) {
  for (auto& rs : rulesets) {
    if (rs) {
      rulesets_.emplace_back(BaseRuleset{.ruleset = std::move(rs)});
    }
  }
}

bool Engine::addDropInConfig(size_t tag, std::unique_ptr<Ruleset> ruleset) {
  if (!ruleset) {
    return false;
  }

  // First located the targeted ruleset
  auto it = std::find_if(
      rulesets_.begin(), rulesets_.end(), [&](const BaseRuleset& b) {
        return b.ruleset->getName() == ruleset->getName();
      });
  if (it == rulesets_.end()) {
    OLOG << "Error: could not locate targeted ruleset: " << ruleset->getName();
    return false;
  }

  // Add drop in ruleset
  DropInRuleset dir;
  dir.tag = tag;
  dir.ruleset = std::move(ruleset);
  // NB: the drop in rulesets must be added/executed LIFO order.
  it->dropins.emplace_front(std::move(dir));

  // Mark base ruleset at targeted
  it->ruleset->markDropInTargeted();

  Oomd::incrementStat(CoreStats::kNumDropInAdds, 1);

  return true;
}

void Engine::removeDropInConfig(size_t tag) {
  auto pred = [&](const DropInRuleset& dir) { return dir.tag == tag; };

  for (auto& base : rulesets_) {
    auto new_end =
        std::remove_if(base.dropins.begin(), base.dropins.end(), pred);

    int n = base.dropins.cend() - new_end;
    if (!n) {
      continue;
    }

    // Delete properly tagged drop in rulesets as requested
    base.dropins.erase(new_end, base.dropins.end());

    // Mark base ruleset as untargeted
    for (int i = 0; i < n; ++i) {
      base.ruleset->markDropInUntargeted();
    }

    // Make sure to decrement counter if there's a remove. This is to
    // normalize the count in case the same drop-in config is added/
    // removed a bunch for some reason.
    Oomd::incrementStat(CoreStats::kNumDropInAdds, -n);
  }
}

void Engine::prerun(OomdContext& context) {
  for (const auto& base : rulesets_) {
    for (const auto& dropin : base.dropins) {
      if (dropin.ruleset) {
        dropin.ruleset->prerun(context);
      }
    }

    base.ruleset->prerun(context);
  }
}

void Engine::runOnce(OomdContext& context) {
  uint32_t nr_dropins_run = 0;

  for (const auto& base : rulesets_) {
    // Run all the drop in rulesets first
    for (const auto& dropin : base.dropins) {
      if (dropin.ruleset) {
        nr_dropins_run += dropin.ruleset->runOnce(context);
      }
    }

    // Now run the base ruleset
    base.ruleset->runOnce(context);
  }

  Oomd::incrementStat(CoreStats::kNumDropInFired, nr_dropins_run);
}

const MonitoredResources& Engine::getMonitoredResources() const {
  return resources_;
}

} // namespace Engine
} // namespace Oomd
