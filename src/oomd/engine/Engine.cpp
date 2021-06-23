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
#include <optional>

#include "oomd/Log.h"
#include "oomd/Stats.h"
#include "oomd/include/CoreStats.h"

namespace Oomd {
namespace Engine {

Engine::Engine(
    std::vector<std::unique_ptr<Ruleset>> rulesets,
    std::vector<std::unique_ptr<PrekillHook>> prekill_hooks) {
  for (auto& rs : rulesets) {
    if (rs) {
      rulesets_.emplace_back(BaseRuleset{.ruleset = std::move(rs)});
    }
  }

  // add base config hooks in reverse order so they'll be tried in forward order
  for (auto it = prekill_hooks.rbegin(); it != prekill_hooks.rend(); ++it) {
    prekill_hooks_in_reverse_order_.emplace_back(
        TaggedPrekillHook{.dropin_tag = std::nullopt, .hook = std::move(*it)});
  }
}

bool Engine::addDropInConfig(const std::string& tag, DropInUnit unit) {
  for (auto& drop_in : unit.rulesets) {
    if (!addDropInRuleset(tag, std::move(drop_in))) {
      // Clean up partial added drop ins
      removeDropInConfig(tag);
      return false;
    }
  }

  // add dropin hooks in reverse order to the end of
  // prekill_hooks_in_reverse_order_ so they'll be tried in forward order,
  // before the base hooks.
  for (auto it = unit.prekill_hooks.rbegin(); it != unit.prekill_hooks.rend();
       ++it) {
    prekill_hooks_in_reverse_order_.emplace_back(
        TaggedPrekillHook{.dropin_tag = tag, .hook = std::move(*it)});
  }

  return true;
}

bool Engine::addDropInRuleset(
    const std::string& tag,
    std::unique_ptr<Ruleset> ruleset) {
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

void Engine::removeDropInConfig(const std::string& tag) {
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

  auto new_hooks_end = std::remove_if(
      prekill_hooks_in_reverse_order_.begin(),
      prekill_hooks_in_reverse_order_.end(),
      [&](const TaggedPrekillHook& tagged_hook) {
        return tagged_hook.dropin_tag == tag;
      });
  prekill_hooks_in_reverse_order_.erase(
      new_hooks_end, prekill_hooks_in_reverse_order_.end());
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

std::optional<std::unique_ptr<PrekillHookInvocation>> Engine::firePrekillHook(
    const CgroupContext& cgroup_ctx,
    const OomdContext& oomd_context) {
  // try hooks in reverse order so dropins come first
  for (auto it = prekill_hooks_in_reverse_order_.rbegin();
       it != prekill_hooks_in_reverse_order_.rend();
       ++it) {
    if (it->hook->canRunOnCgroup(cgroup_ctx)) {
      return it->hook->fire(cgroup_ctx, oomd_context.getActionContext());
    }
  }

  return std::nullopt;
}

} // namespace Engine
} // namespace Oomd
