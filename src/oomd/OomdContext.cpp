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

#include "oomd/OomdContext.h"
#include <optional>

#include "oomd/Log.h"
#include "oomd/engine/Engine.h"
#include "oomd/util/Fs.h"

namespace Oomd {

std::vector<CgroupPath> OomdContext::cgroups() const {
  std::vector<CgroupPath> keys;

  for (const auto& pair : cgroups_) {
    keys.emplace_back(pair.first);
  }

  return keys;
}

std::optional<OomdContext::ConstCgroupContextRef> OomdContext::addToCacheAndGet(
    const CgroupPath& cgroup) {
  // Return cached cgroup if already exists
  if (auto pos = cgroups_.find(cgroup); pos != cgroups_.end()) {
    return pos->second;
  }
  if (auto ctx = CgroupContext::make(*this, cgroup)) {
    return cgroups_.emplace(cgroup, std::move(*ctx)).first->second;
  }
  return std::nullopt;
}

std::vector<OomdContext::ConstCgroupContextRef> OomdContext::addToCacheAndGet(
    const std::unordered_set<CgroupPath>& cgroups) {
  std::unordered_set<CgroupPath> all_resolved;
  std::vector<ConstCgroupContextRef> ret;
  for (const auto& cgroup : cgroups) {
    auto resolved = cgroup.resolveWildcard();
    all_resolved.insert(resolved.begin(), resolved.end());
  }
  for (const auto& resolved : all_resolved) {
    if (auto cgroup_ctx = addToCacheAndGet(resolved)) {
      ret.push_back(*cgroup_ctx);
    }
  }
  return ret;
}

std::optional<OomdContext::ConstCgroupContextRef>
OomdContext::addChildToCacheAndGet(
    const CgroupContext& cgroup_ctx,
    const std::string& child) {
  if (auto child_ctx = cgroup_ctx.createChildCgroupCtx(child)) {
    const CgroupPath& child_path = child_ctx->cgroup();
    const auto& pair = cgroups_.emplace(child_path, std::move(*child_ctx));
    return std::make_optional<OomdContext::ConstCgroupContextRef>(
        pair.first->second);
  } else {
    OLOG << "failed to get child of " << cgroup_ctx.cgroup().relativePath()
         << " named " << child;
    return std::nullopt;
  }
}

std::vector<OomdContext::ConstCgroupContextRef>
OomdContext::addChildrenToCacheAndGet(const CgroupContext& cgroup_ctx) {
  std::vector<OomdContext::ConstCgroupContextRef> ret;

  CgroupContext::Error err;
  std::unordered_set<Oomd::CgroupPath> child_paths;
  if (auto children = cgroup_ctx.children(&err)) {
    for (const auto& name : *children) {
      if (const auto& child_ctx = addChildToCacheAndGet(cgroup_ctx, name)) {
        ret.push_back(*child_ctx);
      } else {
        OLOG << "failed to get child of " << cgroup_ctx.cgroup().relativePath()
             << " named " << name;
      }
    }
  } else {
    OLOG << "failed to get children of cgroup "
         << cgroup_ctx.cgroup().relativePath();
  }

  return ret;
}

const ActionContext& OomdContext::getActionContext() const {
  return action_context_;
}

void OomdContext::setActionContext(const ActionContext& context) {
  action_context_ = context;
}

const SystemContext& OomdContext::getSystemContext() const {
  return system_ctx_;
}

void OomdContext::setSystemContext(const SystemContext& context) {
  system_ctx_ = context;
}

const std::optional<Engine::Ruleset*> OomdContext::getInvokingRuleset() {
  return invoking_ruleset_;
}

void OomdContext::setInvokingRuleset(std::optional<Engine::Ruleset*> ruleset) {
  invoking_ruleset_ = ruleset;
}

void OomdContext::dump() {
  std::vector<ConstCgroupContextRef> cgroups;
  for (auto& pair : cgroups_) {
    cgroups.push_back(pair.second);
  }
  dump(cgroups);
}

void OomdContext::dump(
    const std::vector<ConstCgroupContextRef>& cgroup_ctxs,
    const bool skip_negligible) {
  auto cgmax = std::numeric_limits<int64_t>::max();
  OLOG << "Dumping OomdContext: ";
  for (const CgroupContext& cgroup_ctx : cgroup_ctxs) {
    auto mem_pressure = cgroup_ctx.mem_pressure().value_or(ResourcePressure{});
    auto io_pressure = cgroup_ctx.io_pressure().value_or(ResourcePressure{});
    auto current_usage = cgroup_ctx.current_usage().value_or(0);
    auto average_usage = cgroup_ctx.average_usage().value_or(0);
    auto memory_low = cgroup_ctx.memory_low().value_or(0);
    auto memory_min = cgroup_ctx.memory_min().value_or(0);
    auto memory_high = cgroup_ctx.memory_high().value_or(cgmax);
    auto memory_high_tmp = cgroup_ctx.memory_high_tmp().value_or(cgmax);
    auto memory_max = cgroup_ctx.memory_max().value_or(cgmax);
    auto memory_protection = cgroup_ctx.memory_protection().value_or(0);
    auto anon_usage = cgroup_ctx.anon_usage().value_or(0);
    auto swap_usage = cgroup_ctx.swap_usage().value_or(0);
    auto io_cost_cumulative = cgroup_ctx.io_cost_cumulative().value_or(0);
    auto io_cost_rate = cgroup_ctx.io_cost_rate().value_or(0);
    auto pg_scan_cumulative = cgroup_ctx.pg_scan_cumulative().value_or(0);
    auto pg_scan_rate = cgroup_ctx.pg_scan_rate().value_or(0);
    auto kill_preference =
        cgroup_ctx.kill_preference().value_or(KillPreference::NORMAL);

    if (skip_negligible) {
      // don't show if <1% pressure && <.1% usage
      auto meminfo = Fs::getMeminfo();
      // TODO(dschatzberg) report error
      if (meminfo) {
        const float press_min = 1;
        const int64_t mem_min = (*meminfo)["MemTotal"] / 1000;
        const int64_t swap_min = (*meminfo)["SwapTotal"] / 1000;

        if (!(mem_pressure.sec_10 >= press_min ||
              mem_pressure.sec_60 >= press_min ||
              mem_pressure.sec_300 >= press_min ||
              io_pressure.sec_10 >= press_min ||
              io_pressure.sec_60 >= press_min ||
              io_pressure.sec_300 >= press_min || current_usage > mem_min ||
              average_usage > mem_min || swap_usage > swap_min)) {
          continue;
        }
      }
    }

    OLOG << "name=" << cgroup_ctx.cgroup().relativePath();
    OLOG << "  pressure=" << mem_pressure.sec_10 << ":" << mem_pressure.sec_60
         << ":" << mem_pressure.sec_300 << "-" << io_pressure.sec_10 << ":"
         << io_pressure.sec_60 << ":" << io_pressure.sec_300;
    OLOG << "  mem=" << (current_usage >> 20) << "MB"
         << " mem_avg=" << (average_usage >> 20) << "MB"
         << " mem_low=" << (memory_low >> 20) << "MB"
         << " mem_min=" << (memory_min >> 20) << "MB"
         << " mem_high=" << (memory_high >> 20) << "MB"
         << " mem_high_tmp=" << (memory_high_tmp >> 20) << "MB"
         << " mem_max=" << (memory_max >> 20) << "MB"
         << " mem_prot=" << (memory_protection >> 20) << "MB"
         << " anon=" << (anon_usage >> 20) << "MB"
         << " swap_usage=" << (swap_usage >> 20) << "MB";
    OLOG << "  io_cost_cumulative=" << io_cost_cumulative
         << " io_cost_rate=" << io_cost_rate;
    OLOG << "  pg_scan_cumulative=" << pg_scan_cumulative
         << " pg_scan_rate=" << pg_scan_rate;
    OLOG << "  kill_preference=" << kill_preference;
  }
}

void OomdContext::refresh() {
  auto it = cgroups_.begin();
  while (it != cgroups_.end()) {
    it = it->second.refresh() ? std::next(it) : cgroups_.erase(it);
  }
}

void OomdContext::setPrekillHooks(
    const std::vector<std::unique_ptr<Engine::PrekillHook>>& prekill_hooks) {
  prekill_hooks_ = prekill_hooks;
}

std::optional<std::unique_ptr<Engine::PrekillHookInvocation>>
OomdContext::firePrekillHook(const CgroupContext& cgroup_ctx) {
  if (prekill_hooks_) {
    for (auto& hook : prekill_hooks_.value().get()) {
      if (hook->canRunOnCgroup(cgroup_ctx)) {
        return hook->fire(cgroup_ctx);
      }
    }
  }

  return std::nullopt;
}

} // namespace Oomd
