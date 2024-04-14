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

#include <algorithm>
#include <iomanip>
#include <string>
#include <utility>
#include <vector>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/include/Types.h"
#include "oomd/util/Util.h"

namespace Oomd {

template <typename Base>
int KillMemoryGrowth<Base>::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  this->argParser_.addArgumentCustom(
      "size_threshold", size_threshold_, PluginArgParser::parseUnsignedInt);

  this->argParser_.addArgumentCustom(
      "growing_size_percentile",
      growing_size_percentile_,
      [&](const std::string& s) {
        int v = std::stoi(s);
        if (v < 0 || v >= 100) {
          throw std::invalid_argument(
              "growing_size_percentile must be in range [0, 100)");
        }
        return v;
      });

  this->argParser_.addArgumentCustom(
      "min_growth_ratio", min_growth_ratio_, PluginArgParser::parseUnsignedInt);

  return Base::init(args, context);
}

template <typename Base>
void KillMemoryGrowth<Base>::prerun(OomdContext& ctx) {
  // Make sure temporal counters be available when run() is invoked
  Base::prerunOnCgroups(
      ctx, [](const auto& cgroup_ctx) { cgroup_ctx.average_usage(); });
}

template <typename Base>
std::function<std::pair<KMGPhase, std::tuple<int64_t, float, int64_t>>(
    const CgroupContext&)>
KillMemoryGrowth<Base>::get_ranking_fn(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {
  // First, compute respective thresholds for inclusion in the first 2
  // phases, size_threshold_in_bytes and
  // growth_kill_min_effective_usage_threshold.

  int64_t cur_memcurrent = 0;
  for (const CgroupContext& cgroup_ctx : cgroups) {
    cur_memcurrent += cgroup_ctx.current_usage().value_or(0);
  }
  int64_t size_threshold_in_bytes =
      cur_memcurrent * (static_cast<double>(size_threshold_) / 100);

  // Only the top P(growing_size_percentile_) cgroups by usage are eligible
  // for killing by growth. nth is the index of the idx of the cgroup w/
  // smallest usage in the top P(growing_size_percentile_)
  int64_t growth_kill_min_effective_usage_threshold = 0;
  if (cgroups.size() > 0 && growing_size_percentile_ > 0) {
    const size_t nth =
        std::ceil(
            cgroups.size() *
            (100 - static_cast<double>(growing_size_percentile_)) / 100) -
        1;
    auto cgroups_mutable_copy = cgroups;
    std::nth_element(
        cgroups_mutable_copy.begin(),
        cgroups_mutable_copy.begin() + nth,
        cgroups_mutable_copy.end(),
        [](const auto& a, const auto& b) {
          // order by effective_usage desc
          return a.get().effective_usage().value_or(0) >
              b.get().effective_usage().value_or(0);
        });
    growth_kill_min_effective_usage_threshold =
        cgroups_mutable_copy[nth].get().effective_usage().value_or(0);
  }

  return [=, this](const CgroupContext& cgroup_ctx) {
    int64_t current_usage = cgroup_ctx.current_usage().value_or(0);
    int64_t effective_usage = cgroup_ctx.effective_usage().value_or(0);
    float growth_ratio = cgroup_ctx.memory_growth().value_or(0);

    bool size_phase_eligible = current_usage >= size_threshold_in_bytes;
    auto growth_phase_eligible = growth_ratio >= min_growth_ratio_ &&
        effective_usage >= growth_kill_min_effective_usage_threshold;

    // KillMemoryGrowth has 3 phases: cgroups above a usage threshold are
    // targeted first, then cgroups above a growth threshold, and finally
    // the rest.
    KMGPhase phase = size_phase_eligible ? KMGPhase::SIZE_THRESHOLD
        : growth_phase_eligible          ? KMGPhase::GROWTH
                                         : KMGPhase::SIZE_NO_THRESHOLD;

    // All cgroups in SIZE_THRESHOLD phase rank first, then groups in
    // KMGPhase::GROWTH, then KMGPhase::SIZE_NO_THRESHOLD because tuples
    // sort lexicographically and cgroups ineligible for a phase get a 0 for
    // that phase.
    auto rank = std::make_tuple(
        size_phase_eligible ? effective_usage : 0,
        growth_phase_eligible ? growth_ratio : 0,
        effective_usage);

    return std::make_pair(phase, rank);
  };
}

template <typename Base>
std::vector<OomdContext::ConstCgroupContextRef>
KillMemoryGrowth<Base>::rankForKilling(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {
  auto rank_cgroup = get_ranking_fn(ctx, cgroups);

  // Note kill_preference take priority over phase, which is
  // handled automatically by sortDescWithKillPrefs.
  return OomdContext::sortDescWithKillPrefs(
      cgroups, [&](const CgroupContext& cgroup_ctx) {
        return rank_cgroup(cgroup_ctx).second;
      });
}

template <typename Base>
void KillMemoryGrowth<Base>::ologKillTarget(
    OomdContext& ctx,
    const CgroupContext& target,
    const std::vector<OomdContext::ConstCgroupContextRef>& peers) {
  auto rank_cgroup = get_ranking_fn(ctx, peers);

  int64_t sib_memcurrent = 0;
  for (const CgroupContext& cgroup_ctx : peers) {
    sib_memcurrent += cgroup_ctx.current_usage().value_or(0);
  }

  switch (rank_cgroup(target).first) {
    case KMGPhase::SIZE_THRESHOLD: {
      OLOG << "Picked \"" << target.cgroup().relativePath() << "\" ("
           << target.current_usage().value_or(0) / 1024 / 1024
           << "MB) based on size > " << size_threshold_ << "% of sibling total "
           << sib_memcurrent / 1024 / 1024 << "MB" << " with kill preference "
           << target.kill_preference().value_or(KillPreference::NORMAL);
      break;
    }

    case KMGPhase::GROWTH: {
      std::ostringstream oss;
      oss << std::setprecision(2) << std::fixed;
      oss << "Picked \"" << target.cgroup().relativePath() << "\" ("
          << target.current_usage().value_or(0) / 1024 / 1024
          << "MB) based on growth rate " << target.memory_growth().value_or(0)
          << " (min growth rate " << min_growth_ratio_ << ")" << " among P"
          << growing_size_percentile_ << " largest," << " with kill preference "
          << target.kill_preference().value_or(KillPreference::NORMAL);
      OLOG << oss.str();
      break;
    }

    case KMGPhase::SIZE_NO_THRESHOLD: {
      OLOG << "Picked \"" << target.cgroup().relativePath() << "\" ("
           << target.current_usage().value_or(0) / 1024 / 1024
           << "MB) based on effective usage of "
           << target.effective_usage().value_or(0) << "MB"
           << " with kill preference "
           << target.kill_preference().value_or(KillPreference::NORMAL);
      break;
    }
  }
}

} // namespace Oomd
