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
#include <cmath>
#include <iomanip>
#include <string>
#include <utility>
#include <vector>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/include/Types.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace Oomd {

template <typename Base>
int KillMemoryGrowth<Base>::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  if (args.find("cgroup") != args.end()) {
    const auto& cgroup_fs = context.cgroupFs();

    auto cgroups = Util::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
      cgroups_.emplace(cgroup_fs, c);
    }
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  if (args.find("size_threshold") != args.end()) {
    int val = std::stoi(args.at("size_threshold"));

    if (val < 0) {
      OLOG << "Argument=size_threshold must be non-negative";
      return 1;
    }

    size_threshold_ = val;
  }

  if (args.find("growing_size_percentile") != args.end()) {
    int val = std::stoi(args.at("growing_size_percentile"));

    if (val < 0) {
      OLOG << "Argument=growing_size_percentile must be non-negative";
      return 1;
    }

    growing_size_percentile_ = val;
  }

  if (args.find("min_growth_ratio") != args.end()) {
    int val = std::stof(args.at("min_growth_ratio"));

    if (val < 0) {
      OLOG << "Argument=min_growth_ratio must be non-negative";
      return 1;
    }

    min_growth_ratio_ = val;
  }

  if (args.find("post_action_delay") != args.end()) {
    int val = std::stoi(args.at("post_action_delay"));

    if (val < 0) {
      OLOG << "Argument=post_action_delay must be non-negative";
      return 1;
    }

    post_action_delay_ = val;
  }

  if (args.find("dry") != args.end()) {
    const std::string& val = args.at("dry");

    if (val == "true" || val == "True" || val == "1") {
      dry_ = true;
    }
  }

  if (args.find("debug") != args.end()) {
    const std::string& val = args.at("debug");

    if (val == "true" || val == "True" || val == "1") {
      debug_ = true;
    }
  }

  // Success
  return 0;
}

template <typename Base>
void KillMemoryGrowth<Base>::prerun(OomdContext& ctx) {
  for (const CgroupContext& cgroup_ctx : ctx.addToCacheAndGet(cgroups_)) {
    // Make sure temporal counters be available when run() is invoked
    cgroup_ctx.average_usage();
  }
}

template <typename Base>
Engine::PluginRet KillMemoryGrowth<Base>::run(OomdContext& ctx) {
  // First try to kill by size, respecting size_threshold. Failing that,
  // try to kill by growth. Failing that, try to kill by size, ignoring
  // size_threshold.
  bool ret = tryToKillSomething(ctx);

  if (ret) {
    std::this_thread::sleep_for(std::chrono::seconds(post_action_delay_));
    return Engine::PluginRet::STOP;
  } else {
    return Engine::PluginRet::CONTINUE;
  }
}

template <typename Base>
bool KillMemoryGrowth<Base>::tryToKillSomething(OomdContext& ctx) {
  const auto usage_sorted =
      ctx.reverseSort(cgroups_, [](const CgroupContext& cgroup_ctx) {
        return cgroup_ctx.effective_usage().value_or(0);
      });

  if (usage_sorted.size() < 1) {
    return false;
  }

  // Only the top P(growing_size_percentile_) cgroups by usage are eligible for
  // killing by growth.
  const size_t nr = std::ceil(
      usage_sorted.size() *
      (100 - static_cast<double>(growing_size_percentile_)) / 100);
  const int64_t growth_kill_min_effective_usage_threshold =
      usage_sorted[nr - 1].get().effective_usage().value_or(0);

  // Compute phase 1's threshold in bytes from size_threshold_, which is given
  // as a pct of total
  int64_t cur_memcurrent = 0;
  for (const CgroupContext& cgroup_ctx : usage_sorted) {
    cur_memcurrent += cgroup_ctx.current_usage().value_or(0);
  }
  int64_t size_threshold_in_bytes =
      cur_memcurrent * (static_cast<double>(size_threshold_) / 100);

  auto get_growth = [](const CgroupContext& cgroup_ctx) {
    return static_cast<double>(cgroup_ctx.current_usage().value_or(0)) /
        cgroup_ctx.average_usage().value_or(1);
  };

  auto rank_cgroup = [=](const CgroupContext& cgroup_ctx) {
    int64_t current_usage = cgroup_ctx.current_usage().value_or(0);
    int64_t effective_usage = cgroup_ctx.effective_usage().value_or(0);
    float growth_ratio = get_growth(cgroup_ctx);

    bool size_phase_eligible = current_usage >= size_threshold_in_bytes;
    auto growth_phase_eligible = growth_ratio >= min_growth_ratio_ &&
        effective_usage >= growth_kill_min_effective_usage_threshold;

    // KillMemoryGrowth has 3 phases: cgroups above a usage threshold are
    // targeted first, then cgroups above a growth threshold, and finally the
    // rest. Phase is lower priority than kill_preference; a PREFER cgroup
    // passing no thresholds will be targeted before an AVOID cgroup above the
    // mem threshold. Tuples sort lexicographically, and cgroups ineligible for
    // a phase get a 0 for that phase, so all cgroups in size_phase come before
    // all cgroups only in growth phase, which come before those in neither
    // (kill_preference aside).
    return std::make_tuple(
        cgroup_ctx.kill_preference().value_or(KillPreference::NORMAL),
        size_phase_eligible ? effective_usage : 0,
        growth_phase_eligible ? growth_ratio : 0,
        effective_usage);
  };

  const auto ranked = ctx.reverseSort(cgroups_, rank_cgroup);
  OomdContext::dump(ranked, !debug_);

  // try to kill from highest ranked to lowest, until one works
  for (const CgroupContext& cgroup_ctx : ranked) {
    // Skip trying to kill an empty cgroup, which would unfairly increment the
    // empty cgroup's kill counters and pollute the logs. We get into a
    // situation where we try to kill empty cgroups when a cgroup marked PREFER
    // is not the source of pressure: KillMemoryGrowth will kill the PREFER
    // cgroup first, but that won't fix the problem so it will kill again; on
    // the second time around, it first targets the now-empty PREFER cgroup
    // before moving on to a better victim.
    if (!cgroup_ctx.is_populated().value_or(true)) {
      continue;
    }

    int64_t size_threshold_phase_value, growth_threshold_phase_value;
    std::tie(
        std::ignore,
        size_threshold_phase_value,
        growth_threshold_phase_value,
        std::ignore) = rank_cgroup(cgroup_ctx);
    bool kill_size_threshold_phase = (size_threshold_phase_value != 0);
    bool kill_growth_threshold_phase =
        !kill_size_threshold_phase && growth_threshold_phase_value != 0;

    if (kill_growth_threshold_phase) {
      std::ostringstream oss;
      oss << std::setprecision(2) << std::fixed;
      oss << "Picked \"" << cgroup_ctx.cgroup().relativePath() << "\" ("
          << cgroup_ctx.current_usage().value_or(0) / 1024 / 1024
          << "MB) based on growth rate " << get_growth(cgroup_ctx) << " among P"
          << growing_size_percentile_ << " largest,"
          << " with kill preference "
          << cgroup_ctx.kill_preference().value_or(KillPreference::NORMAL);

    } else {
      OLOG << "Picked \"" << cgroup_ctx.cgroup().relativePath() << "\" ("
           << cgroup_ctx.current_usage().value_or(0) / 1024 / 1024
           << "MB) based on size > " << size_threshold_ << "% of total "
           << cur_memcurrent / 1024 / 1024 << "MB"
           << (!kill_size_threshold_phase ? " (size threshold overridden)" : "")
           << " with kill preference "
           << cgroup_ctx.kill_preference().value_or(KillPreference::NORMAL);
    }

    if (auto kill_uuid = Base::tryToKillCgroup(
            cgroup_ctx.cgroup().absolutePath(), true, dry_)) {
      Base::logKill(
          cgroup_ctx.cgroup(),
          cgroup_ctx,
          ctx.getActionContext(),
          *kill_uuid,
          dry_);
      return true;
    }
  }

  return false;
}

} // namespace Oomd
