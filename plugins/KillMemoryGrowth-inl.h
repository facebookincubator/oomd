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

namespace {
auto constexpr kCgroupFs = "/sys/fs/cgroup";
} // namespace

namespace Oomd {

template <typename Base>
int KillMemoryGrowth<Base>::init(
    Engine::MonitoredResources& resources,
    const Engine::PluginArgs& args) {
  if (args.find("cgroup") != args.end()) {
    auto cgroup_fs =
        (args.find("cgroup_fs") != args.end() ? args.at("cgroup_fs")
                                              : kCgroupFs);

    auto cgroups = Fs::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
      resources.emplace(cgroup_fs, c);
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
Engine::PluginRet KillMemoryGrowth<Base>::run(OomdContext& ctx) {
  // First try to kill by size, respecting size_threshold. Failing that,
  // try to kill by growth. Failing that, try to kill by size, ignoring
  // size_threshold.
  bool ret = tryToKillBySize(ctx, false);
  if (!ret) {
    ret = tryToKillByGrowth(ctx);
  }
  if (!ret) {
    ret = tryToKillBySize(ctx, true);
  }

  if (ret) {
    std::this_thread::sleep_for(std::chrono::seconds(post_action_delay_));
    return Engine::PluginRet::STOP;
  } else {
    return Engine::PluginRet::CONTINUE;
  }
}

template <typename Base>
bool KillMemoryGrowth<Base>::tryToKillBySize(
    OomdContext& ctx,
    bool ignore_threshold) {
  int64_t cur_memcurrent = 0;
  for (const auto& cgroup : cgroups_) {
    cur_memcurrent += Fs::readMemcurrentWildcard(cgroup.absolutePath());
  }

  // Sort all the cgroups by (size - memory.low) and remove all the cgroups
  // we are not assigned to kill
  auto size_sorted = ctx.reverseSort([](const CgroupContext& cgroup_ctx) {
    return cgroup_ctx.current_usage - cgroup_ctx.memory_low;
  });
  Base::removeSiblingCgroups(cgroups_, size_sorted);
  OomdContext::dumpOomdContext(size_sorted, !debug_);

  // First try to kill the biggest cgroup over it's assigned memory.low
  for (const auto& state_pair : size_sorted) {
    if (!ignore_threshold &&
        state_pair.second.current_usage <
            (cur_memcurrent * (static_cast<double>(size_threshold_) / 100))) {
      OLOG << "Skipping size heuristic kill on "
           << state_pair.first.relativePath() << " b/c not big enough";
      break;
    }

    OLOG << "Picked \"" << state_pair.first.relativePath() << "\" ("
         << state_pair.second.current_usage / 1024 / 1024
         << "MB) based on size > " << size_threshold_ << "% of total "
         << cur_memcurrent / 1024 / 1024 << "MB";

    if (Base::tryToKillCgroup(state_pair.first.absolutePath(), true, dry_)) {
      Base::logKill(
          state_pair.first, state_pair.second, ctx.getActionContext(), dry_);
      return true;
    }
  }

  return false;
}

template <typename Base>
bool KillMemoryGrowth<Base>::tryToKillByGrowth(OomdContext& ctx) {
  // Pick the top P(growing_size_percentile_) and sort them by the growth rate
  // (current usage / avg usage) and try to kill the highest one.
  auto growth_sorted = ctx.reverseSort([](const CgroupContext& cgroup_ctx) {
    return cgroup_ctx.current_usage - cgroup_ctx.memory_low;
  });
  const size_t nr = std::ceil(
      growth_sorted.size() *
      (100 - static_cast<double>(growing_size_percentile_)) / 100);
  while (growth_sorted.size() > nr) {
    growth_sorted.pop_back();
  }
  OomdContext::reverseSort(growth_sorted, [](const CgroupContext& cgroup_ctx) {
    return static_cast<double>(cgroup_ctx.current_usage) /
        cgroup_ctx.average_usage;
  });
  OomdContext::dumpOomdContext(growth_sorted, !debug_);

  for (const auto& state_pair : growth_sorted) {
    float growth_ratio = static_cast<double>(state_pair.second.current_usage) /
        state_pair.second.average_usage;
    if (growth_ratio < min_growth_ratio_) {
      OLOG << "Skipping growth heuristic kill on "
           << state_pair.first.relativePath() << " b/c growth is less than "
           << min_growth_ratio_;
      break;
    }

    std::ostringstream oss;
    oss << std::setprecision(2) << std::fixed;
    oss << "Picked \"" << state_pair.first.relativePath() << "\" ("
        << state_pair.second.current_usage / 1024 / 1024
        << "MB) based on growth rate "
        << static_cast<double>(state_pair.second.current_usage) /
            state_pair.second.average_usage
        << " among P" << growing_size_percentile_ << " largest";
    OLOG << oss.str();

    if (Base::tryToKillCgroup(state_pair.first.absolutePath(), true, dry_)) {
      Base::logKill(
          state_pair.first, state_pair.second, ctx.getActionContext(), dry_);
      return true;
    }
  }

  return false;
}

} // namespace Oomd
