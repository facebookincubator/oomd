/*
 * Copyright (C) 2026-present, Meta Platforms, Inc. and affiliates
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

#include "oomd/plugins/SwapAboveMemoryMax.h"

#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"

namespace Oomd {
namespace {

bool isAboveThreshold(
    uint64_t swap_bytes,
    uint64_t memory_max_bytes,
    uint64_t threshold_pct) {
  // Evaluate 100 * swap_bytes > memory_max_bytes * threshold_pct without
  // forming either potentially overflowing product.
  const uint64_t swap_whole = swap_bytes / memory_max_bytes;
  const uint64_t threshold_whole = threshold_pct / 100;
  if (swap_whole != threshold_whole) {
    return swap_whole > threshold_whole;
  }

  const uint64_t swap_remainder = swap_bytes % memory_max_bytes;
  const uint64_t threshold_remainder = threshold_pct % 100;

  // floor(memory_max_bytes * threshold_remainder / 100), decomposed so
  // neither multiplication can overflow. Since swap_remainder is integral,
  // comparing it with this floor is equivalent to the strict cross-product
  // comparison swap_bytes * 100 > memory_max_bytes * threshold_pct.
  const uint64_t threshold_remainder_bytes =
      (memory_max_bytes / 100) * threshold_remainder +
      ((memory_max_bytes % 100) * threshold_remainder) / 100;
  return swap_remainder > threshold_remainder_bytes;
}

} // namespace

REGISTER_PLUGIN(swap_above_memory_max, SwapAboveMemoryMax::create);

int SwapAboveMemoryMax::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  argParser_.addArgumentCustom(
      "cgroup", cgroups_, [context](const std::string& cgroup_str) {
        return PluginArgParser::parseCgroup(context, cgroup_str);
      });
  argParser_.addArgumentCustom(
      "ruleset_cgroup",
      ruleset_cgroups_,
      [context](const std::string& cgroup_str) {
        return PluginArgParser::parseCgroup(context, cgroup_str);
      });
  argParser_.addArgumentCustom(
      "threshold_pct",
      threshold_pct_,
      [](const std::string& threshold) {
        size_t parsed = 0;
        const auto value = std::stoll(threshold, &parsed);
        if (parsed != threshold.size() || value < 0 ||
            value > std::numeric_limits<int>::max()) {
          throw std::invalid_argument("must be a non-negative integer");
        }
        return static_cast<int>(value);
      },
      true);
  argParser_.addArgument("debug", debug_);

  return argParser_.parse(args) ? 0 : 1;
}

Engine::PluginRet SwapAboveMemoryMax::run(OomdContext& ctx) {
  const auto cgroup_contexts = ctx.addToCacheAndGet(cgroups_, ruleset_cgroups_);
  if (cgroup_contexts.empty() && debug_) {
    OLOG << "swap_above_memory_max reason=no_matching_cgroups"
         << " threshold_pct=" << threshold_pct_;
  }

  bool above_threshold = false;
  for (const CgroupContext& cgroup_ctx : cgroup_contexts) {
    const auto swap_bytes = cgroup_ctx.swap_usage();
    const auto memory_max_bytes = cgroup_ctx.memory_max();
    const char* reason = nullptr;
    bool current_above_threshold = false;
    bool ratio_available = false;

    if (!swap_bytes) {
      reason = "missing_swap_current";
    } else if (*swap_bytes < 0) {
      reason = "negative_swap_current";
    } else if (!memory_max_bytes) {
      reason = "missing_memory_max";
    } else if (*memory_max_bytes == std::numeric_limits<int64_t>::max()) {
      reason = "unlimited_memory_max";
    } else if (*memory_max_bytes == 0) {
      reason = "zero_memory_max";
    } else if (*memory_max_bytes < 0) {
      reason = "negative_memory_max";
    } else {
      ratio_available = true;
      current_above_threshold = isAboveThreshold(
          static_cast<uint64_t>(*swap_bytes),
          static_cast<uint64_t>(*memory_max_bytes),
          static_cast<uint64_t>(threshold_pct_));
      reason =
          current_above_threshold ? "above_threshold" : "at_or_below_threshold";
    }

    if (debug_) {
      const auto ratio_pct = ratio_available
          ? std::to_string(
                static_cast<long double>(*swap_bytes) * 100 / *memory_max_bytes)
          : "unavailable";
      OLOG << "swap_above_memory_max cgroup=\""
           << cgroup_ctx.cgroup().relativePath() << "\""
           << " swap_bytes="
           << (swap_bytes ? std::to_string(*swap_bytes) : "missing")
           << " memory_max_bytes="
           << (memory_max_bytes ? std::to_string(*memory_max_bytes) : "missing")
           << " ratio_pct=" << ratio_pct << " threshold_pct=" << threshold_pct_
           << " reason=" << reason;
    }

    above_threshold |= current_above_threshold;
  }

  return above_threshold ? Engine::PluginRet::CONTINUE
                         : Engine::PluginRet::STOP;
}

} // namespace Oomd
