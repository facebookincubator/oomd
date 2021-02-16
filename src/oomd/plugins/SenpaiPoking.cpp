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

#include "oomd/plugins/SenpaiPoking.h"

#include <iomanip>

#include "oomd/PluginRegistry.h"
#include "oomd/util/ScopeGuard.h"

namespace Oomd {

REGISTER_PLUGIN(senpai_poking, SenpaiPoking::create);

int SenpaiPoking::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  if (SenpaiCommon<SenpaiPoking>::init(args, context)) {
    return 1;
  }

  if (args.find("pressure_pct") != args.end()) {
    mem_pressure_pct_ = std::stod(args.at("pressure_pct"));
  }

  if (args.find("io_pressure_pct") != args.end()) {
    io_pressure_pct_ = std::stod(args.at("pressure_pct"));
  }

  if (args.find("swap_threshold") != args.end()) {
    swap_threshold_ = std::stod(args.at("swap_threshold"));
  }

  if (args.find("swapout_bps_threshold") != args.end()) {
    swapout_bps_threshold_ = std::stoull(args.at("swapout_bps_threshold"));
  }

  if (args.find("swap_validation") != args.end()) {
    const std::string& val = args.at("swap_validation");

    if (val == "true" || val == "True" || val == "1") {
      swap_validation_ = true;
    }
  }

  if (args.find("modulate_swappiness") != args.end()) {
    const std::string& val = args.at("modulate_swappiness");

    if (val == "true" || val == "True" || val == "1") {
      modulate_swappiness_ = true;
    }
  }

  return 0;
}

SystemMaybe<Unit> SenpaiPoking::initializeCgroup(
    const CgroupContext& /* unused  */,
    CgroupState& /* unused */) {
  return noSystemError();
}

bool SenpaiPoking::tick(
    const CgroupContext& cgroup_ctx,
    SenpaiPoking::CgroupState& state) {
  // Wait for interval to prevent making senpai too aggressive
  // May wait longer if pressures are too high
  if (++state.ticks < interval_) {
    return true;
  }

  auto validate_pressure_maybe = validatePressure(cgroup_ctx);
  if (!validate_pressure_maybe) {
    return false;
  }

  auto validate = *validate_pressure_maybe;
  if (swap_validation_) {
    auto validate_swap_maybe = validateSwap(cgroup_ctx);
    if (!validate_swap_maybe) {
      return false;
    }
    validate = validate && *validate_swap_maybe;
  }
  if (validate) {
    auto limit_min_bytes_opt = getLimitMinBytes(cgroup_ctx);
    if (!limit_min_bytes_opt) {
      return false;
    }
    auto current_opt = cgroup_ctx.current_usage();
    if (!current_opt) {
      return false;
    }
    if (*current_opt > *limit_min_bytes_opt) {
      double swap_factor = 1.0;
      if (modulate_swappiness_) {
        auto swap_factor_maybe = calculateSwapFactor(cgroup_ctx);
        if (!swap_factor_maybe) {
          return false;
        }
        swap_factor = *swap_factor_maybe;
      }
      // Set the limit to somewhere between memory.current and lower limit
      // Scale down reclaim if we need to reduce swap activity
      int64_t reclaim_size =
          (*current_opt - *limit_min_bytes_opt) * max_probe_ * swap_factor;
      // Memory high is always a multiple of 4K
      int64_t limit = (*current_opt - reclaim_size) & ~0xFFF;

      int original_swappiness;
      if (modulate_swappiness_) {
        original_swappiness =
            cgroup_ctx.oomd_ctx().getSystemContext().swappiness;
        int temp_swappiness = original_swappiness * swap_factor;
        // In case swappiness == 0, we are potentially reclaiming from little
        // file cache, which is likely to trigger race with workload allocation.
        // Workaround until we get better way to trigger reclaim.
        if (temp_swappiness == 0) {
          return true;
        }
        Fs::setSwappiness(temp_swappiness);
      }
      OOMD_SCOPE_EXIT {
        if (modulate_swappiness_) {
          Fs::setSwappiness(original_swappiness);
        }
      };

      // Poking by setting memory limit and immediately resetting it, which
      // prevents sudden allocation later from triggering thrashing
      if (memory_high_timeout_.count() > 0) {
        if (!writeMemhighTimeout(cgroup_ctx, limit, memory_high_timeout_)) {
          return false;
        }
      } else {
        if (!writeMemhigh(cgroup_ctx, limit)) {
          return false;
        }
      }
      if (!resetMemhigh(cgroup_ctx)) {
        return false;
      }
      state.probe_count++;
      state.probe_bytes += *current_opt - limit;
      state.ticks = 0;
    }
  }

  return true;
}

std::ostream& SenpaiPoking::log(std::ostream& os, CgroupState& state) {
  os << state.probe_count << " probe attempts (" << std::setprecision(3)
     << std::fixed << state.probe_bytes / (double)(1 << 30UL) << "gb)";
  // Reset stats
  state.probe_count = 0;
  state.probe_bytes = 0;

  return os;
}

// Validate that pressure is low enough to drive Senpai
SystemMaybe<bool> SenpaiPoking::validatePressure(
    const CgroupContext& cgroup_ctx) const {
  auto mem_pressure_maybe =
      Fs::readMempressureAt(cgroup_ctx.fd(), Fs::PressureType::SOME);
  if (!mem_pressure_maybe) {
    return SYSTEM_ERROR(mem_pressure_maybe);
  }
  auto io_pressure_maybe =
      Fs::readIopressureAt(cgroup_ctx.fd(), Fs::PressureType::SOME);
  if (!io_pressure_maybe) {
    return SYSTEM_ERROR(io_pressure_maybe);
  }

  // Only drive senpai if both short and long term pressure from memory and I/O
  // are lower than target
  return std::max(mem_pressure_maybe->sec_10, mem_pressure_maybe->sec_60) <
      mem_pressure_pct_ &&
      std::max(io_pressure_maybe->sec_10, io_pressure_maybe->sec_60) <
      io_pressure_pct_;
}

// Validate that swap is sufficient to run Senpai
SystemMaybe<bool> SenpaiPoking::validateSwap(
    const CgroupContext& cgroup_ctx) const {
  const auto& system_ctx = cgroup_ctx.oomd_ctx().getSystemContext();
  // If there's no swap at all, then there's nothing to validate
  if (system_ctx.swaptotal == 0 || system_ctx.swappiness == 0) {
    return true;
  }

  // Similarly if effective swap.max is zero, nothing to validate
  auto effective_swap_max_opt = cgroup_ctx.effective_swap_max();
  if (!effective_swap_max_opt) {
    return SYSTEM_ERROR(ENOENT);
  }
  if (*effective_swap_max_opt == 0) {
    return true;
  }

  // We validate that the effective swap usage is below the defined
  // threshold. This is useful to prevent OOM killing due to swap
  // depletion.
  auto effective_swap_util_pct_opt = cgroup_ctx.effective_swap_util_pct();
  if (!effective_swap_util_pct_opt) {
    return SYSTEM_ERROR(ENOENT);
  }
  return *effective_swap_util_pct_opt >= swap_threshold_;
}

// Calculate swap factor (between 0 and 1) for a cgroup to modulate swap
// behavior. Zero means we should not swap for this cgroup for now.
SystemMaybe<double> SenpaiPoking::calculateSwapFactor(
    const CgroupContext& cgroup_ctx) const {
  if (swap_threshold_ <= 0) {
    return 0;
  }

  auto swapout_bps_60 = cgroup_ctx.oomd_ctx().getSystemContext().swapout_bps_60;
  auto swapout_bps_300 =
      cgroup_ctx.oomd_ctx().getSystemContext().swapout_bps_300;
  auto swapout_bps = std::max(swapout_bps_60, swapout_bps_300);
  if (swapout_bps >= swapout_bps_threshold_) {
    return 0;
  }
  // If system has swapout bps close to or above threshold, factor will be close
  // to or equal to 0. If instead rate is close to 0, factor approaches 1.
  auto limit_by_rate = 1.0 - swapout_bps / swapout_bps_threshold_;

  auto effective_swap_util_pct_opt = cgroup_ctx.effective_swap_util_pct();
  if (!effective_swap_util_pct_opt) {
    return SYSTEM_ERROR(ENOENT);
  }
  if (*effective_swap_util_pct_opt >= swap_threshold_) {
    return 0;
  }
  // If cgroup has swap usage close to or above threshold, factor will be close
  // to or equal to 0. If instead usage is close to 0, factor approaches 1.
  auto limit_by_size = 1.0 - *effective_swap_util_pct_opt / swap_threshold_;

  return std::min(limit_by_rate, limit_by_size);
}
} // namespace Oomd
