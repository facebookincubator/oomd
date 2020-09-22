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

#include "oomd/plugins/Senpai.h"

#include <iomanip>
#include <sstream>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(senpai, Senpai::create);

int Senpai::init(
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

  if (args.find("limit_min_bytes") != args.end()) {
    limit_min_bytes_ = std::stoull(args.at("limit_min_bytes"));
  }

  if (args.find("limit_max_bytes") != args.end()) {
    limit_max_bytes_ = std::stoull(args.at("limit_max_bytes"));
  }

  if (args.find("interval") != args.end()) {
    interval_ = std::stoull(args.at("interval"));
  }

  if (args.find("pressure_ms") != args.end()) {
    pressure_ms_ =
        std::chrono::milliseconds(std::stoull(args.at("pressure_ms")));
  }

  // Currently only used for immediate backoff
  if (args.find("pressure_pct") != args.end()) {
    pressure_pct_ = std::stod(args.at("pressure_pct"));
  }

  if (args.find("max_probe") != args.end()) {
    max_probe_ = std::stod(args.at("max_probe"));
  }

  if (args.find("max_backoff") != args.end()) {
    max_backoff_ = std::stod(args.at("max_backoff"));
  }

  if (args.find("coeff_probe") != args.end()) {
    coeff_probe_ = std::stod(args.at("coeff_probe"));
  }

  if (args.find("coeff_backoff") != args.end()) {
    coeff_backoff_ = std::stod(args.at("coeff_backoff"));
  }

  if (args.find("immediate_backoff") != args.end()) {
    const std::string& val = args.at("immediate_backoff");

    if (val == "true" || val == "True" || val == "1") {
      immediate_backoff_ = true;
    }
  }

  if (args.find("log_interval") != args.end()) {
    log_interval_ = std::stoull(args.at("log_interval"));
  }

  auto meminfo = Fs::getMeminfo();
  if (auto pos = meminfo.find("MemTotal"); pos != meminfo.end()) {
    host_mem_total_ = pos->second;
  } else {
    OLOG << "Cannot read host MemTotal";
    return 1;
  }

  return 0;
}

Engine::PluginRet Senpai::run(OomdContext& ctx) {
  auto resolved_cgroups = ctx.reverseSort(
      cgroups_,
      [](const CgroupContext& cgroup_ctx) { return cgroup_ctx.id(); });
  // Use reverse iterator after reverseSort to make it normal order
  auto resolvedIt = resolved_cgroups.crbegin();
  auto trackedIt = tracked_cgroups_.begin();

  bool do_aggregate_log = false;
  if (immediate_backoff_ && ++log_ticks_ >= log_interval_) {
    log_ticks_ = 0;
    do_aggregate_log = true;
  }

  // Iterate both tracked cgroups and resolved cgroups in increasing id order
  while (resolvedIt != resolved_cgroups.crend()) {
    const CgroupContext& cgroup_ctx = *resolvedIt;
    // Use id to identify CgroupContext across intervals, as path, dir_fd, and
    // memory address could all be recycled upon cgroup recreation.
    auto id_opt = cgroup_ctx.id();
    if (!id_opt) {
      continue;
    }
    if (trackedIt == tracked_cgroups_.end() || *id_opt < trackedIt->first) {
      // Resolved cgroup not in tracked map, track it
      // New cgroups will be polled after a "tick" has elapsed
      if (auto new_cgroup_state_opt = initializeCgroup(cgroup_ctx)) {
        tracked_cgroups_.emplace_hint(
            trackedIt, *id_opt, *new_cgroup_state_opt);
      }
      ++resolvedIt;
    } else if (*cgroup_ctx.id() > trackedIt->first) {
      trackedIt = tracked_cgroups_.erase(trackedIt);
    } else {
      bool tick_result = immediate_backoff_
          ? tick_immediate_backoff(cgroup_ctx, trackedIt->second)
          : tick(cgroup_ctx, trackedIt->second);
      if (do_aggregate_log && tick_result) {
        auto& state = trackedIt->second;
        std::ostringstream oss;
        oss << "cgroup " << cgroup_ctx.cgroup().relativePath() << " "
            << state.reclaim_count << " reclaim attempts ("
            << std::setprecision(3) << std::fixed
            << state.reclaim_bytes / (double)(1 << 30UL) << " gb)";
        OLOG << oss.str();
        // Reset stats
        state.reclaim_count = 0;
        state.reclaim_bytes = 0;
      }
      // Keep the tracked cgroups if they are still valid after tick
      trackedIt = tick_result ? std::next(trackedIt)
                              : tracked_cgroups_.erase(trackedIt);
      ++resolvedIt;
    }
  }
  tracked_cgroups_.erase(trackedIt, tracked_cgroups_.end());
  return Engine::PluginRet::CONTINUE;
}

Senpai::CgroupState::CgroupState(
    int64_t start_limit,
    std::chrono::microseconds total,
    int64_t start_ticks)
    : limit{start_limit}, last_total{total}, ticks{start_ticks} {}

namespace {
// Get the total pressure (some) from a cgroup, or nullopt if cgroup is invalid
std::optional<std::chrono::microseconds> getPressureTotalSome(
    const CgroupContext& cgroup_ctx) {
  try {
    // Senpai reads pressure.some to get early notice that a workload
    // may be under resource pressure
    if (const auto pressure = Oomd::Fs::readMempressureAt(
            cgroup_ctx.fd(), Oomd::Fs::PressureType::SOME)) {
      if (const auto total = pressure.value().total) {
        return total.value();
      }
      throw std::runtime_error("Senpai enabled but no total pressure info");
    }
  } catch (const Fs::bad_control_file&) {
  }
  return std::nullopt;
}
} // namespace

// Check if the system support memory.high.tmp cgroup control file. If the given
// cgroup supports it, the system supports it. The result is then stored and
// further calls won't access filesystem. If the cgroup is no longer valid and
// no stored result exists, nullopt is returned.
std::optional<bool> Senpai::hasMemoryHighTmp(const CgroupContext& cgroup_ctx) {
  if (!has_memory_high_tmp_.has_value()) {
    if (auto memhightmp = cgroup_ctx.memory_high_tmp()) {
      has_memory_high_tmp_ = true;
    } else if (auto memhigh = cgroup_ctx.memory_high()) {
      // If memory.high exists but memory.high.tmp doesn't, it's not supported
      has_memory_high_tmp_ = false;
    }
    // If neither exist, cgroup is invalid. Nothing changed.
  }
  return has_memory_high_tmp_;
}

// Read from memory.high.tmp (preferred) or memory.high of a given cgroup.
// Return nullopt if cgroup is no longer valid.
std::optional<int64_t> Senpai::readMemhigh(const CgroupContext& cgroup_ctx) {
  if (auto has_memory_high_tmp = hasMemoryHighTmp(cgroup_ctx)) {
    return *has_memory_high_tmp ? cgroup_ctx.memory_high_tmp()
                                : cgroup_ctx.memory_high();
  }
  return std::nullopt;
}

// Write to memory.high.tmp (preferred) or memory.high of a given cgroup.
// Return if the cgroup is still valid.
bool Senpai::writeMemhigh(const CgroupContext& cgroup_ctx, int64_t value) {
  if (auto has_memory_high_tmp = hasMemoryHighTmp(cgroup_ctx)) {
    try {
      if (*has_memory_high_tmp) {
        Oomd::Fs::writeMemhightmpAt(
            cgroup_ctx.fd(), value, std::chrono::seconds(20));
      } else {
        Oomd::Fs::writeMemhighAt(cgroup_ctx.fd(), value);
      }
      return true;
    } catch (const Fs::bad_control_file&) {
    }
  }
  return false;
}

// Reset memory.high.tmp (preferred) or memory.high of a given cgroup to max.
// Return if the cgroup is still valid.
bool Senpai::resetMemhigh(const CgroupContext& cgroup_ctx) {
  if (auto has_memory_high_tmp = hasMemoryHighTmp(cgroup_ctx)) {
    try {
      auto value = std::numeric_limits<int64_t>::max();
      if (*has_memory_high_tmp) {
        Oomd::Fs::writeMemhightmpAt(
            cgroup_ctx.fd(), value, std::chrono::seconds(0));
      } else {
        Oomd::Fs::writeMemhighAt(cgroup_ctx.fd(), value);
      }
      return true;
    } catch (const Fs::bad_control_file&) {
    }
  }
  return false;
}

/**
 * Return the maximum of the following:
 *  memory.current - file_cache + limit_min_bytes (default: 100M)
 *    where file_cache = memory.stat[active_file] + memory.stat[inactive_file]
 *  memory.min
 */
std::optional<int64_t> Senpai::getLimitMinBytes(
    const CgroupContext& cgroup_ctx) {
  const auto& stat_opt = cgroup_ctx.memory_stat();
  if (!stat_opt) {
    return std::nullopt;
  }
  auto active_file_pos = stat_opt->find("active_file");
  auto inactive_file_pos = stat_opt->find("inactive_file");
  if (active_file_pos == stat_opt->end() ||
      inactive_file_pos == stat_opt->end()) {
    throw std::runtime_error("Invalid memory.stat cgroup file");
  }
  auto file_cache = active_file_pos->second + inactive_file_pos->second;
  auto memcurr_opt = cgroup_ctx.current_usage();
  if (!memcurr_opt) {
    return std::nullopt;
  }
  // TODO(lnyng): test the effect of swap and take that into account
  // Set limit min bytes based on unreclaimable memory
  auto limit_min_bytes = limit_min_bytes_ + (*memcurr_opt - file_cache);

  auto memmin_opt = cgroup_ctx.memory_min();
  if (!memmin_opt) {
    return std::nullopt;
  }
  // Make sure memory.high don't go below memory.min
  limit_min_bytes = std::max(limit_min_bytes, *memmin_opt);

  return limit_min_bytes;
}

/**
 * Return the minimum of the following:
 *  /proc/meminfo[MemTotal]
 *  memory.current + limit_max_bytes (default: 10G)
 *  memory.high (only if memory.high.tmp exist)
 *  memory.max
 */
std::optional<int64_t> Senpai::getLimitMaxBytes(
    const CgroupContext& cgroup_ctx) {
  auto memcurr_opt = cgroup_ctx.current_usage();
  if (!memcurr_opt) {
    return std::nullopt;
  }
  auto limit_max_bytes =
      std::min(host_mem_total_, limit_max_bytes_ + *memcurr_opt);

  // Don't let memory.high.tmp go above memory.high as kernel ignores the
  // latter when the former is set.
  auto has_memory_high_tmp_opt = hasMemoryHighTmp(cgroup_ctx);
  if (!has_memory_high_tmp_opt) {
    return std::nullopt;
  }
  if (*has_memory_high_tmp_opt) {
    auto memhigh_opt = cgroup_ctx.memory_high();
    if (!memhigh_opt) {
      return std::nullopt;
    }
    limit_max_bytes = std::min(limit_max_bytes, *memhigh_opt);
  }

  auto memmax_opt = cgroup_ctx.memory_max();
  if (!memmax_opt) {
    return std::nullopt;
  }
  limit_max_bytes = std::min(limit_max_bytes, *memmax_opt);

  return limit_max_bytes;
}

// Update state of a cgroup. Return if the cgroup is still valid.
bool Senpai::tick(const CgroupContext& cgroup_ctx, CgroupState& state) {
  auto name = cgroup_ctx.cgroup().absolutePath();
  auto limit_opt = readMemhigh(cgroup_ctx);
  if (!limit_opt) {
    return false;
  }
  auto limit = *limit_opt;
  auto factor = 0.0;

  if (*limit_opt != state.limit) {
    // Something else changed limits on this cgroup or it was
    // recreated in-between ticks - reset the state and return,
    // unfortuantely, the rest of this logic is still racy after this
    // point
    std::ostringstream oss;
    oss << "cgroup " << name << " memory.high " << limit
        << " does not match recorded state " << state.limit
        << ". Resetting cgroup";
    OLOG << oss.str();
    if (auto state_opt = initializeCgroup(cgroup_ctx)) {
      state = *state_opt;
      return true;
    }
    return false;
  }

  // Adjust cgroup limit by factor
  auto adjust = [&](double factor) {
    auto limit_min_bytes_opt = getLimitMinBytes(cgroup_ctx);
    if (!limit_min_bytes_opt) {
      return false;
    }
    auto limit_max_bytes_opt = getLimitMaxBytes(cgroup_ctx);
    if (!limit_max_bytes_opt) {
      return false;
    }

    state.limit += state.limit * factor;
    state.limit = std::max(
        *limit_min_bytes_opt, std::min(*limit_max_bytes_opt, state.limit));
    // Memory high is always a multiple of 4K
    state.limit &= ~0xFFF;
    state.ticks = interval_;
    state.cumulative = std::chrono::microseconds{0};
    return writeMemhigh(cgroup_ctx, state.limit);
  };
  auto total_opt = getPressureTotalSome(cgroup_ctx);
  if (!total_opt) {
    return false;
  }
  auto total = *total_opt;
  auto delta = total - state.last_total;
  state.last_total = total;
  state.cumulative += delta;
  auto cumulative = state.cumulative.count();

  if (state.cumulative >= pressure_ms_) {
    // Excessive pressure, back off. The rate scales exponentially
    // with pressure deviation. The coefficient defines how sensitive
    // we are to fluctuations around the target pressure: when the
    // coefficient is 10, the adjustment curve reaches the backoff
    // limit when observed pressure is ten times the target pressure.
    double error = state.cumulative / pressure_ms_;
    factor = error / coeff_backoff_;
    factor *= factor;
    factor = std::min(factor * max_backoff_, max_backoff_);
    if (!adjust(factor)) {
      return false;
    }

    std::ostringstream oss;
    oss << "cgroup " << name << std::setprecision(3) << std::fixed
        << " limitgb " << limit / (double)(1 << 30UL) << " totalus "
        << total.count() << " deltaus " << delta.count() << " cumus "
        << cumulative << " ticks " << state.ticks << std::defaultfloat
        << " adjust " << factor;
    OLOG << oss.str();
  } else if (state.ticks) {
    --state.ticks;
  } else {
    // Pressure too low, tighten the limit. Like when backing off, the
    // adjustment becomes exponentially more aggressive as observed
    // pressure falls below the target pressure. The adjustment limit
    // is reached when stall time falls through pressure/coeff_probe_.
    auto one = std::chrono::microseconds{1};
    double error = pressure_ms_ / std::max(state.cumulative, one);
    factor = error / coeff_probe_;
    factor *= factor;
    factor = std::min(factor * max_probe_, max_probe_);
    factor = -factor;
    if (!adjust(factor)) {
      return false;
    }
  }
  return true;
}

// Update state of a cgroup. Return if the cgroup is still valid.
bool Senpai::tick_immediate_backoff(
    const CgroupContext& cgroup_ctx,
    CgroupState& state) {
  // Wait for interval to prevent making senpai too aggressive
  // May wait longer if pressures are too high
  if (state.ticks) {
    state.ticks--;
    return true;
  }

  auto mem_pressure_opt = Fs::readMempressureAt(cgroup_ctx.fd());
  if (!mem_pressure_opt) {
    return false;
  }
  auto io_pressure_opt = Fs::readIopressureAt(cgroup_ctx.fd());
  if (!io_pressure_opt) {
    return false;
  }

  // Only drive senpai if both short and long term pressure from memory and I/O
  // are lower than target
  if (std::max({mem_pressure_opt->sec_10,
                mem_pressure_opt->sec_60,
                io_pressure_opt->sec_10,
                io_pressure_opt->sec_60}) < pressure_pct_) {
    auto limit_min_bytes_opt = getLimitMinBytes(cgroup_ctx);
    if (!limit_min_bytes_opt) {
      return false;
    }
    auto current_opt = cgroup_ctx.current_usage();
    if (!current_opt) {
      return false;
    }
    if (*current_opt > *limit_min_bytes_opt) {
      // Set the limit to somewhere between memory.current and lower limit
      int64_t limit = *limit_min_bytes_opt +
          (*current_opt - *limit_min_bytes_opt) * (1 - max_probe_);
      // Memory high is always a multiple of 4K
      limit &= ~0xFFF;
      // Poking by setting memory limit and immediately resetting it, which
      // prevents sudden allocation later from triggering thrashing
      if (!writeMemhigh(cgroup_ctx, limit) || !resetMemhigh(cgroup_ctx)) {
        return false;
      }
      state.reclaim_count++;
      state.reclaim_bytes += *current_opt - limit;
      state.ticks = interval_;
    }
  }

  return true;
}

// Initialize a CgroupState. Return nullopt if cgroup no longer valid.
std::optional<Senpai::CgroupState> Senpai::initializeCgroup(
    const CgroupContext& cgroup_ctx) {
  int64_t start_limit = 0;
  // Immediate backoff does not use limit as a state.
  if (!immediate_backoff_) {
    auto current_opt = cgroup_ctx.current_usage();
    if (!current_opt) {
      return std::nullopt;
    }
    if (!writeMemhigh(cgroup_ctx, *current_opt)) {
      return std::nullopt;
    }
    start_limit = *current_opt;
  }
  auto total_opt = getPressureTotalSome(cgroup_ctx);
  if (!total_opt) {
    return std::nullopt;
  }
  return CgroupState(start_limit, *total_opt, interval_);
}

} // namespace Oomd
