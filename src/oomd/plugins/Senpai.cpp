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

#include <pthread.h>

#include <cerrno>
#include <csignal>
#include <future>
#include <iomanip>
#include <sstream>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/PluginArgParser.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(senpai, Senpai::create);

int Senpai::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  argParser_.addArgumentCustom(
      "cgroup",
      cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      },
      true);
  argParser_.addArgument("limit_min_bytes", limit_min_bytes_);
  argParser_.addArgument("limit_max_bytes", limit_max_bytes_);
  argParser_.addArgument("interval", interval_);
  argParser_.addArgument("pressure_ms", pressure_ms_);
  argParser_.addArgument("pressure_pct", mem_pressure_pct_);
  argParser_.addArgument("io_pressure_pct", io_pressure_pct_);
  argParser_.addArgument("max_probe", max_probe_);
  argParser_.addArgument("max_backoff", max_backoff_);
  argParser_.addArgument("coeff_probe", coeff_probe_);
  argParser_.addArgument("coeff_backoff", coeff_backoff_);
  argParser_.addArgument("immediate_backoff", immediate_backoff_);
  argParser_.addArgument("memory_high_timeout_ms", memory_high_timeout_);
  argParser_.addArgument("swap_threshold", swap_threshold_);
  argParser_.addArgument("swapout_bps_threshold", swapout_bps_threshold_);
  argParser_.addArgument("swap_validation", swap_validation_);
  argParser_.addArgument("modulate_swappiness", modulate_swappiness_);
  argParser_.addArgument("log_interval", log_interval_);

  if (!argParser_.parse(args)) {
    return 1;
  }

  auto meminfo = Fs::getMeminfo();
  // TODO(dschatzberg): Report Error
  if (meminfo) {
    if (auto pos = meminfo->find("MemTotal"); pos != meminfo->end()) {
      host_mem_total_ = pos->second;
    }
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
  if (++log_ticks_ >= log_interval_) {
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
            << state.probe_count << " probe attempts (" << std::setprecision(3)
            << std::fixed << state.probe_bytes / (double)(1 << 30UL) << " gb)";
        OLOG << oss.str();
        // Reset stats
        state.probe_count = 0;
        state.probe_bytes = 0;
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
  // Senpai reads pressure.some to get early notice that a workload
  // may be under resource pressure
  if (const auto pressure = Oomd::Fs::readMempressureAt(
          cgroup_ctx.fd(), Oomd::Fs::PressureType::SOME)) {
    if (const auto total = pressure.value().total) {
      return total.value();
    }
    throw std::runtime_error("Senpai enabled but no total pressure info");
  }
  return std::nullopt;
}
} // namespace

// Check if the system support memory.reclaim cgroup control file. If the given
// cgroup supports it, the system supports it. The result is then stored and
// further calls won't access filesystem. If no stored result exists and the
// cgroup does not has memory controller enabled or is no longer valid, nullopt
// is returned.
std::optional<bool> Senpai::hasMemoryReclaim(const CgroupContext& cgroup_ctx) {
  if (!has_memory_reclaim_.has_value()) {
    if (auto controllers_maybe = Fs::readControllersAt(cgroup_ctx.fd());
        controllers_maybe) {
      for (const auto& ctrl : *controllers_maybe) {
        if (ctrl == "memory") {
          has_memory_reclaim_ =
              (bool)Fs::checkExistAt(cgroup_ctx.fd(), Fs::kMemReclaimFile);
          break;
        }
      }
    }
  }
  return has_memory_reclaim_;
}

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
    if (*has_memory_high_tmp) {
      if (!Oomd::Fs::writeMemhightmpAt(
              cgroup_ctx.fd(), value, std::chrono::seconds(20))) {
        return false;
      }
    } else if (!Oomd::Fs::writeMemhighAt(cgroup_ctx.fd(), value)) {
      return false;
    }
    return true;
  }
  return false;
}

/*
 * Invoke functor with some timeout. If functor does not return after timeout,
 * a signal is sent to the thread running functor to interrupt the running
 * syscall every second. Won't help if functor is uninterruptable or spinning.
 */
template <class Functor, class Duration>
SystemMaybe<typename std::invoke_result<Functor>::type> timed_invoke(
    Functor&& fn,
    Duration timeout) {
  // ensure signal handler is setup before waiting on functor execution
  std::promise<void> barrier;
  auto barrier_future = barrier.get_future();

  std::promise<typename std::invoke_result<Functor>::type> result;
  auto future = result.get_future();

  std::thread t(
      [](auto&& barrier, auto&& result, Functor&& fn) {
        // Empty signal handler to interrupt syscall in fn
        std::signal(SIGUSR1, [](int) {});
        barrier.set_value();
        result.set_value(fn());
      },
      std::move(barrier),
      std::move(result),
      std::forward<Functor>(fn));

  barrier_future.wait();
  if (future.wait_for(timeout) == std::future_status::timeout) {
    // Send signal to interrupt every second until we hear back from thread
    do {
      if (auto rc = ::pthread_kill(t.native_handle(), SIGUSR1); rc != 0) {
        // thread already gone
        if (rc == ESRCH) {
          break;
        }
        // Something very wrong...
        OLOG << systemError(rc, "pthread_kill failed").error().what();
        std::terminate();
      }
    } while (future.wait_for(std::chrono::seconds(1)) ==
             std::future_status::timeout);
    t.join();
    return systemError(ETIMEDOUT, "Timed out waiting execution");
  } else {
    t.join();
    return future.get();
  }
}

// Call writeMemhigh in a different thread and send signal to interrupt write
// after timeout. Workaround for a kernel "feature" that blocks such write
// indefinitely if reclaim target is too low.
bool Senpai::writeMemhighTimeout(
    const CgroupContext& cgroup_ctx,
    int64_t value,
    std::chrono::milliseconds timeout) {
  auto valid_maybe =
      timed_invoke([&]() { return writeMemhigh(cgroup_ctx, value); }, timeout);
  if (!valid_maybe) {
    // Most likely write timed out. Assume cgroup still valid and verify later.
    OLOG << "Failed to write memory limit for "
         << cgroup_ctx.cgroup().relativePath() << ": "
         << valid_maybe.error().what();
    return true;
  } else {
    return valid_maybe.value();
  }
}

// Reset memory.high.tmp (preferred) or memory.high of a given cgroup to max.
// Return if the cgroup is still valid.
bool Senpai::resetMemhigh(const CgroupContext& cgroup_ctx) {
  if (auto has_memory_high_tmp = hasMemoryHighTmp(cgroup_ctx)) {
    auto value = std::numeric_limits<int64_t>::max();
    if (*has_memory_high_tmp) {
      if (!Oomd::Fs::writeMemhightmpAt(
              cgroup_ctx.fd(), value, std::chrono::seconds(0))) {
        return false;
      }
    } else if (!Oomd::Fs::writeMemhighAt(cgroup_ctx.fd(), value)) {
      return false;
    }
    return true;
  }
  return false;
}

// Reclaim some number of bytes from the given cgroup.
bool Senpai::reclaim(const CgroupContext& cgroup_ctx, int64_t size) {
  auto has_memory_reclaim_opt = hasMemoryReclaim(cgroup_ctx);
  if (has_memory_reclaim_opt && *has_memory_reclaim_opt) {
    return (bool)Fs::writeMemReclaimAt(cgroup_ctx.fd(), size, 0);
  }

  auto current_opt = cgroup_ctx.current_usage();
  if (!current_opt) {
    return false;
  }
  int64_t limit = *current_opt - size;

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
  return true;
}

/** Returns file cache + swappable anon. */
SystemMaybe<int64_t> Senpai::getReclaimableBytes(
    const CgroupContext& cgroup_ctx) {
  const auto& stat_opt = cgroup_ctx.memory_stat();
  if (!stat_opt) {
    return SYSTEM_ERROR(ENOENT);
  }

  auto active_file_pos = stat_opt->find("active_file");
  auto inactive_file_pos = stat_opt->find("inactive_file");
  if (active_file_pos == stat_opt->end() ||
      inactive_file_pos == stat_opt->end()) {
    throw std::runtime_error("Invalid memory.stat cgroup file");
  }
  auto file_cache = active_file_pos->second + inactive_file_pos->second;

  int64_t swappable = 0;
  const auto& system_ctx = cgroup_ctx.oomd_ctx().getSystemContext();
  if (system_ctx.swaptotal > 0 && system_ctx.swappiness > 0) {
    auto effective_swap_free_opt = cgroup_ctx.effective_swap_free();
    if (!effective_swap_free_opt) {
      return SYSTEM_ERROR(ENOENT);
    } else if (*effective_swap_free_opt > 0) {
      auto active_anon_pos = stat_opt->find("active_anon");
      auto inactive_anon_pos = stat_opt->find("inactive_anon");
      if (active_anon_pos == stat_opt->end() ||
          inactive_anon_pos == stat_opt->end()) {
        return SYSTEM_ERROR(EINVAL);
      }
      auto anon_size = active_anon_pos->second + inactive_anon_pos->second;
      swappable = std::min(*effective_swap_free_opt, anon_size);
    }
  }

  return file_cache + swappable;
}

/** Returns unreclaimable + limit_min_bytes. */
std::optional<int64_t> Senpai::getLimitMinBytes(
    const CgroupContext& cgroup_ctx) {
  auto memcurr_opt = cgroup_ctx.current_usage();
  if (!memcurr_opt) {
    return std::nullopt;
  }
  auto reclaimable_maybe = getReclaimableBytes(cgroup_ctx);
  if (!reclaimable_maybe) {
    return std::nullopt;
  }
  auto unreclaimable = *memcurr_opt - *reclaimable_maybe;
  auto limit_min_bytes = limit_min_bytes_ + unreclaimable;

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
  auto factor = 0.0;

  if (*limit_opt != state.limit) {
    // Something else changed limits on this cgroup or it was
    // recreated in-between ticks - reset the state and return,
    // unfortuantely, the rest of this logic is still racy after this
    // point
    std::ostringstream oss;
    oss << "cgroup " << name << " memory.high " << *limit_opt
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
        << " limitgb " << *limit_opt / (double)(1 << 30UL) << " totalus "
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
    if (*limit_opt > state.limit) {
      state.probe_count++;
      state.probe_bytes += *limit_opt - state.limit;
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
      int original_swappiness;
      if (modulate_swappiness_) {
        original_swappiness =
            cgroup_ctx.oomd_ctx().getSystemContext().swappiness;
        auto swappiness_factor_maybe = calculateSwappinessFactor(cgroup_ctx);
        if (!swappiness_factor_maybe) {
          return false;
        }
        Fs::setSwappiness(original_swappiness * (*swappiness_factor_maybe));
      }
      OOMD_SCOPE_EXIT {
        if (modulate_swappiness_) {
          Fs::setSwappiness(original_swappiness);
        }
      };

      // Reclaim slowly towards limit_min_bytes
      int64_t reclaim_size = (*current_opt - *limit_min_bytes_opt) * max_probe_;
      // Reclaim in number of 4k pages
      reclaim_size &= ~0xFFF;
      if (!reclaim(cgroup_ctx, reclaim_size)) {
        return false;
      }
      state.probe_count++;
      state.probe_bytes += reclaim_size;
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

// Validate that pressure is low enough to drive Senpai
SystemMaybe<bool> Senpai::validatePressure(
    const CgroupContext& cgroup_ctx) const {
  auto mem_pressure_opt = cgroup_ctx.mem_pressure_some();
  if (!mem_pressure_opt) {
    return SYSTEM_ERROR(ENOENT);
  }
  auto io_pressure_opt = cgroup_ctx.io_pressure_some();
  if (!io_pressure_opt) {
    return SYSTEM_ERROR(ENOENT);
  }

  // Only drive senpai if both short and long term pressure from memory and I/O
  // are lower than target
  return std::max(mem_pressure_opt->sec_10, mem_pressure_opt->sec_60) <
      mem_pressure_pct_ &&
      std::max(io_pressure_opt->sec_10, io_pressure_opt->sec_60) <
      io_pressure_pct_;
}

// Validate that swap is sufficient to run Senpai
SystemMaybe<bool> Senpai::validateSwap(const CgroupContext& cgroup_ctx) const {
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

// Calculate swappiness factor (between 0 and 1) for a cgroup to modulate swap
// behavior.
SystemMaybe<double> Senpai::calculateSwappinessFactor(
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
