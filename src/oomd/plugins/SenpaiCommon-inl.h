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

#include <pthread.h>

#include <cerrno>
#include <csignal>
#include <future>

#include "oomd/Log.h"
#include "oomd/util/Util.h"

namespace Oomd {
template <typename T>
int SenpaiCommon<T>::init(
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

  if (args.find("max_probe") != args.end()) {
    max_probe_ = std::stod(args.at("max_probe"));
  }

  if (args.find("log_interval") != args.end()) {
    log_interval_ = std::stoull(args.at("log_interval"));
  }

  if (args.find("memory_high_timeout_ms") != args.end()) {
    memory_high_timeout_ = std::chrono::milliseconds(
        std::stoull(args.at("memory_high_timeout_ms")));
  }

  auto meminfo = Fs::getMeminfo();
  // TODO(dschatzberg): Report Error
  if (meminfo) {
    if (auto pos = meminfo->find("MemTotal"); pos != meminfo->end()) {
      host_mem_total_ = pos->second;
      return 0;
    }
  }
  OLOG << "Cannot read host MemTotal";
  return 1;
}

template <typename T>
Engine::PluginRet SenpaiCommon<T>::run(OomdContext& ctx) {
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
      if (auto new_cgroup_state_opt =
              static_cast<T&>(*this).initializeCgroup(cgroup_ctx)) {
        tracked_cgroups_.emplace_hint(
            trackedIt, *id_opt, *new_cgroup_state_opt);
      }
      ++resolvedIt;
    } else if (*cgroup_ctx.id() > trackedIt->first) {
      trackedIt = tracked_cgroups_.erase(trackedIt);
    } else {
      bool tick_result =
          static_cast<T&>(*this).tick(cgroup_ctx, trackedIt->second);
      if (do_aggregate_log && tick_result) {
        auto& state = trackedIt->second;
        std::ostringstream oss;
        oss << "cgroup " << cgroup_ctx.cgroup().relativePath() << " ";
        static_cast<T&>(*this).log(oss, state);
        OLOG << oss.str();
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

template <typename T>
std::ostream& SenpaiCommon<T>::log(
    std::ostream& os,
    CgroupState<T>& /* unused */) {
  return os;
}

// Check if the system support memory.high.tmp cgroup control file. If the given
// cgroup supports it, the system supports it. The result is then stored and
// further calls won't access filesystem. If the cgroup is no longer valid and
// no stored result exists, nullopt is returned.
template <typename T>
std::optional<bool> SenpaiCommon<T>::hasMemoryHighTmp(
    const CgroupContext& cgroup_ctx) {
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
template <typename T>
std::optional<int64_t> SenpaiCommon<T>::readMemhigh(
    const CgroupContext& cgroup_ctx) {
  if (auto has_memory_high_tmp = hasMemoryHighTmp(cgroup_ctx)) {
    return *has_memory_high_tmp ? cgroup_ctx.memory_high_tmp()
                                : cgroup_ctx.memory_high();
  }
  return std::nullopt;
}

// Write to memory.high.tmp (preferred) or memory.high of a given cgroup.
// Return if the cgroup is still valid.
template <typename T>
bool SenpaiCommon<T>::writeMemhigh(
    const CgroupContext& cgroup_ctx,
    int64_t value) {
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

namespace {
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
        // Something very wrong...
        OLOG << "pthread_kill failed";
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

} // namespace
// Call writeMemhigh in a different thread and send signal to interrupt write
// after timeout. Workaround for a kernel "feature" that blocks such write
// indefinitely if reclaim target is too low.
template <typename T>
bool SenpaiCommon<T>::writeMemhighTimeout(
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
template <typename T>
bool SenpaiCommon<T>::resetMemhigh(const CgroupContext& cgroup_ctx) {
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

/** Returns file cache + swappable anon. */
template <typename T>
SystemMaybe<int64_t> SenpaiCommon<T>::getReclaimableBytes(
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
template <typename T>
std::optional<int64_t> SenpaiCommon<T>::getLimitMinBytes(
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
template <typename T>
std::optional<int64_t> SenpaiCommon<T>::getLimitMaxBytes(
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

} // namespace Oomd
