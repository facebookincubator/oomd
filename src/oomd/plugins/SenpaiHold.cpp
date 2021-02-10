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

#include "oomd/plugins/SenpaiHold.h"

#include <cmath>
#include <iomanip>
#include <iostream>

#include <fcntl.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>

#include "oomd/PluginRegistry.h"
#include "oomd/include/Assert.h"
#include "oomd/util/ScopeGuard.h"

namespace Oomd {
REGISTER_PLUGIN(senpai_hold, SenpaiHold::create);

namespace {
// This is the main epoll thread loop. It blocks on epoll_wait
// forever until either a pressure trigger fires (in which case we
// write "max" to the memory.high) or the eventfd triggers (in
// which case the thread exits)
void runEpollThread(std::shared_ptr<EpollThreadState> state) {
  struct epoll_event event;
  while (true) {
    auto ret = ::epoll_wait(state->epoll_fd.fd(), &event, 1, -1);
    if (ret == -1) {
      if (errno == EINTR) {
        continue;
      }

      // Some really unexpected error would have to
      // happen to hit this case, just abort
      OLOG << "Unexpected error " << errno << " from epoll_wait()";
      OCHECK(false);
    }

    auto cgroup_id = event.data.u64;
    if (!cgroup_id) {
      // Woken up via eventfd - means we are exiting
      break;
    } else if (event.events & EPOLLPRI) {
      const std::lock_guard<std::mutex> lock(state->mutex);
      auto it = state->cgroups.find(cgroup_id);
      if (it == state->cgroups.end()) {
        // It's possible destruction of the cgroup (and associated
        // state) raced with a notification we just received - just
        // ignore it in that case.
        continue;
      }
      auto& cgroup_state = it->second;
      std::string val;
      if (cgroup_state.use_memory_high_tmp) {
        val = "max 20000000";
      } else {
        val = "max";
      }
      // Ignore return value, not much we can do
      // other than try to write here
      Util::writeFull(cgroup_state.mem_high_fd.fd(), val.c_str(), val.size());
    }
  }
}
} // namespace

int SenpaiHold::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  if (SenpaiCommon<SenpaiHold>::init(args, context)) {
    return 1;
  }

  if (args.find("pressure_target") != args.end()) {
    pressure_target_ = std::stod(args.at("pressure_target"));
  }

  if (args.find("blowout_threshold") != args.end()) {
    blowout_threshold_ = std::stod(args.at("blowout_threshold"));
  }

  if (args.find("max_action_interval_ms") != args.end()) {
    max_action_interval_ms_ = std::chrono::milliseconds(
        std::stoull(args.at("max_action_interval_ms")));
  }

  if (args.find("max_backoff") != args.end()) {
    max_backoff_ = std::stod(args.at("max_backoff"));
  }

  if (args.find("coeff_backoff") != args.end()) {
    coeff_backoff_ = std::stod(args.at("coeff_backoff"));
  }

  // We must calculate the exponential function of the form f(x) = Ae^(kx) that
  // intersects (0, blowout_threshold_) and (max_action_interval_ms_, epsilon).
  // Given two points (x0, y0) and (x1, y1) the exponent k is calculated as:
  // k = ln(y0/y1) / (x0 - x1)
  // Because f(0) = blowout_threshold_, A = blowout_threshold_
  double epsilon = 0.01;
  error_threshold_exponent_ = std::log(blowout_threshold_ / epsilon) /
      (-1 * max_action_interval_ms_.count());

  // Setup epoll and communication with epoll thread
  auto ret = ::eventfd(0, EFD_NONBLOCK);
  if (ret == -1) {
    return 1;
  }

  epoll_eventfd_ = Fs::Fd(ret);

  ret = ::epoll_create(1);
  if (ret == -1) {
    return 1;
  }
  auto epoll_fd = Fs::Fd(ret);
  struct epoll_event event = {
      .events = EPOLLIN, .data = {.u64 = 0}, // 0 here indicates the eventfd
  };
  ret = ::epoll_ctl(epoll_fd.fd(), EPOLL_CTL_ADD, epoll_eventfd_.fd(), &event);
  if (ret) {
    return 1;
  }

  epoll_thread_state_ = std::make_shared<EpollThreadState>(std::move(epoll_fd));
  epoll_thread_ = std::thread([state = epoll_thread_state_]() mutable {
    runEpollThread(std::move(state));
  });

  return 0;
}

SenpaiHold::~SenpaiHold() {
  if (epoll_thread_.joinable()) {
    uint64_t val = 1;
    auto ret = Util::writeFull(epoll_eventfd_.fd(), (char*)&val, sizeof(val));
    OCHECK(ret != -1);
    epoll_thread_.join();
  }
}

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

CgroupState<SenpaiHold>::~CgroupState() noexcept {
  if (epoll_thread_state) {
    const std::lock_guard<std::mutex> lock(epoll_thread_state->mutex);
    // We don't need to remove the fd from the epoll. Closing the
    // file descriptor should do that automatically
    epoll_thread_state->cgroups.erase(cgroup_id);
  }
}

SystemMaybe<Unit> SenpaiHold::initializeCgroup(
    const CgroupContext& cgroup_ctx,
    CgroupState& state) {
  auto pres_fd = Fs::Fd::openat(
      cgroup_ctx.fd(), Fs::kMemPressureFile, /* read_only =  */ false);
  if (!pres_fd) {
    return SYSTEM_ERROR(pres_fd);
  }

  auto has_memory_high_tmp_opt = hasMemoryHighTmp(cgroup_ctx);
  if (!has_memory_high_tmp_opt) {
    return SYSTEM_ERROR(ENOENT);
  }

  auto high_fd = Fs::Fd::openat(
      cgroup_ctx.fd(),
      *has_memory_high_tmp_opt ? Fs::kMemHighTmpFile : Fs::kMemHighFile,
      /* read_only = */ false);
  if (!high_fd) {
    return SYSTEM_ERROR(high_fd);
  }

  state.epoll_thread_state = epoll_thread_state_;
  auto id = cgroup_ctx.id();
  if (!id) {
    return SYSTEM_ERROR(EINVAL);
  }
  state.cgroup_id = *id;
  auto pressure_fd = pres_fd->fd();
  {
    const std::lock_guard<std::mutex> lock(state.epoll_thread_state->mutex);
    state.epoll_thread_state->cgroups.emplace(
        std::piecewise_construct,
        std::forward_as_tuple(state.cgroup_id),
        std::forward_as_tuple(
            std::move(*pres_fd),
            std::move(*high_fd),
            *has_memory_high_tmp_opt));
  }
  std::chrono::microseconds min_window = std::chrono::milliseconds(500);
  uint64_t trigger_value =
      pressure_target_ * blowout_threshold_ * min_window.count();
  std::string msg = "some " + std::to_string(trigger_value) + " " +
      std::to_string(min_window.count());
  auto ret = Util::writeFull(pressure_fd, msg.c_str(), msg.size());
  if (ret == -1) {
    return SYSTEM_ERROR(errno);
  }

  struct epoll_event event = {
      .events = EPOLLPRI,
      .data = {.u64 = state.cgroup_id},
  };
  // We grab the lock on the cgroup state here to ensure that we can't
  // race until we've finished initializing it.
  const std::lock_guard<std::mutex> lock(state.epoll_thread_state->mutex);
  ret = ::epoll_ctl(
      state.epoll_thread_state->epoll_fd.fd(),
      EPOLL_CTL_ADD,
      pressure_fd,
      &event);
  if (ret) {
    return SYSTEM_ERROR(errno);
  }

  return resetLimit(cgroup_ctx, state);
}

SystemMaybe<Unit> SenpaiHold::resetLimit(
    const CgroupContext& cgroup_ctx,
    CgroupState& state) {
  auto current_opt = cgroup_ctx.current_usage();
  if (!current_opt) {
    return SYSTEM_ERROR(ENOENT);
  }

  auto limit_min_bytes_opt = getLimitMinBytes(cgroup_ctx);
  if (!limit_min_bytes_opt) {
    return SYSTEM_ERROR(ENOENT);
  }

  auto start_limit = *current_opt;
  auto now = std::chrono::steady_clock::now();

  if (!writeMemhigh(cgroup_ctx, start_limit)) {
    return SYSTEM_ERROR(ENOENT);
  }
  auto total_opt = getPressureTotalSome(cgroup_ctx);
  if (!total_opt) {
    return SYSTEM_ERROR(ENOENT);
  }
  state.limit = start_limit;
  state.last_total = *total_opt;
  state.last_action_time = now;
  return noSystemError();
}

// Update state of a cgroup. Return if the cgroup is still valid.
bool SenpaiHold::tick(const CgroupContext& cgroup_ctx, CgroupState& state) {
  const std::lock_guard<std::mutex> lock(state.epoll_thread_state->mutex);
  auto name = cgroup_ctx.cgroup().absolutePath();
  auto limit_opt = readMemhigh(cgroup_ctx);
  if (!limit_opt) {
    return false;
  }

  if (*limit_opt != state.limit) {
    // This can happen if the trigger fired or if the cgroup was
    // recreated.
    if (resetLimit(cgroup_ctx, state)) {
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
    state.last_action_time = std::chrono::steady_clock::now();
    state.cumulative = std::chrono::microseconds{0};
    return writeMemhigh(cgroup_ctx, state.limit);
  };
  auto now = std::chrono::steady_clock::now();
  auto total_opt = getPressureTotalSome(cgroup_ctx);
  if (!total_opt) {
    return false;
  }
  auto total = *total_opt;
  auto delta = total - state.last_total;
  state.last_total = total;
  state.cumulative += delta;
  auto cumulative = state.cumulative.count();

  using std::chrono::duration_cast;
  using std::chrono::microseconds;
  using std::chrono::milliseconds;
  double cumulative_us = duration_cast<microseconds>(state.cumulative).count();
  double elapsed_us =
      duration_cast<microseconds>(now - state.last_action_time).count();
  auto pressure_observed = cumulative_us / elapsed_us;
  // This is a very simple error function. We could consider adding
  // asymmetry to it (e.g. seeing pressure too-high is penalized more
  // than too-low) or exponentiation (to treat 1.0 vs 2.0 differently
  // than 10.0 vs 11.0).
  auto error =
      std::abs(pressure_observed - pressure_target_) / pressure_target_;

  auto elapsed_ms =
      duration_cast<milliseconds>(now - state.last_action_time).count();
  auto threshold =
      blowout_threshold_ * std::exp(error_threshold_exponent_ * elapsed_ms);
  if (error > threshold) {
    if (pressure_observed > pressure_target_) {
      // Excessive pressure, back off. The rate scales exponentially
      // with pressure deviation. The coefficient defines how sensitive
      // we are to fluctuations around the target pressure: when the
      // coefficient is 9, the adjustment curve reaches the backoff
      // limit when observed pressure is ten times the target pressure.
      auto factor = error / coeff_backoff_;
      factor *= factor;
      factor = std::min(factor * max_backoff_, max_backoff_);
      if (!adjust(factor)) {
        return false;
      }

      std::ostringstream oss;
      oss << "cgroup " << name << std::setprecision(3) << std::fixed
          << " limitgb " << *limit_opt / (double)(1 << 30UL) << " totalus "
          << total.count() << " deltaus " << delta.count() << " cumus "
          << cumulative << " elapsed ms " << (uint64_t)elapsed_ms
          << std::defaultfloat << " adjust " << factor;
      OLOG << oss.str();
    } else {
      // Pressure too low, tighten the limit. Like when backing off, the
      // adjustment becomes exponentially more aggressive as observed
      // pressure falls below the target pressure.
      auto factor = error;
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

      std::ostringstream oss;
      oss << "cgroup " << name << std::setprecision(3) << std::fixed
          << " limitgb " << *limit_opt / (double)(1 << 30UL) << " totalus "
          << total.count() << " deltaus " << delta.count() << " cumus "
          << cumulative << " elapsed ms " << (uint64_t)elapsed_ms
          << std::defaultfloat << " adjust " << factor;
      OLOG << oss.str();
    }
  }
  return true;
}
} // namespace Oomd
