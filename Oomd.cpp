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

#include "oomd/Oomd.h"

#include <signal.h>
#include <unistd.h>

#include <sys/epoll.h>
#include <sys/timerfd.h>
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <sstream>
#include <thread>
#include <unordered_set>

#include "oomd/Log.h"
#include "oomd/include/Assert.h"
#include "oomd/include/Defines.h"
#include "oomd/util/Fs.h"

static constexpr auto kMaxEvents = 10;

namespace {
/*
 * Helper function that resolves a set of wildcarded cgroup paths.
 *
 * @returns a set of resolved cgroup paths
 */
std::unordered_set<Oomd::CgroupPath> resolveCgroupPaths(
    const std::unordered_set<Oomd::CgroupPath>& cgroups) {
  std::unordered_set<Oomd::CgroupPath> ret;

  for (const auto& cgroup : cgroups) {
    // TODO: see what the performance penalty of walking the FS
    // (in resolveWildcardPath) every time is. If it's a big penalty,
    // we could search `absolutePath` for any fnmatch operators and
    // only resolve if we find symbols.
    auto resolved_paths = Oomd::Fs::resolveWildcardPath(cgroup.absolutePath());

    for (const auto& resolved_path : resolved_paths) {
      size_t idx = 0;

      // TODO: make Fs::resolveWildcardPath return a "cleaned" path
      if (resolved_path.find("./", 0) != std::string::npos) {
        idx += 2;
      }
      idx += cgroup.cgroupFs().size() + /* trailing slash */ +1;

      if (idx < resolved_path.size()) {
        std::string cgroup_relative = resolved_path.substr(idx);
        ret.emplace(cgroup.cgroupFs(), std::move(cgroup_relative));
      }
    }
  }

  return ret;
}
} // namespace

namespace Oomd {

Oomd::Oomd(
    std::unique_ptr<Engine::Engine> engine,
    int interval,
    const std::string& cgroup_fs)
    : interval_(interval), cgroup_fs_(cgroup_fs), engine_(std::move(engine)) {
  // Ensure that each monitored cgroup's cgroup fs is the same as the one
  // passed in by the command line
  if (engine_) { // Tests will pass in a nullptr
    // First ensure cgroup fs is uniform
    for (const auto& cgroup : engine_->getMonitoredResources()) {
      if (cgroup.cgroupFs() != cgroup_fs_) {
        resources_.emplace(cgroup_fs_, cgroup.relativePath());
      } else {
        resources_.emplace(cgroup);
      }
    }

    // Then make sure all parent cgroups are pulled in as well. This is
    // necessary so plugins can walk a prepopulated tree.
    for (const auto& cgroup : resources_) {
      if (cgroup.isRoot()) {
        continue;
      }

      CgroupPath parent = cgroup.getParent();
      while (!parent.isRoot()) {
        resources_.emplace(parent);
        parent = parent.getParent();
      }
    }
  }
}

int64_t Oomd::calculateProtectionOverage(
    const CgroupPath& cgroup,
    OomdContext& ctx,
    std::unordered_map<CgroupPath, int64_t>& cache) {
  if (cache.find(cgroup) != cache.end()) {
    return cache.at(cgroup);
  }

  auto node = ctx.getCgroupNode(cgroup);
  OCHECK_EXCEPT(node, std::runtime_error("cgroup missing from OomdContext"));

  auto l_func = [](const CgroupContext& c) -> int64_t {
    return std::min(c.current_usage, std::max(c.memory_min, c.memory_low));
  };

  std::function<int64_t(const std::shared_ptr<CgroupNode>)> p_func =
      [&](const std::shared_ptr<CgroupNode> node) -> int64_t {
    auto parent = node->parent.lock();
    if (parent->path.isRoot()) {
      // We're at a top level cgroup where P(cgrp) == L(cgrp)
      return l_func(node->ctx);
    }

    int64_t l_sum_children = 0;
    for (const auto& child : parent->children) {
      l_sum_children += l_func(child->ctx);
    }

    // If the cgroup isn't using any memory then it's trivially true it's
    // not receiving any protection
    if (l_sum_children == 0) {
      return 0;
    }

    return p_func(parent) * l_func(node->ctx) / l_sum_children;
  };

  int64_t ret = node->ctx.current_usage - p_func(node);
  cache[cgroup] = ret;

  return ret;
}

bool Oomd::updateContextCgroup(const CgroupPath& path, OomdContext& ctx) {
  std::string absolute_cgroup_path = path.absolutePath();

  // Warn once if memory controller is not enabled on target cgroup
  auto controllers = Fs::readControllers(absolute_cgroup_path);
  if (!std::any_of(controllers.begin(), controllers.end(), [](std::string& s) {
        return s == "memory";
      })) {
    if (!warned_mem_controller_.count(absolute_cgroup_path)) {
      OLOG << "WARNING: cgroup memory controller not enabled on "
           << absolute_cgroup_path;
      warned_mem_controller_.emplace(absolute_cgroup_path);
    }

    // Can't extract much info if memory controller isn't enabled
    return false;
  }

  auto current = Fs::readMemcurrent(absolute_cgroup_path);
  auto pressures = Fs::readMempressure(absolute_cgroup_path);
  auto memlow = Fs::readMemlow(absolute_cgroup_path);
  auto memmin = Fs::readMemmin(absolute_cgroup_path);
  auto swap_current = Fs::readSwapCurrent(absolute_cgroup_path);
  auto memory_stats = Fs::getMemstat(absolute_cgroup_path);
  auto anon_usage = memory_stats["anon"];
  ResourcePressure io_pressure;
  try {
    io_pressure = Fs::readIopressure(absolute_cgroup_path);
  } catch (const std::exception& ex) {
    if (!warned_io_pressure_.count(absolute_cgroup_path)) {
      warned_io_pressure_.emplace(absolute_cgroup_path);
      OLOG << "IO pressure unavailable on " << absolute_cgroup_path << ": "
           << ex.what();
    }
    // older kernels don't have io.pressure, nan them out
    io_pressure = {std::nanf(""), std::nanf(""), std::nanf("")};
  }

  ctx.setCgroupContext(
      path,
      {.pressure = pressures,
       .io_pressure = io_pressure,
       .current_usage = current,
       .memory_low = memlow,
       .swap_usage = swap_current,
       .anon_usage = anon_usage,
       .memory_min = memmin});

  return true;
}

void Oomd::updateContext(
    const std::unordered_set<CgroupPath>& cgroups,
    OomdContext& ctx) {
  OomdContext new_ctx;
  // Caching results helps reduce tree walks
  std::unordered_map<CgroupPath, int64_t> protection_overage_cache;

  auto resolved = resolveCgroupPaths(cgroups);
  for (const auto& resolved_cgroup : resolved) {
    // Only care about subtree cgroups, not the cgroup files
    if (!Fs::isDir(resolved_cgroup.absolutePath())) {
      continue;
    }

    updateContextCgroup(resolved_cgroup, new_ctx);
  }

  // Update values that depend on the rest of OomdContext being up to date
  for (auto& key : new_ctx.cgroups()) {
    auto new_cgroup_ctx = new_ctx.getCgroupContext(key); // copy

    // Update running average
    float prev_avg = 0;
    if (ctx.hasCgroupContext(key)) {
      prev_avg = ctx.getCgroupContext(key).average_usage;
    }
    new_cgroup_ctx.average_usage =
        prev_avg * ((average_size_decay_ - 1) / average_size_decay_) +
        (new_cgroup_ctx.current_usage / average_size_decay_);

    // Update protection overage
    new_cgroup_ctx.protection_overage =
        calculateProtectionOverage(key, new_ctx, protection_overage_cache);

    new_ctx.setCgroupContext(key, new_cgroup_ctx);
  }

  // update object state
  ctx = std::move(new_ctx);
}

int Oomd::prepEventLoop(const std::chrono::seconds& interval) {
  char buf[1024];

  timerfd_ = ::timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
  if (timerfd_ < 0) {
    OLOG << "timerfd_create: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  struct itimerspec ts;
  std::memset(&ts, 0, sizeof(ts));
  ts.it_value.tv_sec = interval.count(); // initial expiration
  ts.it_interval.tv_sec = interval.count(); // periodic expiration

  if (::timerfd_settime(timerfd_, 0, &ts, nullptr) < 0) {
    OLOG << "timerfd_settime: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  epollfd_ = ::epoll_create1(EPOLL_CLOEXEC);
  if (epollfd_ < 0) {
    OLOG << "epoll_create1: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  // Add timerfd to epoll set
  struct epoll_event ev;
  std::memset(&ev, 0, sizeof(ev));
  ev.events = EPOLLIN;
  ev.data.fd = timerfd_;
  if (::epoll_ctl(epollfd_, EPOLL_CTL_ADD, timerfd_, &ev) < 0) {
    OLOG << "epoll_ctl: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  return 0;
}

int Oomd::processEventLoop() {
  struct epoll_event events[kMaxEvents];
  char buf[1024];
  int ret;

  int n = ::epoll_wait(epollfd_, events, kMaxEvents, -1);
  if (n < 0) {
    OLOG << "epoll_wait: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  for (int i = 0; i < n; ++i) {
    int fd = events[i].data.fd;
    if (fd == timerfd_) {
      uint64_t expired = 0;
      ret = read(fd, &expired, sizeof(expired));
      if (ret < 0) {
        OLOG << "read: " << ::strerror_r(errno, buf, sizeof(buf));
        return 1;
      }
    } else {
      OLOG << "Unknown fd=" << fd << " in event loop";
      return 1;
    }
  }

  return 0;
}

int Oomd::run() {
  OomdContext ctx;
  int ret;

  ret = prepEventLoop(interval_);
  if (ret) {
    return ret;
  }

  OLOG << "Running oomd";
  if (!engine_) {
    OLOG << "Could not run engine. Your config file is probably invalid\n";
    return EXIT_CANT_RECOVER;
  }

  while (true) {
    try {
      // Sleeps and handles events
      ret = processEventLoop();
      if (ret) {
        return ret;
      }

      updateContext(resources_, ctx);

      // Run all the plugins
      engine_->runOnce(ctx);

    } catch (const Fs::bad_control_file& ex) {
      OLOG << "Caught bad_control_file: " << ex.what();
      return 1;
    } catch (const std::exception& ex) {
      OLOG << "Caught exception: " << ex.what();
      return 1;
    }
  }

  return 0;
}

} // namespace Oomd
