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

#include <algorithm>
#include <thread>

#include <folly/logging/xlog.h>

#include "oomd/Log.h"
#include "oomd/util/Fs.h"

namespace {
std::atomic<bool> need_tunables_reload{false};

void reloadHandler(int /* unused */) {
  need_tunables_reload = true;
}
} // namespace

namespace Oomd {

bool Oomd::prepareRun() {
  if (cgroups_.size() == 0) {
    XLOG(ERR) << "OOM detection or OOM killer classes not injected";
    return false;
  }

  if (!tunables_) {
    XLOG(ERR) << "Tunables not injected";
    return false;
  }

  if (!registerHandlers()) {
    XLOG(ERR) << "Unable to register signal handlers";
    return false;
  }

  updateTunables();
  return true;
}

bool Oomd::registerHandlers() const {
  struct sigaction act;
  std::memset(&act, 0, sizeof(act));

  act.sa_handler = reloadHandler;

  if (::sigaction(SIGUSR1, &act, nullptr) < 0) {
    perror("sigaction");
    return false;
  }

  return true;
}

void Oomd::updateTunables() {
  int raw_interval = tunables_->get<int>(Tunables::Tunable::INTERVAL);
  interval_ = std::chrono::seconds(raw_interval);
  post_kill_delay_ = std::chrono::seconds(
      tunables_->get<int>(Tunables::Tunable::POST_KILL_DELAY));
  verbose_ticks_ =
      tunables_->get<int>(Tunables::Tunable::VERBOSE_INTERVAL) / raw_interval;
  average_size_decay_ =
      tunables_->get<double>(Tunables::Tunable::AVERAGE_SIZE_DECAY);
}

void Oomd::updateContext(const std::string& cgroup_path, OomdContext& ctx) {
  OomdContext new_ctx;

  // If the targeted cgroup does not have the memory controller enabled,
  // we fail fast and early b/c we need the exposed memory controller
  // knobs to function
  auto controllers = Fs::readControllers(cgroup_path);
  if (!std::any_of(controllers.begin(), controllers.end(), [](std::string& s) {
        return s == "memory";
      })) {
    XLOG(FATAL) << "cgroup memory controller not enabled on " << cgroup_path;
  }

  // grab and update memory stats for cgroups which we are assigned
  // to watch
  auto dirs = Fs::readDir(cgroup_path, Fs::EntryType::DIRECTORY);
  for (auto& dir : dirs) {
    // update our new map with new state information
    auto child_cgroup = cgroup_path + "/" + dir;
    auto current = Fs::readMemcurrent(child_cgroup);
    auto pressures = Fs::readMempressure(child_cgroup);
    auto memlow = Fs::readMemlow(child_cgroup);
    auto swap_current = Fs::readSwapCurrent(child_cgroup);
    new_ctx.setCgroupContext(
        dir, {pressures, current, {}, memlow, swap_current});
  }

  // calculate running averages
  for (auto& key : new_ctx.cgroups()) {
    float prev_avg = 0;
    if (ctx.hasCgroupContext(key)) {
      prev_avg = ctx.getCgroupContext(key).average_usage;
    }

    auto new_cgroup_ctx = new_ctx.getCgroupContext(key); // copy
    new_cgroup_ctx.average_usage =
        prev_avg * ((average_size_decay_ - 1) / average_size_decay_) +
        (new_cgroup_ctx.current_usage / average_size_decay_);
    new_ctx.setCgroupContext(key, new_cgroup_ctx);
  }

  // update object state
  ctx = std::move(new_ctx);
}

int Oomd::run() {
  if (!prepareRun()) {
    return 1;
  }

  uint64_t ticks = 0;
  uint64_t detector_ticks = 0;
  uint64_t killer_ticks = 0;

  XLOG(INFO) << "Running oomd";
  while (true) {
    if (need_tunables_reload) {
      tunables_->loadOverrides();
      tunables_->dump();
      need_tunables_reload = false;
    }
    updateTunables();

    const auto before = std::chrono::steady_clock::now();

    if (verbose_ && ++ticks % verbose_ticks_ == 0) {
      std::ostringstream oss;
      oss << "detectorticks=" << detector_ticks
          << " killticks=" << killer_ticks;
      OOMD_LOG(oss.str(), "oomd heartbeat");
    }

    for (auto& cgroup : cgroups_) {
      updateContext(cgroup->detector->getCgroupPath(), cgroup->context);

      ++detector_ticks;
      if (cgroup->detector->isOOM(cgroup->context)) {
        ++killer_ticks;
        if (cgroup->killer->tryToKillSomething(cgroup->context)) {
          std::this_thread::sleep_for(post_kill_delay_ / 2);
          cgroup->detector->postKill(cgroup->context);
          std::this_thread::sleep_for(post_kill_delay_ / 2);

          // Only kill 1 process system-wide per Tunable::INTERVAL.
          // We do this because oomd examines the system as a whole,
          // as opposed to an isolated per-cgroup manner
          break;
        }
      }
    }

    // Rotate cgroups_ so we have round robin for fairness
    std::rotate(cgroups_.begin(), cgroups_.begin() + 1, cgroups_.end());

    // We may have slept already, so recalculate
    const auto after = std::chrono::steady_clock::now();
    auto to_sleep = interval_ - (after - before);
    if (to_sleep < std::chrono::seconds(0)) {
      to_sleep = std::chrono::seconds(0);
    }

    std::this_thread::sleep_for(to_sleep);
  }

  return 0;
}

} // namespace Oomd
