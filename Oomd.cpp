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
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <thread>

#include "oomd/Log.h"
#include "oomd/include/Assert.h"
#include "oomd/util/Fs.h"

namespace Oomd {

Oomd::Oomd(std::unique_ptr<Engine::Engine> engine, int interval)
    : interval_(interval), engine_(std::move(engine)) {}

void Oomd::updateContext(const std::string& cgroup_path, OomdContext& ctx) {
  OomdContext new_ctx;

  // If the targeted cgroup does not have the memory controller enabled,
  // we fail fast and early b/c we need the exposed memory controller
  // knobs to function
  auto controllers = Fs::readControllers(cgroup_path);
  if (!std::any_of(controllers.begin(), controllers.end(), [](std::string& s) {
        return s == "memory";
      })) {
    OLOG << "FATAL: cgroup memory controller not enabled on " << cgroup_path;
    std::abort();
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

    ResourcePressure io_pressure;
    try {
      io_pressure = Fs::readIopressure(child_cgroup);
    } catch (const std::exception& ex) {
      if (!warned_io_pressure_) {
        warned_io_pressure_ = true;
        OLOG << "IO pressure unavailable: " << ex.what();
      }
      // older kernels don't have io.pressure, nan them out
      io_pressure = {std::nanf(""), std::nanf(""), std::nanf("")};
    }

    new_ctx.setCgroupContext(
        dir, {pressures, io_pressure, current, {}, memlow, swap_current});
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
  OomdContext ctx;

  OLOG << "Running oomd";
  OCHECK(engine_);

  while (true) {
    const auto before = std::chrono::steady_clock::now();

    for (const auto& cgroup : engine_->getMonitoredResources()) {
      updateContext(cgroup, ctx);
    }

    // Run all the plugins
    engine_->runOnce(ctx);

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
