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
#include <iomanip>
#include <sstream>
#include <thread>
#include <unordered_set>

#include "oomd/Log.h"
#include "oomd/include/Assert.h"
#include "oomd/util/Fs.h"

static constexpr auto kCgroupFsRoot = "/sys/fs/cgroup";

namespace {
/*
 * Helper function that resolves a set of wildcarded cgroup paths.
 *
 * @returns a set of resolved absolute paths
 */
std::unordered_set<std::string> resolveCgroupPaths(
    const std::string& cgroup_root_dir,
    const std::unordered_set<std::string>& parent_cgroups) {
  std::unordered_set<std::string> ret;

  for (const auto& parent_cgroup : parent_cgroups) {
    std::string abs_unresolved_path = cgroup_root_dir + "/" + parent_cgroup;
    // TODO: see what the performance penalty of walking the FS
    // (in resolveWildcardPath) every time is. If it's a big penalty,
    // we could search `abs_unresolved_path` for any fnmatch operators and
    // only resolve if we find symbols.
    auto resolved_paths = Oomd::Fs::resolveWildcardPath(abs_unresolved_path);
    ret.insert(resolved_paths.begin(), resolved_paths.end());
  }

  return ret;
}
} // namespace

namespace Oomd {

Oomd::Oomd(std::unique_ptr<Engine::Engine> engine, int interval)
    : interval_(interval), engine_(std::move(engine)) {}

bool Oomd::updateContextCgroup(
    const std::string& relative_cgroup_path,
    const std::string& absolute_cgroup_path,
    OomdContext& ctx) {
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
  auto swap_current = Fs::readSwapCurrent(absolute_cgroup_path);

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
      relative_cgroup_path,
      {pressures, io_pressure, current, {}, memlow, swap_current});

  return true;
}

void Oomd::updateContext(
    const std::string& cgroup_root_dir,
    const std::unordered_set<std::string>& cgroups,
    OomdContext& ctx) {
  OomdContext new_ctx;

  auto resolved = resolveCgroupPaths(cgroup_root_dir, cgroups);
  for (const auto& resolved_cgroup : resolved) {
    // Only care about subtree cgroups, not the cgroup files
    if (!Fs::isDir(resolved_cgroup)) {
      continue;
    }

    std::string relative_cgroup_path = resolved_cgroup;
    Fs::removePrefix(relative_cgroup_path, cgroup_root_dir + "/");
    updateContextCgroup(relative_cgroup_path, resolved_cgroup, new_ctx);
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

    updateContext(kCgroupFsRoot, engine_->getMonitoredResources(), ctx);

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
