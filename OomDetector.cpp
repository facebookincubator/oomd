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

#include "oomd/OomDetector.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iomanip>

#include <folly/logging/xlog.h>

#include "oomd/Log.h"
#include "oomd/util/Fs.h"
#include "oomd/util/ScopeGuard.h"

static auto constexpr kPgscanSwap = "pgscan_kswapd";
static auto constexpr kPgscanDirect = "pgscan_direct";

using std::chrono::steady_clock;

namespace Oomd {

OomDetector::OomDetector(const PluginArgs& args)
    : cgroup_path_(args.cgroup_path),
      kill_list_(args.kill_list),
      tunables_(args.tunables) {
  auto vmstat = Fs::getVmstat();
  last_pgscan_ = vmstat[kPgscanSwap] + vmstat[kPgscanDirect];
}

bool OomDetector::isOOM(OomdContext& ctx) {
  const auto current = Fs::readMemcurrent(cgroup_path_);
  const auto pressure = Fs::readMempressure(cgroup_path_);
  auto meminfo = Fs::getMeminfo();
  const int64_t swapfree = meminfo["SwapFree"];
  const int64_t swaptotal = meminfo["SwapTotal"];
  auto vmstat = Fs::getVmstat();
  const int64_t pgscan = vmstat[kPgscanSwap] + vmstat[kPgscanDirect];
  const int pgscan_window_tick_thres =
      tunables_->get<int>(Tunables::HIGH_THRESHOLD_DURATION) /
      tunables_->get<int>(Tunables::INTERVAL);

  // Update sliding window
  if (pgscan > last_pgscan_) {
    ++pgscan_window_;
  } else {
    pgscan_window_ = 0;
  }

  OOMD_SCOPE_EXIT {
    last_pressure_ = pressure;
    last_pgscan_ = pgscan;
  };

  // log some updated stats
  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << "total=" << current / 1024 / 1024 << "MB pressure=" << pressure.sec_10
      << ":" << pressure.sec_60 << ":" << pressure.sec_600
      << " swapfree=" << swapfree / 1024 / 1024 << "MB/"
      << swaptotal / 1024 / 1024 << "MB pgscan=" << pgscan - last_pgscan_;
  XLOG(INFO) << oss.str();

  // We only declare an generalized OOM if the kernel has scanned for pages
  // the last pgscan_window_tick_thres or more. Each tick is OOMD_INTERVAL
  // seconds long.
  //
  // Swap OOM can happen regardless of pgscans.
  OomContext octx;
  const bool has_pg_scanned = pgscan_window_ >= pgscan_window_tick_thres;

  if (isPressureOOM(pressure, octx) && has_pg_scanned) {
    ctx.setOomContext(octx);
    return true;
  } else if (isHeuristicOOM(current, pressure, octx) && has_pg_scanned) {
    ctx.setOomContext(octx);
    return true;
  } else if (isSwapOOM(swapfree, swaptotal, octx)) {
    ctx.setOomContext(octx);
    return true;
  } else if (
      (isPressureOOM(pressure, octx) ||
       isHeuristicOOM(current, pressure, octx)) &&
      !has_pg_scanned) {
    XLOG(INFO)
        << "Ignoring OOM b/c kernel has not pgscanned but OOM has been detected";
  }

  return false;
}

void OomDetector::postKill(OomdContext& /* unused */) {
  auto vmstat = Fs::getVmstat();
  last_pgscan_ = vmstat[kPgscanSwap] + vmstat[kPgscanDirect];
}

bool OomDetector::isPressureOOM(
    const MemoryPressure& pressure,
    OomContext& octx) const {
  for (const auto& kl_entry : *kill_list_) {
    if (pressure.sec_60 >= kl_entry.kill_pressure) {
      std::ostringstream oss;
      oss << std::setprecision(2) << std::fixed;
      oss << "1m pressure " << pressure.sec_60 << " is over the kill_pressure "
          << kl_entry.kill_pressure << " of " << kl_entry.service;
      XLOG(INFO) << oss.str();
      octx.type = OomType::KILL_LIST;
      return true;
    }
  }

  return false;
}

bool OomDetector::isSwapOOM(
    int64_t swapfree,
    int64_t swaptotal,
    OomContext& octx) {
  const int min_swap_pct = tunables_->get<int>(Tunables::MIN_SWAP_PCT);
  const int64_t swapthres = swaptotal * min_swap_pct / 100;
  if (swaptotal > 0 && swapfree < swapthres) {
    XLOG(INFO) << "SwapFree " << swapfree
               << "MB is smaller than the threshold of " << swapthres
               << "MB, total swap is " << swaptotal << "MB";
    octx.type = OomType::SWAP;
    octx.stat.swap_free = swapfree;
    return true;
  }

  return false;
}

bool OomDetector::isHeuristicOOM(
    int64_t current,
    const MemoryPressure& pressure,
    OomContext& octx) {
  const int threshold = tunables_->get<int>(Tunables::THRESHOLD);
  const int high_threshold = tunables_->get<int>(Tunables::HIGH_THRESHOLD);
  const int high_threshold_duration =
      tunables_->get<int>(Tunables::HIGH_THRESHOLD_DURATION);
  const float fast_fall_ratio =
      tunables_->get<float>(Tunables::FAST_FALL_RATIO);

  // if 10s pressure stays above high_threshold for > high_threshold_duration,
  // we are OOM
  const auto now = steady_clock::now();
  if (pressure.sec_10 > high_threshold) {
    if (high_thres_at_ == steady_clock::time_point()) {
      high_thres_at_ = now;
    }

    const auto diff =
        std::chrono::duration_cast<std::chrono::seconds>(now - high_thres_at_)
            .count();
    if (diff > high_threshold_duration) {
      std::ostringstream oss;
      oss << std::setprecision(2) << std::fixed;
      oss << "10s pressure " << pressure.sec_10 << " >= high_threshold "
          << high_threshold << " for " << diff << "s, total usage is "
          << current / 1024 / 1024 << "MB";
      XLOG(INFO) << oss.str();
      high_thres_at_ = steady_clock::time_point();
      octx.type = OomType::PRESSURE_10;
      octx.stat.pressure_10_duration = diff;
      return true;
    }
  }

  // if pressure is really high or above threshold and trending up
  // (10s value is higher than 1min and 10s isn't falling rapidly),
  // kill something
  if ((pressure.sec_60 >= high_threshold) ||
      ((pressure.sec_60 > threshold) && (pressure.sec_10 >= pressure.sec_60) &&
       (pressure.sec_10 >= last_pressure_.sec_10 * fast_fall_ratio))) {
    std::ostringstream oss;
    oss << std::setprecision(2) << std::fixed;
    oss << "1m pressure " << pressure.sec_60 << " is over the threshold of "
        << threshold << " , total usage is " << current / 1024 / 1024 << "MB";
    XLOG(INFO) << oss.str();
    octx.type = OomType::PRESSURE_60;
    return true;
  }

  return false;
}

} // namespace Oomd
