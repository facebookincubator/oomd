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

#include <sys/stat.h>

#include <iomanip>
#include <sstream>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/Stats.h"
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

  return 0;
}

Engine::PluginRet Senpai::run(OomdContext& ctx) {
  std::set<std::string> resolved_cgroups;
  for (const auto& cgroup : cgroups_) {
    auto resolved = Fs::glob(cgroup.absolutePath());
    for (auto&& cg : std::move(resolved)) {
      if (Fs::isDir(cg)) {
        resolved_cgroups.emplace(cg);
      }
    }
  }

  auto new_cgroups = addRemoveTrackedCgroups(resolved_cgroups);
  for (auto& cg : tracked_cgroups_) {
    tick(cg.first, cg.second);
  }

  // new cgroups will be polled after a "tick" has elapsed, so add
  // them to the tracked group at the end here
  tracked_cgroups_.merge(std::move(new_cgroups));
  return Engine::PluginRet::CONTINUE;
}

Senpai::CgroupState::CgroupState(
    uint64_t start_limit,
    std::chrono::microseconds total,
    uint64_t start_ticks,
    const std::string& path)
    : limit{start_limit}, last_total{total}, ticks{start_ticks} {}

namespace {
std::chrono::microseconds getTotal(const std::string& name) {
  // Senpai reads pressure.some to get early notice that a workload
  // may be under resource pressure
  const auto pressure =
      Oomd::Fs::readMempressure(name, Oomd::Fs::PressureType::SOME);

  if (!pressure.total) {
    throw std::runtime_error("Senpai enabled but no total pressure info");
  }

  return pressure.total.value();
}

uint64_t getCurrent(const std::string& name) {
  return static_cast<uint64_t>(Oomd::Fs::readMemcurrent(name));
}

uint64_t getMemMin(const std::string& name) {
  return static_cast<uint64_t>(Oomd::Fs::readMemmin(name));
}
} // namespace
std::map<std::string, Senpai::CgroupState> Senpai::addRemoveTrackedCgroups(
    const std::set<std::string>& resolved_cgroups) {
  std::map<std::string, CgroupState> new_cgroups;
  auto resolvedIt = resolved_cgroups.cbegin();
  auto trackedIt = tracked_cgroups_.begin();
  while (resolvedIt != resolved_cgroups.cend()) {
    if (trackedIt == tracked_cgroups_.end()) {
      // The rest of the resolved cgroups are not tracked, track them
      for (auto it = resolvedIt; it != resolved_cgroups.cend(); ++it) {
        auto state = initializeCgroup(*it);
        new_cgroups.emplace(std::move(*it), std::move(state));
      }
      return new_cgroups;
    }

    if (*resolvedIt < trackedIt->first) {
      // Resolved cgroup not in tracked map, track it
      auto state = initializeCgroup(*resolvedIt);
      new_cgroups.emplace(std::move(*resolvedIt), std::move(state));
      ++resolvedIt;
    } else {
      if (trackedIt->first < *resolvedIt) {
        // tracked cgroup not found, erase it
        trackedIt = tracked_cgroups_.erase(trackedIt);
      } else {
        ++resolvedIt;
        ++trackedIt;
      }
    }
  }
  tracked_cgroups_.erase(trackedIt, tracked_cgroups_.end());
  return new_cgroups;
}

void Senpai::tick(const std::string& name, CgroupState& state) {
  auto limit = static_cast<uint64_t>(readMemhigh(name));
  auto total = getTotal(name);
  auto factor = 0.0;

  if (limit != state.limit) {
    // Something else changed limits on this cgroup or it was
    // recreated in-between ticks - reset the state and return,
    // unfortuantely, the rest of this logic is still racy after this
    // point
    std::ostringstream oss;
    oss << "cgroup " << name << " memory.high " << limit
        << " does not match recorded state " << state.limit
        << ". Resetting cgroup";
    OLOG << oss.str();
    state = initializeCgroup(name);
    return;
  }

  // Make sure memory.high don't go below memory.min
  auto limit_min_bytes = std::max(limit_min_bytes_, getMemMin(name));

  // Adjust cgroup limit by factor
  auto adjust = [&](double factor) {
    state.limit += state.limit * factor;
    state.limit =
        std::max(limit_min_bytes, std::min(limit_max_bytes_, state.limit));
    // Memory high is always a multiple of 4K
    state.limit &= ~0xFFF;
    writeMemhigh(name, state.limit);
    state.ticks = interval_;
    state.cumulative = std::chrono::microseconds{0};
  };
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
    adjust(factor);
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
    adjust(factor);
  }

  std::ostringstream oss;
  oss << "cgroup " << name << std::setprecision(3) << std::fixed << " limitgb "
      << limit / (double)(1 << 30UL) << " totalus " << total.count()
      << " deltaus " << delta.count() << " cumus " << cumulative << " ticks "
      << state.ticks << std::defaultfloat << " adjust " << factor;
  OLOG << oss.str();
}

Senpai::CgroupState Senpai::initializeCgroup(const std::string& path) {
  auto start_limit = getCurrent(path);
  writeMemhigh(path, start_limit);
  return CgroupState(start_limit, getTotal(path), interval_, path);
}

int64_t Senpai::readMemhigh(const std::string& path) {
  if (has_memory_high_tmp_) {
    try {
      return Oomd::Fs::readMemhightmp(path);
    } catch (const Fs::bad_control_file&) {
      has_memory_high_tmp_ = false;
    }
  }
  return Oomd::Fs::readMemhigh(path);
}

void Senpai::writeMemhigh(const std::string& path, int64_t value) {
  if (has_memory_high_tmp_) {
    try {
      Oomd::Fs::writeMemhightmp(path, value, std::chrono::seconds(20));
      return;
    } catch (const Fs::bad_control_file&) {
      has_memory_high_tmp_ = false;
    }
  }
  Oomd::Fs::writeMemhigh(path, value);
}
} // namespace Oomd
