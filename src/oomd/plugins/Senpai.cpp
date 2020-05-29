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
  auto resolved_cgroups = ctx.reverseSort(
      cgroups_,
      [](const CgroupContext& cgroup_ctx) { return cgroup_ctx.id(); });
  // Use reverse iterator after reverseSort to make it normal order
  auto resolvedIt = resolved_cgroups.crbegin();
  auto trackedIt = tracked_cgroups_.begin();

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
      // Keep the tracked cgroups if they are still valid after tick
      trackedIt = tick(cgroup_ctx, trackedIt->second)
          ? std::next(trackedIt)
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
    const auto pressure = Oomd::Fs::readMempressureAt(
        cgroup_ctx.fd(), Oomd::Fs::PressureType::SOME);

    if (!pressure.total) {
      throw std::runtime_error("Senpai enabled but no total pressure info");
    }

    return pressure.total.value();
  } catch (const Fs::bad_control_file&) {
  }
  return std::nullopt;
}

// Check if the system support memory.high.tmp cgroup control file. If the given
// cgroup supports it, the system supports it. The result is then stored and
// further calls won't access filesystem. If the cgroup is no longer valid and
// no stored result exists, nullopt is returned.
std::optional<bool> hasMemoryHighTmp(const CgroupContext& cgroup_ctx) {
  static std::optional<bool> has_memory_high_tmp = std::nullopt;
  if (!has_memory_high_tmp.has_value()) {
    if (auto memhightmp = cgroup_ctx.memory_high_tmp()) {
      has_memory_high_tmp = true;
    } else if (auto memhigh = cgroup_ctx.memory_high()) {
      // If memory.high exists but memory.high.tmp doesn't, it's not supported
      has_memory_high_tmp = false;
    }
    // If neither exist, cgroup is invalid. Nothing changed.
  }
  return has_memory_high_tmp;
}

// Read from memory.high.tmp (preferred) or memory.high of a given cgroup.
// Return nullopt if cgroup is no longer valid.
std::optional<int64_t> readMemhigh(const CgroupContext& cgroup_ctx) {
  if (auto has_memory_high_tmp = hasMemoryHighTmp(cgroup_ctx)) {
    return *has_memory_high_tmp ? cgroup_ctx.memory_high_tmp()
                                : cgroup_ctx.memory_high();
  }
  return std::nullopt;
}

// Write to memory.high.tmp (preferred) or memory.high of a given cgroup.
// Return if the cgroup is still valid.
bool writeMemhigh(const CgroupContext& cgroup_ctx, int64_t value) {
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
} // namespace

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
    auto memmin_opt = cgroup_ctx.memory_min();
    if (!memmin_opt) {
      return false;
    }
    // Make sure memory.high don't go below memory.min
    auto limit_min_bytes = std::max(limit_min_bytes_, *memmin_opt);

    // Don't let memory.high.tmp go above memory.high as kernel ignores the
    // latter when the former is set.
    auto limit_max_bytes = limit_max_bytes_;
    if (hasMemoryHighTmp(cgroup_ctx)) {
      if (auto memhigh_opt = cgroup_ctx.memory_high()) {
        limit_max_bytes = std::min(limit_max_bytes, *memhigh_opt);
      } else {
        return false;
      }
    }

    state.limit += state.limit * factor;
    state.limit =
        std::max(limit_min_bytes, std::min(limit_max_bytes, state.limit));
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

  std::ostringstream oss;
  oss << "cgroup " << name << std::setprecision(3) << std::fixed << " limitgb "
      << limit / (double)(1 << 30UL) << " totalus " << total.count()
      << " deltaus " << delta.count() << " cumus " << cumulative << " ticks "
      << state.ticks << std::defaultfloat << " adjust " << factor;
  OLOG << oss.str();
  return true;
}

// Initialize a CgroupState. Return nullopt if cgroup no longer valid.
std::optional<Senpai::CgroupState> Senpai::initializeCgroup(
    const CgroupContext& cgroup_ctx) {
  auto current_opt = cgroup_ctx.current_usage();
  if (!current_opt) {
    return std::nullopt;
  }
  auto total_opt = getPressureTotalSome(cgroup_ctx);
  if (!total_opt) {
    return std::nullopt;
  }
  if (!writeMemhigh(cgroup_ctx, *current_opt)) {
    return std::nullopt;
  }
  return CgroupState(*current_opt, *total_opt, interval_);
}

} // namespace Oomd
