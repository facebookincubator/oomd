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

#include "oomd/plugins/MemoryAbove.h"

#include <iomanip>
#include <string>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(memory_above, MemoryAbove::create);

int MemoryAbove::init(
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

  auto meminfoMaybe = args.find("meminfo_location") != args.end()
      ? Fs::getMeminfo(args.at("meminfo_location"))
      : Fs::getMeminfo();

  if (!meminfoMaybe) {
    OLOG << "Could not read meminfo " << meminfoMaybe.error().what();
    return 1;
  } else if (!meminfoMaybe->count("MemTotal")) {
    OLOG << "meminfo does not contain MemTotal";
    return 1;
  }
  auto memTotal = (*meminfoMaybe)["MemTotal"];

  if (args.find("threshold_anon") != args.end()) {
    if (Util::parseSizeOrPercent(
            args.at("threshold_anon"), &threshold_, memTotal) != 0) {
      OLOG << "Failed to parse threshold_anon=" << args.at("threshold_anon");
      return 1;
    }
    is_anon_ = true;
  } else if (args.find("threshold") != args.end()) {
    if (Util::parseSizeOrPercent(args.at("threshold"), &threshold_, memTotal) <
        0) {
      OLOG << "Failed to parse threshold=" << args.at("threshold");
      return 1;
    }
  } else {
    OLOG << "Argument=[anon_]threshold not present";
    return 1;
  }

  if (args.find("duration") != args.end()) {
    duration_ = std::stoi(args.at("duration"));
  } else {
    OLOG << "Argument=duration not present";
    return 1;
  }

  if (args.find("debug") != args.end()) {
    const std::string& val = args.at("debug");

    if (val == "true" || val == "True" || val == "1") {
      debug_ = true;
    }
  }

  // Success
  return 0;
}

Engine::PluginRet MemoryAbove::run(OomdContext& ctx) {
  using std::chrono::steady_clock;
  int64_t current_memory_usage = 0;
  std::string current_cgroup;
  for (const CgroupContext& cgroup_ctx : ctx.addToCacheAndGet(cgroups_)) {
    if (debug_) {
      OLOG << "cgroup \"" << cgroup_ctx.cgroup().relativePath() << "\" "
           << "memory.current=" << cgroup_ctx.current_usage().value_or(0)
           << "memory.stat (anon)=" << cgroup_ctx.anon_usage().value_or(0);
    }
    auto usage = is_anon_ ? cgroup_ctx.anon_usage().value_or(0)
                          : cgroup_ctx.current_usage().value_or(0);
    if (current_memory_usage < usage) {
      current_memory_usage = usage;
      current_cgroup = cgroup_ctx.cgroup().relativePath();
    }
  }

  const auto now = steady_clock::now();

  if (current_memory_usage > threshold_) {
    // Logging this on every positive match is too verbose.  Daniel is
    // fixing it properly but let's shut it up for the time being.
    if (debug_) {
      OLOG << "cgroup \"" << current_cgroup << "\" "
           << (is_anon_ ? "anon usage=" : "memory usage=")
           << current_memory_usage << " hit threshold=" << threshold_;
    }

    if (hit_thres_at_ == steady_clock::time_point()) {
      hit_thres_at_ = now;
    }

    const auto diff =
        std::chrono::duration_cast<std::chrono::seconds>(now - hit_thres_at_)
            .count();

    if (diff >= duration_) {
      // Logging this on every positive match is too verbose.  Daniel is
      // fixing it properly but let's shut it up for the time being.
      if (debug_) {
        std::ostringstream oss;
        oss << std::setprecision(2) << std::fixed;
        oss << "cgroup \"" << current_cgroup << "\" "
            << "current memory usage " << current_memory_usage / 1024 / 1024
            << "MB is over the threshold of " << threshold_ / 1024 / 1024
            << "MB for " << duration_ << " seconds";
        OLOG << oss.str();
      }

      return Engine::PluginRet::CONTINUE;
    }
  } else {
    hit_thres_at_ = steady_clock::time_point();
  }

  return Engine::PluginRet::STOP;
}

} // namespace Oomd
