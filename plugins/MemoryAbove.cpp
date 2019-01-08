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

static constexpr auto kCgroupFs = "/sys/fs/cgroup/";

namespace Oomd {

REGISTER_PLUGIN(memory_above, MemoryAbove::create);

int MemoryAbove::init(
    Engine::MonitoredResources& resources,
    const Engine::PluginArgs& args) {
  if (args.find("cgroup") != args.end()) {
    auto cgroups = Fs::split(args.at("cgroup"), ',');
    resources.insert(cgroups.begin(), cgroups.end());
    cgroups_.insert(cgroups.begin(), cgroups.end());
    cgroup_fs_ =
        (args.find("cgroup_fs") != args.end() ? args.at("cgroup_fs")
                                              : kCgroupFs);
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  if (args.find("threshold") != args.end()) {
    auto threshold_str = args.at("threshold");
    if (threshold_str.size() > 0) {
      if (threshold_str.compare(threshold_str.size() - 1, 1, "%") == 0) {
        relative_ = true;
      }
    }
    if (relative_) {
      threshold_ = std::stoi(threshold_str.substr(0, threshold_str.size() - 1));
    } else {
      threshold_ = std::stoi(threshold_str);
    }
  } else {
    OLOG << "Argument=threshold not present";
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

  if (args.find("meminfo_location") != args.end()) {
    meminfo_location_ = args.at("meminfo_location");
  }
  // Success
  return 0;
}

Engine::PluginRet MemoryAbove::run(OomdContext& ctx) {
  using std::chrono::steady_clock;
  int64_t current_memory_usage = 0;
  std::string current_cgroup;
  for (const auto& cgroup : cgroups_) {
    try {
      auto cgroup_ctx = ctx.getCgroupContext(cgroup);
      if (debug_) {
        OLOG << "cgroup \"" << cgroup << "\" "
             << "memory.stat (anon)=" << cgroup_ctx.anon_usage;
      }
      if (current_memory_usage < cgroup_ctx.anon_usage) {
        current_memory_usage = cgroup_ctx.anon_usage;
        current_cgroup = cgroup;
      }
    } catch (const std::exception& ex) {
      OLOG << "Failed to get cgroup \"" << cgroup << "\" "
           << "context: " << ex.what();
      continue;
    }
  }
  auto meminfo = meminfo_location_.size() ? Fs::getMeminfo(meminfo_location_)
                                          : Fs::getMeminfo();
  const int64_t memtotal = meminfo["MemTotal"];
  if (debug_) {
    OLOG << "MemTotal=" << memtotal;
  }

  bool threshold_broken{false};
  int64_t threshold_bytes;

  if (relative_) {
    auto percent = current_memory_usage * 100 / memtotal;
    threshold_bytes = (threshold_ * memtotal) / 100;
    if (percent > threshold_) {
      OLOG << "cgroup \"" << current_cgroup << "\" "
           << "memory usage=" << current_memory_usage << " (" << percent
           << "%) "
           << "hit threshold=" << threshold_bytes << " (" << threshold_
           << "%) of MemTotal";
      threshold_broken = true;
    }
  } else {
    threshold_bytes = threshold_ * 1024 * 1024;
    if (current_memory_usage > threshold_bytes) {
      OLOG << "cgroup \"" << current_cgroup << "\" "
           << "memory usage=" << current_memory_usage << " "
           << "hit threshold=" << threshold_bytes;
      threshold_broken = true;
    }
  }

  const auto now = steady_clock::now();

  if (threshold_broken) {
    if (hit_thres_at_ == steady_clock::time_point()) {
      hit_thres_at_ = now;
    }

    const auto diff =
        std::chrono::duration_cast<std::chrono::seconds>(now - hit_thres_at_)
            .count();

    if (diff >= duration_) {
      std::ostringstream oss;
      oss << std::setprecision(2) << std::fixed;
      oss << "cgroup \"" << current_cgroup << "\" "
          << "current memory usage " << current_memory_usage / 1024 / 1024
          << "MB is over the threshold of " << threshold_bytes / 1024 / 1024
          << "MB for " << duration_ << " seconds";
      OLOG << oss.str();

      return Engine::PluginRet::CONTINUE;
    }
  } else {
    hit_thres_at_ = steady_clock::time_point();
  }

  return Engine::PluginRet::STOP;
}

} // namespace Oomd
