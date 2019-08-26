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

static constexpr auto kCgroupFs = "/sys/fs/cgroup/";

namespace Oomd {

REGISTER_PLUGIN(memory_above, MemoryAbove::create);

namespace {
int64_t parse_threshold(const std::string& str, int64_t memtotal) {
  if (str.size() > 0 && str.at(str.size() - 1) == '%') {
    double pct = std::stod(str.substr(0, str.size() - 1));
    if (pct < 0 || pct > 100) {
      OLOG << str << " is an illegal percentage value";
      return -1;
    }
    return memtotal * pct / 100;
  } else {
    int64_t v;
    size_t end_pos;

    // compat - a bare number is interpreted as megabytes
    v = std::stoll(str, &end_pos);
    if (end_pos == str.length()) {
      return v << 20;
    }
    if (Util::parseSize(str, &v) == 0) {
      return v;
    }
    return -1;
  }
}
} // namespace

int MemoryAbove::init(
    Engine::MonitoredResources& resources,
    const Engine::PluginArgs& args) {
  if (args.find("cgroup") != args.end()) {
    auto cgroup_fs =
        (args.find("cgroup_fs") != args.end() ? args.at("cgroup_fs")
                                              : kCgroupFs);

    auto cgroups = Util::split(args.at("cgroup"), ',');
    for (const auto& c : cgroups) {
      resources.emplace(cgroup_fs, c);
      cgroups_.emplace(cgroup_fs, c);
    }
  } else {
    OLOG << "Argument=cgroup not present";
    return 1;
  }

  auto meminfo = args.find("meminfo_location") != args.end()
      ? Fs::getMeminfo(args.at("meminfo_location"))
      : Fs::getMeminfo();

  if (args.find("threshold_anon") != args.end()) {
    threshold_ =
        parse_threshold(args.at("threshold_anon"), meminfo["MemTotal"]);
    if (threshold_ < 0) {
      return 1;
    }
    is_anon_ = true;
  } else if (args.find("threshold") != args.end()) {
    threshold_ = parse_threshold(args.at("threshold"), meminfo["MemTotal"]);
    if (threshold_ < 0) {
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
  for (const auto& cgroup : cgroups_) {
    try {
      auto cgroup_ctx = ctx.getCgroupContext(cgroup);
      if (debug_) {
        OLOG << "cgroup \"" << cgroup.relativePath() << "\" "
             << "memory.current=" << cgroup_ctx.current_usage
             << "memory.stat (anon)=" << cgroup_ctx.anon_usage;
      }
      auto usage = is_anon_ ? cgroup_ctx.anon_usage : cgroup_ctx.current_usage;
      if (current_memory_usage < usage) {
        current_memory_usage = usage;
        current_cgroup = cgroup.relativePath();
      }
    } catch (const std::exception& ex) {
      OLOG << "Failed to get cgroup \"" << cgroup.relativePath() << "\" "
           << "context: " << ex.what();
      continue;
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
