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
#include <stdexcept>
#include <string>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/PluginArgParser.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(memory_above, MemoryAbove::create);

int MemoryAbove::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  // load `meminfo` outside of the PluginArgParser.
  // Because the threshold arg parsing depends on meminfo, to avoid making the
  // parser over complicated for this specific case. We handle it as a special
  // case and make a copy of args with `meminfo` removed.
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

  // erase meminfo_location since we already loaded it
  auto argsCopy = args;
  argsCopy.erase("meminfo_location");

  // by default we're looking for `threshold`.
  // if `threshold_anon` is passed in, use it instead
  auto thresholdArgName = "threshold";
  if (argsCopy.find("threshold_anon") != argsCopy.end()) {
    thresholdArgName = "threshold_anon";
    // remove threshold so that we don't need to handle it in the parser
    argsCopy.erase("threshold");
    is_anon_ = true;
  }

  argParser_.addArgumentCustom(
      "cgroup",
      cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      },
      true);

  argParser_.addArgumentCustom(
      thresholdArgName,
      threshold_,
      [memTotal](const std::string& str) {
        int64_t res = 0;
        if (Util::parseSizeOrPercent(str, &res, memTotal) != 0) {
          throw std::invalid_argument("Failed to parse threshold: " + str);
        }
        return res;
      },
      true);

  argParser_.addArgument("duration", duration_, true);
  argParser_.addArgument("debug", debug_);

  if (!argParser_.parse(argsCopy)) {
    return 1;
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
