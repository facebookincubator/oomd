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

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <string>
#include <utility>
#include <vector>

#include "oomd/Log.h"
#include "oomd/include/Types.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace Oomd {

template <typename Base>
int KillSwapUsage<Base>::init(
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

  if (args.find("threshold") != args.end()) {
    auto meminfo = args.find("meminfo_location") != args.end()
        ? Fs::getMeminfo(args.at("meminfo_location"))
        : Fs::getMeminfo();

    if (Util::parseSizeOrPercent(
            args.at("threshold"), &threshold_, meminfo.at("SwapTotal")) != 0) {
      OLOG << "Failed to parse threshold" << args.at("threshold");
      return 1;
    }
  }

  if (args.find("post_action_delay") != args.end()) {
    int val = std::stoi(args.at("post_action_delay"));

    if (val < 0) {
      OLOG << "Argument=post_action_delay must be non-negative";
      return 1;
    }

    post_action_delay_ = val;
  }

  if (args.find("dry") != args.end()) {
    const std::string& val = args.at("dry");

    if (val == "true" || val == "True" || val == "1") {
      dry_ = true;
    }
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

template <typename Base>
Engine::PluginRet KillSwapUsage<Base>::run(OomdContext& ctx) {
  bool ret = tryToKillSomething(ctx);

  if (ret) {
    std::this_thread::sleep_for(std::chrono::seconds(post_action_delay_));
    return Engine::PluginRet::STOP;
  } else {
    return Engine::PluginRet::CONTINUE;
  }
}

template <typename Base>
bool KillSwapUsage<Base>::tryToKillSomething(OomdContext& ctx) {
  auto swap_sorted =
      ctx.reverseSort(cgroups_, [](const CgroupContext& cgroup_ctx) {
        return cgroup_ctx.swap_usage().value_or(0);
      });
  OomdContext::dump(swap_sorted, !debug_);

  for (const CgroupContext& cgroup_ctx : swap_sorted) {
    if (cgroup_ctx.swap_usage().value_or(0) < threshold_) {
      break;
    }

    OLOG << "Picked \"" << cgroup_ctx.cgroup().relativePath() << "\" ("
         << cgroup_ctx.current_usage().value_or(0) / 1024 / 1024
         << "MB) based on swap usage at "
         << cgroup_ctx.swap_usage().value_or(0) / 1024 / 1024 << "MB";
    if (auto kill_uuid = Base::tryToKillCgroup(
            cgroup_ctx.cgroup().absolutePath(), true, dry_)) {
      Base::logKill(
          cgroup_ctx.cgroup(),
          cgroup_ctx,
          ctx.getActionContext(),
          *kill_uuid,
          dry_);
      return true;
    }
  }

  return false;
}

} // namespace Oomd
