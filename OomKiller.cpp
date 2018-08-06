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

#include "oomd/OomKiller.h"

#include <signal.h>
#include <algorithm>
#include <cmath>
#include <iomanip>
#include <thread>

#include "oomd/Log.h"
#include "oomd/util/Fs.h"

static auto constexpr kKillTypeKillListUsage = "kill_list_maxusage";
static auto constexpr kKillTypeKillListPressure = "kill_list_killpressure";
static auto constexpr kKillTypeSwap = "swap";
static auto constexpr kKillTypeSize = "size";
static auto constexpr kKillTypeGrowth = "growth";
static auto constexpr kOomdKillXattr = "trusted.oomd_kill";

namespace Oomd {

OomKiller::OomKiller(const PluginArgs& args)
    : cgroup_path_(args.cgroup_path),
      kill_list_(args.kill_list),
      tunables_(args.tunables),
      dry_(args.dry) {}

bool OomKiller::tryToKillSomething(OomdContext& ctx) {
  int larger_than = tunables_->get<int>(Tunables::Tunable::LARGER_THAN);
  int growth_above = tunables_->get<int>(Tunables::Tunable::GROWTH_ABOVE);
  auto cur_memcurrent = Fs::readMemcurrent(cgroup_path_);
  auto cur_pressure = Fs::readMempressure(cgroup_path_);

  // If we're dealing with a swap OOM, we try to kill the cgroup using
  // the most swap first.
  if (ctx.getOomContext().type == OomType::SWAP) {
    auto swap_sorted = ctx.reverseSort(
        [](const CgroupContext& cgroup_ctx) { return cgroup_ctx.swap_usage; });
    OomdContext::dumpOomdContext(swap_sorted);
    removeBlacklisted(swap_sorted);

    for (const auto& state_pair : swap_sorted) {
      XLOG(INFO) << "Picked \"" << state_pair.first << "\" ("
                 << state_pair.second.current_usage / 1024 / 1024
                 << "MB) based on swap usage at "
                 << state_pair.second.swap_usage / 1024 / 1024 << "MB";
      if (tryToKillCgroup(state_pair.first)) {
        OOMD_KMSG_LOG(
            state_pair.first,
            kKillTypeSwap,
            state_pair.second,
            ctx.getOomContext(),
            dry_);
        return true;
      }
    }
  }

  auto size_sorted = ctx.reverseSort([](const CgroupContext& cgroup_ctx) {
    return cgroup_ctx.current_usage - cgroup_ctx.memory_low;
  });
  OomdContext::dumpOomdContext(size_sorted);
  removeBlacklisted(size_sorted);
  for (const auto& state_pair : size_sorted) {
    for (const auto& kl_entry : *kill_list_) {
      bool max_usage_kill =
          state_pair.second.current_usage >= kl_entry.max_usage;
      bool pressure_kill = cur_pressure.sec_60 >= kl_entry.kill_pressure;
      if (state_pair.first == kl_entry.service &&
          (max_usage_kill || pressure_kill)) {
        if (tryToKillCgroup(kl_entry.service)) {
          OOMD_KMSG_LOG(
              kl_entry.service,
              (max_usage_kill ? kKillTypeKillListUsage
                              : kKillTypeKillListPressure),
              state_pair.second,
              ctx.getOomContext(),
              dry_);
          return true;
        }
      }
    }
  }

  // failed to use kill list to kill something; time to use some heuristics
  // to reap stuff

  // size heuristic first
  for (const auto& state_pair : size_sorted) {
    if (state_pair.second.current_usage <
        (cur_memcurrent * (static_cast<double>(larger_than) / 100))) {
      XLOG(INFO) << "Skipping size heuristic kill on " << state_pair.first
                 << " b/c not big enough";
      break;
    }

    XLOG(INFO) << "Picked \"" << state_pair.first << "\" ("
               << state_pair.second.current_usage / 1024 / 1024
               << "MB) based on size > " << larger_than << "% of total "
               << cur_memcurrent / 1024 / 1024 << "MB";

    if (tryToKillCgroup(state_pair.first)) {
      OOMD_KMSG_LOG(
          state_pair.first,
          kKillTypeSize,
          state_pair.second,
          ctx.getOomContext(),
          dry_);
      return true;
    }
  }

  // growth heuristic -- pick the top P(growth_above) and sort them
  // by the growth rate (current usage / avg usage) and try to kill
  // the highest one.
  auto growth_sorted = std::move(size_sorted); // save ourselves an allocation
  OomdContext::dumpOomdContext(growth_sorted);
  const int nr = std::ceil(
      growth_sorted.size() * (100 - static_cast<double>(growth_above)) / 100);
  growth_sorted.resize(nr);
  OomdContext::reverseSort(growth_sorted, [](const CgroupContext& cgroup_ctx) {
    return static_cast<double>(cgroup_ctx.current_usage) /
        cgroup_ctx.average_usage;
  });
  removeBlacklisted(growth_sorted);
  for (const auto& state_pair : growth_sorted) {
    std::ostringstream oss;
    oss << std::setprecision(2) << std::fixed;
    oss << "Picked \"" << state_pair.first << "\" ("
        << state_pair.second.current_usage / 1024 / 1024
        << "MB) based on growth rate "
        << static_cast<double>(state_pair.second.current_usage) /
            state_pair.second.average_usage
        << " among P" << growth_above << " largest";
    XLOG(INFO) << oss.str();

    if (tryToKillCgroup(state_pair.first)) {
      OOMD_KMSG_LOG(
          state_pair.first,
          kKillTypeGrowth,
          state_pair.second,
          ctx.getOomContext(),
          dry_);
      return true;
    }
  }

  return false;
}

bool OomKiller::tryToKillCgroup(const std::string& id) {
  using namespace std::chrono_literals;

  int last_nr_killed = 0;
  int nr_killed = 0;
  int tries = 10;

  if (dry_) {
    XLOG(INFO) << "OOMD: In dry-run mode; would have tried to kill " << id;
    return true;
  }

  XLOG(INFO) << "Trying to kill " << id;
  while (tries--) {
    nr_killed += tryToKillPids(Fs::getPids(cgroup_path_ + "/" + id, true));

    if (nr_killed == last_nr_killed) {
      break;
    }
    last_nr_killed = nr_killed;

    // give it a breather before killing again
    std::this_thread::sleep_for(1s);
  }

  reportToXattr(id, nr_killed);

  return nr_killed > 0;
}

int OomKiller::tryToKillPids(const std::vector<int>& pids) {
  int nr_killed = 0;
  for (int pid : pids) {
    if (::kill(static_cast<pid_t>(pid), SIGKILL) == 0) {
      XLOG(INFO) << "Killed pid " << pid;
      nr_killed++;
    } else {
      XLOG(WARNING) << "Failed to kill pid " << pid;
    }
  }
  return nr_killed;
}

void OomKiller::reportToXattr(const std::string& id, int num_procs_killed) {
  auto full_path = cgroup_path_ + "/" + id;
  auto prev_xattr_str = Fs::getxattr(full_path, kOomdKillXattr);
  const int prev_xattr = std::stoi(prev_xattr_str != "" ? prev_xattr_str : "0");
  std::string new_xattr_str = std::to_string(prev_xattr + num_procs_killed);

  if (Fs::setxattr(full_path, kOomdKillXattr, new_xattr_str)) {
    XLOG(INFO) << "Set xattr " << kOomdKillXattr << "=" << new_xattr_str
               << " on " << full_path;
  }
}

bool OomKiller::isBlacklisted(const std::string& target) {
  for (const auto& e : *kill_list_) {
    if (target == e.service && e.max_usage == std::numeric_limits<int>::max()) {
      return true;
    }
  }

  return false;
}

void OomKiller::removeBlacklisted(
    std::vector<std::pair<std::string, CgroupContext>>& list) {
  list.erase(
      std::remove_if(
          list.begin(),
          list.end(),
          [this](const auto& pair) { return this->isBlacklisted(pair.first); }),
      list.end());
}

} // namespace Oomd
