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

#include "oomd/plugins/BaseKillPlugin.h"

#include <csignal>
#include <iomanip>

#include "oomd/Log.h"
#include "oomd/util/Fs.h"

static auto constexpr kOomdKillXattr = "trusted.oomd_kill";

namespace Oomd {

bool BaseKillPlugin::tryToKillCgroup(
    const std::string& cgroup_path,
    bool recursive,
    bool dry) {
  using namespace std::chrono_literals;

  int last_nr_killed = 0;
  int nr_killed = 0;
  int tries = 10;

  if (dry) {
    OLOG << "OOMD: In dry-run mode; would have tried to kill " << cgroup_path;
    return true;
  }

  OLOG << "Trying to kill " << cgroup_path;
  if (Fs::getPids(cgroup_path, recursive).empty()) {
    OLOG << "No processes to kill";
    return true;
  }

  while (tries--) {
    nr_killed += tryToKillPids(Fs::getPids(cgroup_path, recursive));

    if (nr_killed == last_nr_killed) {
      break;
    }
    last_nr_killed = nr_killed;

    // give it a breather before killing again
    std::this_thread::sleep_for(1s);
  }

  reportToXattr(cgroup_path, nr_killed);

  return nr_killed > 0;
}

int BaseKillPlugin::tryToKillPids(const std::vector<int>& pids) {
  int nr_killed = 0;
  for (int pid : pids) {
    if (::kill(static_cast<pid_t>(pid), SIGKILL) == 0) {
      OLOG << "Killed pid " << pid;
      nr_killed++;
    } else {
      OLOG << "Failed to kill pid " << pid;
    }
  }
  return nr_killed;
}

void BaseKillPlugin::reportToXattr(
    const std::string& cgroup_path,
    int num_procs_killed) {
  auto prev_xattr_str = Fs::getxattr(cgroup_path, kOomdKillXattr);
  const int prev_xattr = std::stoi(prev_xattr_str != "" ? prev_xattr_str : "0");
  std::string new_xattr_str = std::to_string(prev_xattr + num_procs_killed);

  if (Fs::setxattr(cgroup_path, kOomdKillXattr, new_xattr_str)) {
    OLOG << "Set xattr " << kOomdKillXattr << "=" << new_xattr_str << " on "
         << cgroup_path;
  }
}

void BaseKillPlugin::logKill(
    const std::string& killed_cgroup,
    const CgroupContext& context,
    bool dry) const {
  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << context.pressure.sec_10 << " " << context.pressure.sec_60 << " "
      << context.pressure.sec_600 << " " << killed_cgroup << " "
      << context.current_usage << " "
      << "killer:" << (dry ? "(dry)" : "") << getName() << " v2";
  OOMD_KMSG_LOG(oss.str(), "oomd kill");
}

} // namespace Oomd
