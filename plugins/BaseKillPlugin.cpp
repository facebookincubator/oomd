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

#include <fnmatch.h>

#include <chrono>
#include <csignal>
#include <fstream>
#include <iomanip>
#include <unordered_set>

#include "oomd/Log.h"
#include "oomd/util/Fs.h"

static auto constexpr kOomdKillXattr = "trusted.oomd_kill";

namespace Oomd {

int BaseKillPlugin::getAndTryToKillPids(
    const std::string& path,
    bool recursive,
    size_t stream_size) {
  int nr_killed = 0;
  auto files = Fs::readDir(path, Fs::EntryType::REG_FILE);

  std::ifstream f(path + "/" + Fs::kProcsFile, std::ios::in);
  if (!f.is_open()) {
    OLOG << "Unable to open " << path;
    return 0;
  }
  while (!f.eof()) {
    std::string s;
    std::vector<int> pids;
    while (std::getline(f, s)) {
      pids.push_back(std::stoi(s));
      if (pids.size() == stream_size) {
        break;
      }
    }
    if (f.bad()) {
      OLOG << "Error while processing file " << path;
    }
    nr_killed += tryToKillPids(pids);
  }

  if (recursive) {
    auto dirs = Fs::readDir(path, Fs::EntryType::DIRECTORY);
    for (const auto& dir : dirs) {
      nr_killed += getAndTryToKillPids(path + "/" + dir, true, stream_size);
    }
  }
  return nr_killed;
}

bool BaseKillPlugin::tryToKillCgroup(
    const std::string& cgroup_path,
    bool recursive,
    bool dry) {
  using namespace std::chrono_literals;

  int last_nr_killed = 0;
  int nr_killed = 0;
  int tries = 10;
  static constexpr size_t stream_size = 20;

  if (dry) {
    OLOG << "OOMD: In dry-run mode; would have tried to kill " << cgroup_path;
    return true;
  }

  OLOG << "Trying to kill " << cgroup_path;

  while (tries--) {
    nr_killed += getAndTryToKillPids(cgroup_path, recursive, stream_size);

    if (nr_killed == last_nr_killed) {
      break;
    }

    // Give it a breather before killing again
    //
    // Don't sleep after the first round of kills b/c the majority of the
    // time the sleep isn't necessary. The system responds fast enough.
    if (last_nr_killed) {
      std::this_thread::sleep_for(1s);
    }

    last_nr_killed = nr_killed;
  }
  reportToXattr(cgroup_path, nr_killed);
  return nr_killed > 0;
}

int BaseKillPlugin::tryToKillPids(const std::vector<int>& pids) {
  std::ostringstream buf;
  int nr_killed = 0;

  for (int pid : pids) {
    auto comm_path = std::string("/proc/") + std::to_string(pid) + "/comm";
    auto comm = Fs::readFileByLine(comm_path);

    if (comm.size()) {
      buf << " " << pid << "(" << comm[0] << ")";
    } else {
      buf << " " << pid;
    }

    if (::kill(static_cast<pid_t>(pid), SIGKILL) == 0) {
      nr_killed++;
    } else {
      buf << "[E" << errno << "]";
    }
  }
  if (buf.tellp()) {
    OLOG << "Killed " << nr_killed << ":" << buf.str();
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
    const CgroupPath& killed_cgroup,
    const CgroupContext& context,
    const ActionContext& action_context,
    bool dry) const {
  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << context.pressure.sec_10 << " " << context.pressure.sec_60 << " "
      << context.pressure.sec_600 << " " << killed_cgroup.relativePath() << " "
      << context.current_usage << " "
      << "ruleset:[" << action_context.ruleset << "] "
      << "detectorgroup:[" << action_context.detectorgroup << "] "
      << "killer:" << (dry ? "(dry)" : "") << getName() << " v2";
  OOMD_KMSG_LOG(oss.str(), "oomd kill");
}

void BaseKillPlugin::removeSiblingCgroups(
    const std::unordered_set<CgroupPath>& ours,
    std::vector<std::pair<CgroupPath, Oomd::CgroupContext>>& vec) {
  vec.erase(
      std::remove_if(
          vec.begin(),
          vec.end(),
          [&](const auto& pair) {
            // Remove this cgroup if does not match any of ours
            bool found = false;
            for (const auto& our : ours) {
              if (our.cgroupFs() == pair.first.cgroupFs() &&
                  !::fnmatch(
                      our.relativePath().c_str(),
                      pair.first.relativePath().c_str(),
                      0)) {
                found = true;
              }
            }
            return !found;
          }),
      vec.end());
}

} // namespace Oomd
