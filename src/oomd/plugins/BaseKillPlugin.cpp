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

#include <chrono>
#include <csignal>
#include <fstream>
#include <iomanip>
#include <random>
#include <unordered_set>

#include "oomd/Log.h"
#include "oomd/Stats.h"
#include "oomd/include/CoreStats.h"
#include "oomd/util/Fs.h"

static auto constexpr kOomdKillInitiationXattr = "trusted.oomd_ooms";
static auto constexpr kOomdKillCompletionXattr = "trusted.oomd_kill";
static auto constexpr kOomdKillUuidXattr = "trusted.oomd_kill_uuid";

namespace Oomd {

BaseKillPlugin::BaseKillPlugin() {
  /*
   * Initializes kKillsKey in stats for immediate reporting,
   * rather than waiting for first occurrence
   */
  Oomd::setStat(CoreStats::kKillsKey, 0);
}

int BaseKillPlugin::getAndTryToKillPids(
    const std::string& path,
    bool recursive,
    size_t stream_size) {
  int nr_killed = 0;

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
    nr_killed += tryToKillPids(pids);
    if (f.bad()) {
      OLOG << "Error while processing file " << path;
      // Most likely the cgroup is dead and gone
      break;
    }
  }

  if (recursive) {
    auto de = Fs::readDir(path, Fs::DE_DIR);
    for (const auto& dir : de.dirs) {
      nr_killed += getAndTryToKillPids(path + "/" + dir, true, stream_size);
    }
  }
  return nr_killed;
}

std::optional<BaseKillPlugin::KillUuid> BaseKillPlugin::tryToKillCgroup(
    const std::string& cgroup_path,
    bool recursive,
    bool dry) {
  using namespace std::chrono_literals;

  int last_nr_killed = 0;
  int nr_killed = 0;
  int tries = 10;
  static constexpr size_t stream_size = 20;

  KillUuid kill_uuid = generateKillUuid();

  if (dry) {
    OLOG << "OOMD: In dry-run mode; would have tried to kill " << cgroup_path;
    return kill_uuid;
  }

  OLOG << "Trying to kill " << cgroup_path;

  reportKillUuidToXattr(cgroup_path, kill_uuid);
  reportKillInitiationToXattr(cgroup_path);
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
  reportKillCompletionToXattr(cgroup_path, nr_killed);
  return nr_killed > 0 ? kill_uuid : std::optional<KillUuid>{};
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

BaseKillPlugin::KillUuid BaseKillPlugin::generateKillUuid() const {
  static std::random_device rd;
  static std::mt19937 gen(rd());
  static std::uniform_int_distribution<uint64_t> dis;

  // Combine two 64-bit numbers
  std::stringstream ss;
  ss << std::hex << dis(gen) << dis(gen);

  return ss.str();
}

std::string BaseKillPlugin::getxattr(
    const std::string& path,
    const std::string& attr) {
  return Fs::getxattr(path, attr);
}

bool BaseKillPlugin::setxattr(
    const std::string& path,
    const std::string& attr,
    const std::string& val) {
  return Fs::setxattr(path, attr, val);
}

void BaseKillPlugin::reportKillInitiationToXattr(
    const std::string& cgroup_path) {
  auto prev_xattr_str = getxattr(cgroup_path, kOomdKillInitiationXattr);
  const int prev_xattr = std::stoi(prev_xattr_str != "" ? prev_xattr_str : "0");
  std::string new_xattr_str = std::to_string(prev_xattr + 1);

  if (setxattr(cgroup_path, kOomdKillInitiationXattr, new_xattr_str)) {
    OLOG << "Set xattr " << kOomdKillInitiationXattr << "=" << new_xattr_str
         << " on " << cgroup_path;
  }
}

void BaseKillPlugin::reportKillCompletionToXattr(
    const std::string& cgroup_path,
    int num_procs_killed) {
  auto prev_xattr_str = getxattr(cgroup_path, kOomdKillCompletionXattr);
  const int prev_xattr = std::stoi(prev_xattr_str != "" ? prev_xattr_str : "0");
  std::string new_xattr_str = std::to_string(prev_xattr + num_procs_killed);

  if (setxattr(cgroup_path, kOomdKillCompletionXattr, new_xattr_str)) {
    OLOG << "Set xattr " << kOomdKillCompletionXattr << "=" << new_xattr_str
         << " on " << cgroup_path;
  }
}

void BaseKillPlugin::reportKillUuidToXattr(
    const std::string& cgroup_path,
    const std::string& kill_uuid) {
  if (setxattr(cgroup_path, kOomdKillUuidXattr, kill_uuid)) {
    OLOG << "Set xattr " << kOomdKillUuidXattr << "=" << kill_uuid << " on "
         << cgroup_path;
  }
}

void BaseKillPlugin::logKill(
    const CgroupPath& killed_cgroup,
    const CgroupContext& context,
    const ActionContext& action_context,
    const std::string& kill_uuid,
    bool dry) const {
  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << context.pressure.sec_10 << " " << context.pressure.sec_60 << " "
      << context.pressure.sec_600 << " " << killed_cgroup.relativePath() << " "
      << context.current_usage << " "
      << "ruleset:[" << action_context.ruleset << "] "
      << "detectorgroup:[" << action_context.detectorgroup << "] "
      << "killer:" << (dry ? "(dry)" : "") << getName() << " v2";
  Oomd::incrementStat(CoreStats::kKillsKey, 1);
  OOMD_KMSG_LOG(oss.str(), "oomd kill");

  dumpKillInfo(killed_cgroup, context, action_context, kill_uuid, dry);
}

} // namespace Oomd
