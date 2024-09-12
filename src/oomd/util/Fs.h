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

#pragma once

#include <dirent.h>
#include <sys/types.h>
#include <exception>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "oomd/include/Types.h"
#include "oomd/util/SystemMaybe.h"

namespace Oomd {

class Fs {
 public:
  static constexpr auto kControllersFile = "cgroup.controllers";
  static constexpr auto kSubtreeControlFile = "cgroup.subtree_control";
  static constexpr auto kProcsFile = "cgroup.procs";
  static constexpr auto kEventsFile = "cgroup.events";
  static constexpr auto kCgroupFreeze = "cgroup.freeze";
  static constexpr auto kMemCurrentFile = "memory.current";
  static constexpr auto kMemPressureFile = "memory.pressure";
  static constexpr auto kMemLowFile = "memory.low";
  static constexpr auto kMemHighFile = "memory.high";
  static constexpr auto kMemMaxFile = "memory.max";
  static constexpr auto kMemHighTmpFile = "memory.high.tmp";
  static constexpr auto kMemReclaimFile = "memory.reclaim";
  static constexpr auto kMemMinFile = "memory.min";
  static constexpr auto kMemStatFile = "memory.stat";
  static constexpr auto kCgroupStatFile = "cgroup.stat";
  static constexpr auto kMemSwapCurrentFile = "memory.swap.current";
  static constexpr auto kMemSwapMaxFile = "memory.swap.max";
  static constexpr auto kMemOomGroupFile = "memory.oom.group";
  static constexpr auto kIoPressureFile = "io.pressure";
  static constexpr auto kIoStatFile = "io.stat";
  static constexpr auto kDeviceTypeDir = "queue";
  static constexpr auto kDeviceTypeFile = "rotational";
  static constexpr auto kOomdSystemPreferXAttr = "trusted.oomd_prefer";
  static constexpr auto kOomdUserPreferXAttr = "user.oomd_prefer";
  static constexpr auto kOomdSystemAvoidXAttr = "trusted.oomd_avoid";
  static constexpr auto kOomdUserAvoidXAttr = "user.oomd_avoid";
  static constexpr auto kPidsCurr = "pids.current";
  static constexpr auto kCgroupKill = "cgroup.kill";

  struct DirEnts {
    std::vector<std::string> dirs;
    std::vector<std::string> files;
  };

  enum DirEntFlags {
    DE_FILE = 1,
    DE_DIR = (1 << 1),
  };

  enum class PressureType {
    SOME = 0,
    FULL,
  };

  class DirFd;
  /*
   * Wrapper class for file descriptor that supports auto closing.
   * Fd is opened read-only as currently that's the only use case.
   */
  class Fd {
   public:
    static SystemMaybe<Fd>
    openat(const DirFd& dirfd, const std::string& path, bool read_only = true);
    // Not safe for accessing cgroup control files. Use Openat instead.
    static SystemMaybe<Fd> open(const std::string& path, bool read_only = true);

    explicit Fd(int fd) : fd_(fd) {}
    Fd() = default;
    Fd(const Fd& other) = delete;
    Fd(Fd&& other) noexcept {
      *this = std::move(other);
    }
    Fd& operator=(const Fd& other) = delete;
    Fd& operator=(Fd&& other) {
      fd_ = other.fd_;
      other.fd_ = -1;
      return *this;
    }
    ~Fd() {
      this->close();
    }

    int fd() const& {
      return fd_;
    }

    /*
     * Take ownership of fd. This object is no longer valid. Caller is
     * responsible for closing the stolen fd.
     */
    int fd() && {
      int stolen_fd = fd_;
      fd_ = -1;
      return stolen_fd;
    }

    // Return inode of the fd, or nullopt if anything fails
    SystemMaybe<uint64_t> inode() const;

   protected:
    void close();
    int fd_{-1};
  };

  /*
   * Wrapper class for directory file descriptor that supports auto closing.
   */
  class DirFd : public Fd {
   public:
    static SystemMaybe<DirFd> open(const std::string& path);
    SystemMaybe<DirFd> openChildDir(const std::string& path) const;

   protected:
    explicit DirFd(int fd) : Fd(fd) {}
  };

  static bool isCgroupValid(const DirFd& dirfd);

  /*
   * Reads a directory and returns the names of the requested entry types
   * Won't return any dotfiles (including ./ and ../)
   */
  static SystemMaybe<DirEnts> readDir(const std::string& path, int flags);

  /*
   * Like readDir, but takes a DirFd
   */
  static SystemMaybe<DirEnts> readDirAt(const DirFd& dirfd, int flags);

  /*
   * Checks if @param path is a directory
   */
  static bool isDir(const std::string& path);

  /*
   * Takes a fully qualified and wildcarded path and returns a set of
   * resolved fully qualified paths.
   */
  static SystemMaybe<std::vector<std::string>> glob(
      const std::string& pattern,
      bool dir_only = false);

  /*
   * Path aware prefix removal.
   *
   * If @param str begins with './', those two characters will be stripped.
   */
  static void removePrefix(std::string& str, const std::string& prefix);

  /* Reads a file and returns a newline separated vector of strings */
  static SystemMaybe<std::vector<std::string>> readFileByLine(
      const std::string& path,
      const char delim = '\n');
  /*
   * Same as variant taking string as argument except rvalue Fd is used, which
   * will be closed right after the call as it is read as a whole and we don't
   * seek offsets on Fd.
   */
  static SystemMaybe<std::vector<std::string>> readFileByLine(Fd&& fd);
  /*
   * Same as above, convenience method accepting the output of Fd::openat
   */
  inline static SystemMaybe<std::vector<std::string>> readFileByLine(
      SystemMaybe<Fd>&& fd) {
    if (fd) {
      return readFileByLine(std::move(*fd));
    }
    return SYSTEM_ERROR(fd.error());
  }

  static SystemMaybe<Unit> checkExistAt(const DirFd& dirfd, const char* name);

  static SystemMaybe<std::vector<std::string>> readControllersAt(
      const DirFd& dirfd);
  static SystemMaybe<std::vector<int>> getPidsAt(const DirFd& dirfd);
  static SystemMaybe<bool> readIsPopulatedAt(const DirFd& dirfd);

  static std::string pressureTypeToString(PressureType type);
  /* Helpers to read PSI files */
  static SystemMaybe<ResourcePressure> readRespressureFromLines(
      const std::vector<std::string>& lines,
      PressureType type = PressureType::FULL);
  static SystemMaybe<int64_t> readRootMemcurrent();
  static SystemMaybe<int64_t> readMemcurrentAt(const DirFd& dirfd);
  static SystemMaybe<ResourcePressure> readRootMempressure(
      PressureType type = PressureType::FULL);
  static SystemMaybe<ResourcePressure> readMempressureAt(
      const DirFd& dirfd,
      PressureType type = PressureType::FULL);
  static SystemMaybe<int64_t> readMinMaxLowHighFromLines(
      const std::vector<std::string>& lines);
  static SystemMaybe<int64_t> readMemhightmpFromLines(
      const std::vector<std::string>& lines);
  static SystemMaybe<int64_t> readMemlowAt(const DirFd& dirfd);
  static SystemMaybe<int64_t> readMemhighAt(const DirFd& dirfd);
  static SystemMaybe<int64_t> readMemmaxAt(const DirFd& dirfd);
  static SystemMaybe<int64_t> readMemhightmpAt(const DirFd& dirfd);
  static SystemMaybe<int64_t> readMemminAt(const DirFd& dirfd);
  static SystemMaybe<int64_t> readSwapCurrentAt(const DirFd& dirfd);
  static SystemMaybe<int64_t> readSwapMaxAt(const DirFd& dirfd);
  static SystemMaybe<ResourcePressure> readRootIopressure(
      PressureType type = PressureType::FULL);
  static SystemMaybe<ResourcePressure> readIopressureAt(
      const DirFd& dirfd,
      PressureType type = PressureType::FULL);
  static SystemMaybe<int64_t> readPidsCurrentAt(const DirFd& dirfd);

  static SystemMaybe<Unit> writeMemhighAt(const DirFd& dirfd, int64_t value);
  static SystemMaybe<Unit> writeMemhightmpAt(
      const DirFd& dirfd,
      int64_t value,
      std::chrono::microseconds duration);
  static SystemMaybe<Unit> writeMemReclaimAt(
      const DirFd& dirfd,
      int64_t value,
      std::optional<int64_t> swappiness);
  static SystemMaybe<Unit> writeFreezeAt(const DirFd& dirfd, int value);
  static SystemMaybe<Unit> writeKillAt(const DirFd& dirfd);

  static SystemMaybe<int64_t> getNrDyingDescendantsAt(const DirFd& dirfd);
  static SystemMaybe<KillPreference> readKillPreferenceAt(const DirFd& path);
  static SystemMaybe<bool> readMemoryOomGroupAt(const DirFd& dirfd);
  static SystemMaybe<IOStat> readIostatAt(const DirFd& dirfd);

  static SystemMaybe<std::unordered_map<std::string, int64_t>> getVmstat(
      const std::string& path = "/proc/vmstat");

  static SystemMaybe<std::unordered_map<std::string, int64_t>> getMeminfo(
      const std::string& path = "/proc/meminfo");

  static SystemMaybe<std::unordered_map<std::string, int64_t>> getMemstatAt(
      const DirFd& dirfd);

  // Return root part of cgroup2 from /proc/mounts/
  static SystemMaybe<std::string> getCgroup2MountPoint(
      const std::string& path = "/proc/mounts");

  // Check if path point to parent path or somewhere inside parent path
  static bool isUnderParentPath(
      const std::string& parent_path,
      const std::string& path);

  /* Getters and setters to set xattr values */
  static SystemMaybe<Unit> setxattr(
      const std::string& path,
      const std::string& attr,
      const std::string& val);
  static SystemMaybe<std::string> getxattr(
      const std::string& path,
      const std::string& attr);

  static SystemMaybe<bool> hasxattrAt(
      const DirFd& dirfd,
      const std::string& attr);

  // Return if device is SSD or HDD given its id in <major>:<minor> format
  static SystemMaybe<DeviceType> getDeviceType(
      const std::string& dev_id,
      const std::string& path = "/sys/dev/block");

  // Return system swappiness
  static SystemMaybe<int> getSwappiness(
      const std::string& path = "/proc/sys/vm/swappiness");

  static SystemMaybe<Unit> setSwappiness(
      int swappiness,
      const std::string& path = "/proc/sys/vm/swappiness");

 private:
  static std::unordered_map<std::string, int64_t> getMemstatLikeFromLines(
      const std::vector<std::string>& lines);
  static SystemMaybe<Unit> writeControlFileAt(
      SystemMaybe<Fd>&& fd,
      const std::string& content);
  static SystemMaybe<DirEnts> readDirFromDIR(DIR* dir, int flags);
};

} // namespace Oomd
