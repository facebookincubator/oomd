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

#include <exception>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "oomd/include/Types.h"

namespace Oomd {

class Fs {
 public:
  class bad_control_file : public std::runtime_error {
   public:
    explicit bad_control_file(const std::string& msg)
        : std::runtime_error(msg) {}
    explicit bad_control_file(const char* msg) : std::runtime_error(msg) {}
  };

  static constexpr auto kControllersFile = "cgroup.controllers";
  static constexpr auto kSubtreeControlFile = "cgroup.subtree_control";
  static constexpr auto kProcsFile = "cgroup.procs";
  static constexpr auto kEventsFile = "cgroup.events";
  static constexpr auto kMemCurrentFile = "memory.current";
  static constexpr auto kMemPressureFile = "memory.pressure";
  static constexpr auto kMemLowFile = "memory.low";
  static constexpr auto kMemHighFile = "memory.high";
  static constexpr auto kMemMaxFile = "memory.max";
  static constexpr auto kMemHighTmpFile = "memory.high.tmp";
  static constexpr auto kMemMinFile = "memory.min";
  static constexpr auto kMemStatFile = "memory.stat";
  static constexpr auto kCgroupStatFile = "cgroup.stat";
  static constexpr auto kMemSwapCurrentFile = "memory.swap.current";
  static constexpr auto kMemOomGroupFile = "memory.oom.group";
  static constexpr auto kIoPressureFile = "io.pressure";
  static constexpr auto kIoStatFile = "io.stat";
  static constexpr auto kDeviceTypeDir = "queue";
  static constexpr auto kDeviceTypeFile = "rotational";
  static constexpr auto kOomdPreferXAttr = "trusted.oomd_prefer";
  static constexpr auto kOomdAvoidXAttr = "trusted.oomd_avoid";

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
    static Fd
    openat(const DirFd& dirfd, const std::string& path, bool read_only = true);

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

    int fd() const {
      return fd_;
    }
    bool isValid() const {
      return fd_ >= 0;
    }
    // Return inode of the fd, or nullopt if anything fails
    std::optional<uint64_t> inode() const;

   protected:
    explicit Fd(int fd) : fd_(fd) {}
    void close() const;
    int fd_{-1};
  };

  /*
   * Wrapper class for directory file descriptor that supports auto closing.
   */
  class DirFd : public Fd {
   public:
    static DirFd open(const std::string& path);

   protected:
    explicit DirFd(int fd) : Fd(fd) {}
  };

  static bool isCgroupValid(const DirFd& dirfd);

  /*
   * Reads a directory and returns the names of the requested entry types
   * Won't return any dotfiles (including ./ and ../)
   */
  static struct DirEnts readDir(const std::string& path, int flags);

  /*
   * Checks if @param path is a directory
   */
  static bool isDir(const std::string& path);

  /*
   * Takes a fully qualified and wildcarded path and returns a set of
   * resolved fully qualified paths.
   */
  static std::vector<std::string> glob(
      const std::string& pattern,
      bool dir_only = false);

  /*
   * Path aware prefix removal.
   *
   * If @param str begins with './', those two characters will be stripped.
   */
  static void removePrefix(std::string& str, const std::string& prefix);

  /* Reads a file and returns a newline separated vector of strings */
  static std::vector<std::string> readFileByLine(const std::string& path);
  /*
   * Same as variant taking string as argument except rvalue Fd is used, which
   * will be closed right after the call as it is read as a whole and we don't
   * seek offsets on Fd.
   */
  static std::vector<std::string> readFileByLine(Fd&& fd);

  static std::vector<std::string> readControllersAt(const DirFd& dirfd);
  static std::vector<int> getPidsAt(const DirFd& dirfd);

  static bool readIsPopulatedFromLines(const std::vector<std::string>& lines);
  static bool readIsPopulatedAt(const DirFd& dirfd);

  static std::string pressureTypeToString(PressureType type);
  /* Helpers to read PSI files */
  static ResourcePressure readRespressureFromLines(
      const std::vector<std::string>& lines,
      PressureType type = PressureType::FULL);
  static int64_t readRootMemcurrent();
  static int64_t readMemcurrentAt(const DirFd& dirfd);
  static ResourcePressure readRootMempressure(
      PressureType type = PressureType::FULL);
  static ResourcePressure readMempressureAt(
      const DirFd& dirfd,
      PressureType type = PressureType::FULL);
  static int64_t readMinMaxLowHighFromLines(
      const std::vector<std::string>& lines);
  static int64_t readMemhightmpFromLines(const std::vector<std::string>& lines);
  static int64_t readMemlowAt(const DirFd& dirfd);
  static int64_t readMemhighAt(const DirFd& dirfd);
  static int64_t readMemmaxAt(const DirFd& dirfd);
  static int64_t readMemhightmpAt(const DirFd& dirfd);
  static int64_t readMemminAt(const DirFd& dirfd);
  static int64_t readSwapCurrentAt(const DirFd& dirfd);
  static ResourcePressure readRootIopressure(
      PressureType type = PressureType::FULL);
  static ResourcePressure readIopressureAt(
      const DirFd& dirfd,
      PressureType type = PressureType::FULL);

  static void writeMemhighAt(const DirFd& dirfd, int64_t value);
  static void writeMemhightmpAt(
      const DirFd& dirfd,
      int64_t value,
      std::chrono::microseconds duration);

  static int64_t getNrDyingDescendantsAt(const DirFd& dirfd);
  static KillPreference readKillPreferenceAt(const DirFd& path);
  static bool readMemoryOomGroupAt(const DirFd& dirfd);
  static IOStat readIostatAt(const DirFd& dirfd);

  static std::unordered_map<std::string, int64_t> getVmstat(
      const std::string& path = "/proc/vmstat");

  static std::unordered_map<std::string, int64_t> getMeminfo(
      const std::string& path = "/proc/meminfo");

  static std::unordered_map<std::string, int64_t> getMemstatAt(
      const DirFd& dirfd);

  // Return root part of cgroup2 from /proc/mounts/
  static std::string getCgroup2MountPoint(
      const std::string& path = "/proc/mounts");

  // Check if path point to parent path or somewhere inside parent path
  static bool isUnderParentPath(
      const std::string& parent_path,
      const std::string& path);

  /* Getters and setters to set xattr values */
  static bool setxattr(
      const std::string& path,
      const std::string& attr,
      const std::string& val);
  static std::string getxattr(const std::string& path, const std::string& attr);

  static bool hasxattrAt(const DirFd& dirfd, const std::string& attr);

  // Return if device is SSD or HDD given its id in <major>:<minor> format
  static DeviceType getDeviceType(
      const std::string& dev_id,
      const std::string& path = "/sys/dev/block");

 private:
  static std::unordered_map<std::string, int64_t> getMemstatLikeFromLines(
      const std::vector<std::string>& lines);
  static IOStat readIostatFromLines(const std::vector<std::string>& lines);
  static void writeControlFileAt(Fd&& fd, const std::string& content);
};

} // namespace Oomd
