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

#include "oomd/include/CgroupPath.h"
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
  static constexpr auto kIoPressureFile = "io.pressure";
  static constexpr auto kIoStatFile = "io.stat";
  static constexpr auto kDeviceTypeDir = "queue";
  static constexpr auto kDeviceTypeFile = "rotational";

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
  static std::unordered_set<std::string> resolveWildcardPath(
      const CgroupPath& path);

  /*
   * Path aware prefix removal.
   *
   * If @param str begins with './', those two characters will be stripped.
   */
  static void removePrefix(std::string& str, const std::string& prefix);

  /* Reads a file and returns a newline separated vector of strings */
  static std::vector<std::string> readFileByLine(const std::string& path);

  static std::vector<std::string> readControllers(const std::string& path);

  static std::vector<int> getPids(
      const std::string& path,
      bool recursive = false);

  static std::string pressureTypeToString(PressureType type);
  /* Helpers to read PSI files */
  static ResourcePressure readRespressure(
      const std::string& path,
      PressureType type = PressureType::FULL);
  static int64_t readMemcurrent(const std::string& path);
  static ResourcePressure readMempressure(
      const std::string& path,
      PressureType type = PressureType::FULL);
  static int64_t readMinMaxLowHigh(
      const std::string& path,
      const std::string& file);
  static int64_t readMemlow(const std::string& path);
  static int64_t readMemhigh(const std::string& path);
  static int64_t readMemmax(const std::string& path);
  static int64_t readMemhightmp(const std::string& path);
  static int64_t readMemmin(const std::string& path);
  static int64_t readSwapCurrent(const std::string& path);
  static ResourcePressure readIopressure(
      const std::string& path,
      PressureType type = PressureType::FULL);

  static void writeMemhigh(const std::string& path, int64_t value);
  static void writeMemhightmp(
      const std::string& path,
      int64_t value,
      std::chrono::microseconds duration);

  static int64_t getNrDyingDescendants(const std::string& path);

  static IOStat readIostat(const std::string& path);

  static std::unordered_map<std::string, int64_t> getVmstat(
      const std::string& path = "/proc/vmstat");

  static std::unordered_map<std::string, int64_t> getMeminfo(
      const std::string& path = "/proc/meminfo");

  static std::unordered_map<std::string, int64_t> getMemstat(
      const std::string& path);

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

  // Return if device is SSD or HDD given its id in <major>:<minor> format
  static DeviceType getDeviceType(
      const std::string& dev_id,
      const std::string& path = "/sys/dev/block");

  static bool hasGlob(const std::string& s);

 private:
  static std::unordered_map<std::string, int64_t> getMemstatLike(
      const std::string& file);
};

} // namespace Oomd
