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

#include <string>
#include <unordered_map>
#include <vector>

#include "oomd/include/Types.h"

namespace Oomd {

class Fs {
 public:
  static constexpr auto kSubtreeControlFile = "cgroup.subtree_control";
  static constexpr auto kProcsFile = "cgroup.procs";
  static constexpr auto kMemCurrentFile = "memory.current";
  static constexpr auto kMemPressureFile = "memory.pressure";
  static constexpr auto kMemLowFile = "memory.low";
  static constexpr auto kMemSwapCurrentFile = "memory.swap.current";
  static constexpr auto kIoPressureFile = "io.pressure";

  enum class EntryType {
    REG_FILE = 0,
    DIRECTORY,
  };

  /*
   * Reads a directory and returns the names of the requested entry types
   * Won't return any dotfiles (including ./ and ../)
   */
  static std::vector<std::string> readDir(
      const std::string& path,
      EntryType type);

  /* Split string into tokens by delim */
  static std::vector<std::string> split(const std::string& line, char delim);

  /* Reads a file and returns a newline separated vector of strings */
  static std::vector<std::string> readFileByLine(const std::string& path);

  static std::vector<std::string> readControllers(const std::string& path);

  static std::vector<int> getPids(
      const std::string& path,
      bool recursive = false);

  /* Helpers to read PSI files */
  static ResourcePressure readRespressure(const std::string& path);
  static int64_t readMemcurrent(const std::string& path);
  static ResourcePressure readMempressure(const std::string& path);
  static int64_t readMemlow(const std::string& path);
  static int64_t readSwapCurrent(const std::string& path);
  static ResourcePressure readIopressure(const std::string& path);

  static std::unordered_map<std::string, int64_t> getVmstat(
      const std::string& path = "/proc/vmstat");

  static std::unordered_map<std::string, int64_t> getMeminfo(
      const std::string& path = "/proc/meminfo");

  /* Getters and setters to set xattr values */
  static bool setxattr(
      const std::string& path,
      const std::string& attr,
      const std::string& val);
  static std::string getxattr(const std::string& path, const std::string& attr);
};

} // namespace Oomd
