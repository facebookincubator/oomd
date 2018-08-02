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
#include <sys/xattr.h>

#include <cassert>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include <folly/logging/xlog.h>

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
      EntryType type) {
    DIR* d;
    struct dirent* dir;
    std::vector<std::string> v;

    d = opendir(path.c_str());
    if (!d) {
      XLOG(WARNING) << "Unable to open directory=" << path;
      return v;
    }

    while ((dir = readdir(d)) != NULL) {
      if (dir->d_name[0] == '.') {
        continue;
      }

      switch (type) {
        case EntryType::REG_FILE:
          if (dir->d_type == DT_REG) {
            v.push_back(dir->d_name);
          }
          break;
        case EntryType::DIRECTORY:
          if (dir->d_type == DT_DIR) {
            v.push_back(dir->d_name);
          }
          break;
      }
    }

    closedir(d);
    return v;
  }

  /* Split string into tokens by delim */
  static std::vector<std::string> split(const std::string& line, char delim) {
    std::istringstream iss(line);
    std::string item;
    std::vector<std::string> ret;
    while (std::getline(iss, item, delim)) {
      ret.push_back(std::move(item));
    }
    return ret;
  }

  /* Reads a file and returns a newline separated vector of strings */
  static std::vector<std::string> readFileByLine(const std::string& path) {
    std::ifstream f(path, std::ios::in);
    if (!f.is_open()) {
      XLOG(WARNING) << "Unable to open " << path;
    }

    std::string s;
    std::vector<std::string> v;
    while (std::getline(f, s)) {
      v.push_back(std::move(s));
    }

    if (f.bad()) {
      XLOG(WARNING) << "Error while processing file " << path;
    }

    return v;
  }

  static std::vector<std::string> readControllers(const std::string& path) {
    std::vector<std::string> controllers;
    auto lines = readFileByLine(path + "/" + kSubtreeControlFile);
    if (!lines.size()) {
      return controllers;
    }

    controllers = split(lines[0], ' ');

    return controllers;
  }

  static std::vector<int> getPids(
      const std::string& path,
      bool recursive = false) {
    std::vector<int> pids;
    auto files = readDir(path, EntryType::REG_FILE);
    if (std::any_of(files.begin(), files.end(), [](const std::string& s) {
          return s == kProcsFile;
        })) {
      auto str_pids = readFileByLine(path + "/" + kProcsFile);
      for (const auto& sp : str_pids) {
        XLOG(INFO) << "found pid " << sp;
        pids.push_back(std::stoi(sp));
      }
    }

    if (recursive) {
      auto dirs = readDir(path, EntryType::DIRECTORY);
      for (const auto& dir : dirs) {
        auto recursive_pids = getPids(path + "/" + dir, true);
        pids.insert(pids.end(), recursive_pids.begin(), recursive_pids.end());
      }
    }

    return pids;
  }

  static int64_t readMemcurrent(const std::string& path) {
    auto lines = readFileByLine(path + "/" + kMemCurrentFile);
    assert(lines.size() == 1);
    return static_cast<int64_t>(std::stoll(lines[0]));
  }

  static MemoryPressure readMempressure(const std::string& path) {
    auto lines = readFileByLine(path + "/" + kMemPressureFile);

    if (lines.size() == 2) {
      // Upstream v4.16+ format
      //
      // some avg10=0.22 avg60=0.17 avg300=1.11 total=58761459
      // full avg10=0.22 avg60=0.16 avg300=1.08 total=58464525
      std::vector<std::string> toks = split(lines[1], ' ');
      assert(toks[0] == "full");
      std::vector<std::string> avg10 = split(toks[1], '=');
      assert(avg10[0] == "avg10");
      std::vector<std::string> avg60 = split(toks[2], '=');
      assert(avg60[0] == "avg60");
      std::vector<std::string> avg300 = split(toks[3], '=');
      assert(avg300[0] == "avg300");

      return MemoryPressure{
          std::stof(avg10[1]),
          std::stof(avg60[1]),
          std::stof(avg300[1]),
      };
    } else {
      // Old experimental format
      //
      // aggr 316016073
      // some 0.00 0.03 0.05
      // full 0.00 0.03 0.05
      assert(lines.size() == 3);
      std::vector<std::string> toks = split(lines[2], ' ');
      assert(toks[0] == "full");

      return MemoryPressure{
          std::stof(toks[1]),
          std::stof(toks[2]),
          std::stof(toks[3]),
      };
    }
  }

  static int64_t readMemlow(const std::string& path) {
    auto lines = readFileByLine(path + "/" + kMemLowFile);
    assert(lines.size() == 1);
    return static_cast<int64_t>(std::stoll(lines[0]));
  }

  static int64_t readSwapCurrent(const std::string& path) {
    auto lines = readFileByLine(path + "/" + kMemSwapCurrentFile);
    assert(lines.size() == 1);
    return static_cast<int64_t>(std::stoll(lines[0]));
  }

  static std::unordered_map<std::string, int64_t> getVmstat(
      const std::string& path = "/proc/vmstat") {
    auto lines = readFileByLine(path);
    std::unordered_map<std::string, int64_t> map;
    char space{' '};

    for (auto& line : lines) {
      std::stringstream ss(line);
      std::string item;

      // get key
      std::getline(ss, item, space);
      std::string key{item};

      // insert value into map
      std::getline(ss, item, space);
      map[key] = static_cast<int64_t>(std::stoll(item));
    }

    return map;
  }

  static std::unordered_map<std::string, int64_t> getMeminfo(
      const std::string& path = "/proc/meminfo") {
    char name[256] = {0};
    unsigned long val;
    std::unordered_map<std::string, int64_t> map;

    auto lines = readFileByLine(path);
    for (auto& line : lines) {
      int ret = sscanf(line.c_str(), "%255[^:]:%*[ \t]%lu%*s\n", name, &val);
      if (ret == 2) {
        map[name] = val * 1024;
      }
    }

    return map;
  }

  static ssize_t writeFull(int fd, const char* msg_buf, size_t count) {
    ssize_t totalBytes = 0;
    ssize_t r;
    do {
      r = write(fd, msg_buf, count);
      if (r == -1) {
        if (errno == EINTR) {
          continue;
        }
        return r;
      }

      totalBytes += r;
      msg_buf += r;
      count -= r;
    } while (r != 0 && count); // 0 means EOF

    return totalBytes;
  }

  static bool setxattr(
      const std::string& path,
      const std::string& attr,
      const std::string& val) {
    int ret =
        ::setxattr(path.c_str(), attr.c_str(), val.c_str(), val.size(), 0);
    if (ret == -1) {
      LOG(WARNING) << "Unable to set xattr " << attr << "=" << val << " on "
                   << path << ". errno=" << errno;
      return false;
    }
    return true;
  }

  static std::string getxattr(
      const std::string& path,
      const std::string& attr) {
    std::string val;

    int size = ::getxattr(path.c_str(), attr.c_str(), nullptr, 0);
    if (size <= 0) {
      return val;
    }

    val.resize(size);
    ::getxattr(path.c_str(), attr.c_str(), &val[0], val.size());
    return val;
  }
};

} // namespace Oomd
