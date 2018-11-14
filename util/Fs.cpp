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

#include "oomd/util/Fs.h"

#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/xattr.h>
#include <unistd.h>

#include <fstream>
#include <sstream>

#include "oomd/Log.h"
#include "oomd/include/Assert.h"

namespace Oomd {

std::vector<std::string> Fs::readDir(const std::string& path, EntryType type) {
  DIR* d;
  std::vector<std::string> v;

  d = opendir(path.c_str());
  if (!d) {
    OLOG << "Unable to open directory=" << path;
    return v;
  }

  while (struct dirent* dir = ::readdir(d)) {
    if (dir->d_name[0] == '.') {
      continue;
    }

    auto file = path + "/" + dir->d_name;
    struct stat buf;
    int ret = ::lstat(file.c_str(), &buf);
    if (ret == -1) {
      auto errorNum = errno;
      // TODO: better error handling for methods here instead of ignoring.
      OLOG << "lstat failed with errno: " << errorNum << ", file: " << file
           << ", ignoring file";
      continue;
    }

    switch (type) {
      case EntryType::REG_FILE:
        if (buf.st_mode & S_IFREG) {
          v.push_back(dir->d_name);
        }
        break;
      case EntryType::DIRECTORY:
        if (buf.st_mode & S_IFDIR) {
          v.push_back(dir->d_name);
        }
        break;
    }
  }

  ::closedir(d);
  return v;
}

std::vector<std::string> Fs::split(const std::string& line, char delim) {
  std::istringstream iss(line);
  std::string item;
  std::vector<std::string> ret;
  while (std::getline(iss, item, delim)) {
    if (item.size()) {
      ret.push_back(std::move(item));
    }
  }
  return ret;
}

/* Reads a file and returns a newline separated vector of strings */
std::vector<std::string> Fs::readFileByLine(const std::string& path) {
  std::ifstream f(path, std::ios::in);
  if (!f.is_open()) {
    OLOG << "Unable to open " << path;
    return {};
  }

  std::string s;
  std::vector<std::string> v;
  while (std::getline(f, s)) {
    v.push_back(std::move(s));
  }

  if (f.bad()) {
    OLOG << "Error while processing file " << path;
  }

  return v;
}

std::vector<std::string> Fs::readControllers(const std::string& path) {
  std::vector<std::string> controllers;
  auto lines = readFileByLine(path + "/" + kSubtreeControlFile);
  if (!lines.size()) {
    return controllers;
  }

  controllers = split(lines[0], ' ');

  return controllers;
}

std::vector<int> Fs::getPids(const std::string& path, bool recursive) {
  std::vector<int> pids;
  auto files = readDir(path, EntryType::REG_FILE);
  if (std::any_of(files.begin(), files.end(), [](const std::string& s) {
        return s == kProcsFile;
      })) {
    auto str_pids = readFileByLine(path + "/" + kProcsFile);
    for (const auto& sp : str_pids) {
      OLOG << "found pid " << sp;
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

ResourcePressure Fs::readRespressure(const std::string& path) {
  auto lines = readFileByLine(path);

  if (lines.size() == 2) {
    // Upstream v4.16+ format
    //
    // some avg10=0.22 avg60=0.17 avg300=1.11 total=58761459
    // full avg10=0.22 avg60=0.16 avg300=1.08 total=58464525
    std::vector<std::string> toks = split(lines[1], ' ');
    OCHECK(toks[0] == "full");
    std::vector<std::string> avg10 = split(toks[1], '=');
    OCHECK(avg10[0] == "avg10");
    std::vector<std::string> avg60 = split(toks[2], '=');
    OCHECK(avg60[0] == "avg60");
    std::vector<std::string> avg300 = split(toks[3], '=');
    OCHECK(avg300[0] == "avg300");

    return ResourcePressure{
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
    OCHECK(lines.size() == 3);
    std::vector<std::string> toks = split(lines[2], ' ');
    OCHECK(toks[0] == "full");

    return ResourcePressure{
        std::stof(toks[1]),
        std::stof(toks[2]),
        std::stof(toks[3]),
    };
  }
}

int64_t Fs::readMemcurrent(const std::string& path) {
  auto lines = readFileByLine(path + "/" + kMemCurrentFile);
  OCHECK(lines.size() == 1);
  return static_cast<int64_t>(std::stoll(lines[0]));
}

ResourcePressure Fs::readMempressure(const std::string& path) {
  return readRespressure(path + "/" + kMemPressureFile);
}

int64_t Fs::readMemlow(const std::string& path) {
  auto lines = readFileByLine(path + "/" + kMemLowFile);
  OCHECK(lines.size() == 1);
  if (lines[0] == "max") {
    return std::numeric_limits<int64_t>::max();
  }
  return static_cast<int64_t>(std::stoll(lines[0]));
}

int64_t Fs::readSwapCurrent(const std::string& path) {
  auto lines = readFileByLine(path + "/" + kMemSwapCurrentFile);
  OCHECK(lines.size() == 1);
  return static_cast<int64_t>(std::stoll(lines[0]));
}

std::unordered_map<std::string, int64_t> Fs::getVmstat(
    const std::string& path) {
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

std::unordered_map<std::string, int64_t> Fs::getMeminfo(
    const std::string& path) {
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

ResourcePressure Fs::readIopressure(const std::string& path) {
  const std::string ioPressurePath = path + "/" + kIoPressureFile;

  // earlier kernels had only mempressure, throw an exception if missing
  std::ifstream f(ioPressurePath, std::ios::in);
  if (!f.is_open()) {
    throw std::system_error(ENOENT, std::system_category());
  }
  return readRespressure(ioPressurePath);
}

bool Fs::setxattr(
    const std::string& path,
    const std::string& attr,
    const std::string& val) {
  int ret = ::setxattr(path.c_str(), attr.c_str(), val.c_str(), val.size(), 0);
  if (ret == -1) {
    OLOG << "Unable to set xattr " << attr << "=" << val << " on " << path
         << ". errno=" << errno;
    return false;
  }
  return true;
}

std::string Fs::getxattr(const std::string& path, const std::string& attr) {
  std::string val;

  int size = ::getxattr(path.c_str(), attr.c_str(), nullptr, 0);
  if (size <= 0) {
    return val;
  }

  val.resize(size);
  ::getxattr(path.c_str(), attr.c_str(), &val[0], val.size());
  return val;
}

} // namespace Oomd
