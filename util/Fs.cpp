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
#include <fnmatch.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/xattr.h>
#include <unistd.h>

#include <deque>
#include <fstream>
#include <utility>

#include "oomd/Log.h"
#include "oomd/include/Assert.h"
#include "oomd/util/Util.h"

namespace Oomd {

std::vector<std::string> Fs::readDir(const std::string& path, EntryType type) {
  DIR* d;
  std::vector<std::string> v;

  d = ::opendir(path.c_str());
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

bool Fs::isDir(const std::string& path) {
  struct stat sb;
  if (!::stat(path.c_str(), &sb) && S_ISDIR(sb.st_mode)) {
    return true;
  }

  return false;
}

std::unordered_set<std::string> Fs::resolveWildcardPath(
    const std::string& path) {
  std::unordered_set<std::string> ret;
  if (path.empty()) {
    return ret;
  }

  auto parts = Util::split(path, '/');
  std::deque<std::pair<std::string, size_t>> queue;
  // Add initial path piece to begin search on. Start at root
  // if provided path is absolute, else go with relative dir.
  queue.emplace_back((path[0] == '/' ? "/" : "./"), 0);

  // Perform a DFS on the entire search space. Note that we pattern
  // match at each level of the provided path to eliminate "dead"
  // branches. The algorithm is still O(N) but in practice this will
  // prevent us from enumerating every entry in the root filesystem.
  //
  // We choose DFS because we predict the FS tree is wider than it
  // is tall. DFS will use less space than BFS in this case because
  // it does not need to store every node at each level of the tree.
  while (!queue.empty()) {
    const auto front = queue.front(); // copy
    queue.pop_front();

    // We can't continue BFS if we've hit a regular file
    if (!isDir(front.first)) {
      continue;
    }

    // Only resolve regular files and directories
    auto entries = readDir(front.first, EntryType::DIRECTORY);
    auto files = readDir(front.first, EntryType::REG_FILE);
    entries.insert(entries.end(), files.begin(), files.end());

    for (const auto& entry : entries) {
      if (::fnmatch(parts[front.second].c_str(), entry.c_str(), 0) == 0) {
        if (front.second == parts.size() - 1) {
          // We have reached a leaf, add it to the return set
          ret.emplace(front.first + entry);
        } else if (front.second < parts.size() - 1) {
          // There are still more parts of the provided path to search.
          //
          // Note that we add the '/' at the end of the new path. This makes
          // the recursive case easier, as the recursive case need only
          // add the next part of the path on. Also note the 'emplace_front'
          // that makes the deque into a stack (thus the DFS).
          queue.emplace_front(front.first + entry + "/", front.second + 1);
        }
      }
    }
  }

  return ret;
}

void Fs::removePrefix(std::string& str, const std::string& prefix) {
  if (str.find(prefix) != std::string::npos) {
    // Strip the leading './' if it exists and we haven't been explicitly
    // told to strip it
    if (str.find("./") == 0 && prefix.find("./") != 0) {
      str.erase(0, 2);
    }

    str.erase(0, prefix.size());
  }
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
  auto lines = readFileByLine(path + "/" + kControllersFile);
  if (!lines.size()) {
    return controllers;
  }

  controllers = Util::split(lines[0], ' ');

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

std::string Fs::pressureTypeToString(PressureType type) {
  switch (type) {
    case PressureType::SOME:
      return "some";
    case PressureType::FULL:
      return "full";
  }
  throw std::runtime_error("Invalid PressureType. Code should not be reached");
}

ResourcePressure Fs::readRespressure(
    const std::string& path,
    PressureType type) {
  auto lines = readFileByLine(path);

  auto type_name = pressureTypeToString(type);
  size_t pressure_line_index = 0;
  switch (type) {
    case PressureType::SOME:
      pressure_line_index = 0;
      break;
    case PressureType::FULL:
      pressure_line_index = 1;
      break;
  }
  if (lines.size() == 2) {
    // Upstream v4.16+ format
    //
    // some avg10=0.22 avg60=0.17 avg300=1.11 total=58761459
    // full avg10=0.22 avg60=0.16 avg300=1.08 total=58464525
    std::vector<std::string> toks =
        Util::split(lines[pressure_line_index], ' ');
    OCHECK_EXCEPT(
        toks[0] == type_name, bad_control_file(path + ": invalid format"));
    std::vector<std::string> avg10 = Util::split(toks[1], '=');
    OCHECK_EXCEPT(
        avg10[0] == "avg10", bad_control_file(path + ": invalid format"));
    std::vector<std::string> avg60 = Util::split(toks[2], '=');
    OCHECK_EXCEPT(
        avg60[0] == "avg60", bad_control_file(path + ": invalid format"));
    std::vector<std::string> avg300 = Util::split(toks[3], '=');
    OCHECK_EXCEPT(
        avg300[0] == "avg300", bad_control_file(path + ": invalid format"));
    std::vector<std::string> total = Util::split(toks[4], '=');
    OCHECK_EXCEPT(
        total[0] == "total", bad_control_file(path + ": invalid format"));

    return ResourcePressure{
        std::stof(avg10[1]),
        std::stof(avg60[1]),
        std::stof(avg300[1]),
        std::chrono::microseconds(std::stoull(total[1])),
    };
  } else if (lines.size() == 3) {
    // Old experimental format
    //
    // aggr 316016073
    // some 0.00 0.03 0.05
    // full 0.00 0.03 0.05
    std::vector<std::string> toks =
        Util::split(lines[pressure_line_index + 1], ' ');
    OCHECK_EXCEPT(
        toks[0] == type_name, bad_control_file(path + ": invalid format"));

    return ResourcePressure{
        std::stof(toks[1]),
        std::stof(toks[2]),
        std::stof(toks[3]),
    };
  } else {
    // Missing the control file
    throw bad_control_file(path + ": missing file");
  }
}

int64_t Fs::readMemcurrent(const std::string& path) {
  auto lines = readFileByLine(path + "/" + kMemCurrentFile);
  OCHECK_EXCEPT(lines.size() == 1, bad_control_file(path + ": missing file"));
  return static_cast<int64_t>(std::stoll(lines[0]));
}

int64_t Fs::readMemcurrentWildcard(const std::string& path) {
  auto resolved = resolveWildcardPath(path + "/" + kMemCurrentFile);
  int64_t total = 0;

  for (const auto& path : resolved) {
    auto lines = readFileByLine(path);
    OCHECK_EXCEPT(lines.size() == 1, bad_control_file(path + ": missing file"));
    total += static_cast<int64_t>(std::stoll(lines[0]));
  }

  return total;
}

ResourcePressure Fs::readMempressure(
    const std::string& path,
    PressureType type) {
  return readRespressure(path + "/" + kMemPressureFile, type);
}

int64_t Fs::readMinMaxLowHigh(
    const std::string& path,
    const std::string& file) {
  auto lines = readFileByLine(path + "/" + file);
  OCHECK_EXCEPT(lines.size() == 1, bad_control_file(path + ": missing file"));
  if (lines[0] == "max") {
    return std::numeric_limits<int64_t>::max();
  }
  return static_cast<int64_t>(std::stoll(lines[0]));
}

int64_t Fs::readMemlow(const std::string& path) {
  return Fs::readMinMaxLowHigh(path, kMemLowFile);
}

int64_t Fs::readMemhigh(const std::string& path) {
  return Fs::readMinMaxLowHigh(path, kMemHighFile);
}

int64_t Fs::readMemmin(const std::string& path) {
  return Fs::readMinMaxLowHigh(path, kMemMinFile);
}

int64_t Fs::readSwapCurrent(const std::string& path) {
  auto lines = readFileByLine(path + "/" + kMemSwapCurrentFile);

  // The swap controller can be disabled via CONFIG_MEMCG_SWAP=n
  if (lines.size() == 1) {
    return static_cast<int64_t>(std::stoll(lines[0]));
  } else {
    return 0;
  }
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

std::unordered_map<std::string, int64_t> Fs::getMemstat(
    const std::string& path) {
  char name[256] = {0};
  unsigned long val;
  std::unordered_map<std::string, int64_t> map;

  auto lines = readFileByLine(path + "/" + kMemStatFile);
  for (const auto& line : lines) {
    int ret = sscanf(line.c_str(), "%255s %lu\n", name, &val);
    if (ret == 2) {
      map[name] = val;
    }
  }

  return map;
}

ResourcePressure Fs::readIopressure(
    const std::string& path,
    PressureType type) {
  const std::string ioPressurePath = path + "/" + kIoPressureFile;

  // earlier kernels had only mempressure, throw an exception if missing
  std::ifstream f(ioPressurePath, std::ios::in);
  if (!f.is_open()) {
    throw std::system_error(ENOENT, std::system_category());
  }
  return readRespressure(ioPressurePath, type);
}

void Fs::writeMemhigh(const std::string& path, int64_t value) {
  std::ofstream f(path + "/" + kMemHighFile);
  f << value;
  f.flush();
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

bool Fs::isUnderParentPath(
    const std::string& parent_path,
    const std::string& path) {
  if (parent_path.empty() || path.empty()) {
    return false;
  }

  auto parent_parts = Util::split(parent_path, '/');
  auto path_parts = Util::split(path, '/');
  int i = 0;

  if (path_parts.size() < parent_parts.size()) {
    return false;
  }

  for (const auto& parts : parent_parts) {
    if (path_parts[i] != parts) {
      return false;
    }
    i++;
  }
  return true;
}

std::string Fs::getCgroup2MountPoint(const std::string& path) {
  auto lines = readFileByLine(path);
  for (auto& line : lines) {
    auto parts = Util::split(line, ' ');
    if (parts.size() > 1) {
      if (parts[0] == "cgroup2") {
        return parts[1] + '/';
      }
    }
  }
  return "";
}

} // namespace Oomd
