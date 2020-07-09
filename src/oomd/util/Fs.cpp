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
#include <fcntl.h>
#include <glob.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/xattr.h>
#include <unistd.h>

#include <cinttypes>
#include <deque>
#include <fstream>
#include <sstream>
#include <utility>

#include "oomd/include/Assert.h"
#include "oomd/util/Util.h"

namespace {

enum class PsiFormat {
  MISSING = 0, // File is missing
  INVALID, // Don't recognize
  EXPERIMENTAL, // Experimental format
  UPSTREAM, // Upstream kernel format
};

PsiFormat getPsiFormat(const std::vector<std::string>& lines) {
  if (lines.size() == 0) {
    return PsiFormat::MISSING;
  }

  const auto& first = lines[0];
  if (Oomd::Util::startsWith("some", first) && lines.size() >= 2) {
    return PsiFormat::UPSTREAM;
  } else if (Oomd::Util::startsWith("aggr", first) && lines.size() >= 3) {
    return PsiFormat::EXPERIMENTAL;
  } else {
    return PsiFormat::INVALID;
  }
}

}; // namespace

namespace Oomd {

std::optional<Fs::Fd>
Fs::Fd::openat(const DirFd& dirfd, const std::string& path, bool read_only) {
  int flags = read_only ? O_RDONLY : O_WRONLY;
  const auto fd = ::openat(dirfd.fd(), path.c_str(), flags);
  if (fd == -1) {
    return std::nullopt;
  }
  return std::make_optional(Fd(fd));
}

std::optional<uint64_t> Fs::Fd::inode() const {
  struct ::stat buf;
  if (::fstat(fd_, &buf) == 0) {
    return static_cast<uint64_t>(buf.st_ino);
  }
  return std::nullopt;
}

void Fs::Fd::close() {
  if (fd_ != -1) {
    ::close(fd_);
    fd_ = -1;
  }
}

std::optional<Fs::DirFd> Fs::DirFd::open(const std::string& path) {
  int fd = ::open(path.c_str(), O_RDONLY | O_DIRECTORY);
  if (fd == -1) {
    return std::nullopt;
  }
  return std::make_optional(DirFd(fd));
}

bool Fs::isCgroupValid(const DirFd& dirfd) {
  // If cgroup.controllers file exists, the cgroup should still be valid
  return ::faccessat(dirfd.fd(), kControllersFile, F_OK, 0) == 0;
}

struct Fs::DirEnts Fs::readDir(const std::string& path, int flags) {
  DIR* d;
  struct Fs::DirEnts de;

  d = ::opendir(path.c_str());
  if (!d) {
    return de;
  }

  while (struct dirent* dir = ::readdir(d)) {
    if (dir->d_name[0] == '.') {
      continue;
    }

    /*
     * Optimisation: Avoid doing lstat calls if kernfs gives us back d_type.
     * This actually can be pretty useful, since avoiding lstat()ing everything
     * can reduce oomd CPU usage by ~10% on a reasonably sized cgroup
     * hierarchy.
     */
    if ((flags & DirEntFlags::DE_FILE) && dir->d_type == DT_REG) {
      de.files.push_back(dir->d_name);
      continue;
    }
    if ((flags & DirEntFlags::DE_DIR) && dir->d_type == DT_DIR) {
      de.dirs.push_back(dir->d_name);
      continue;
    }

    auto file = path + "/" + dir->d_name;
    struct stat buf;
    int ret = ::lstat(file.c_str(), &buf);
    if (ret == -1) {
      continue;
    }

    if ((flags & DirEntFlags::DE_FILE) && (buf.st_mode & S_IFREG)) {
      de.files.push_back(dir->d_name);
    }
    if ((flags & DirEntFlags::DE_DIR) && (buf.st_mode & S_IFDIR)) {
      de.files.push_back(dir->d_name);
    }
  }

  ::closedir(d);
  return de;
}

bool Fs::isDir(const std::string& path) {
  struct stat sb;
  if (!::stat(path.c_str(), &sb) && S_ISDIR(sb.st_mode)) {
    return true;
  }

  return false;
}

std::vector<std::string> Fs::glob(const std::string& pattern, bool dir_only) {
  glob_t globbuf;
  std::vector<std::string> ret;
  int flags = GLOB_NOSORT | GLOB_BRACE;
  if (dir_only) {
    flags |= GLOB_ONLYDIR;
  }
  if (0 == ::glob(pattern.c_str(), flags, nullptr, &globbuf)) {
    for (size_t i = 0; i < globbuf.gl_pathc; i++) {
      std::string path(globbuf.gl_pathv[i]);
      // GLOB_ONLYDIR is a hint and we need to double check
      if (dir_only && !isDir(path)) {
        continue;
      }
      ret.emplace_back(std::move(path));
    }
  }
  ::globfree(&globbuf);
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
std::optional<std::vector<std::string>> Fs::readFileByLine(
    const std::string& path) {
  std::ifstream f(path, std::ios::in);
  if (!f.is_open()) {
    return std::nullopt;
  }

  std::string s;
  std::vector<std::string> v;
  while (std::getline(f, s)) {
    v.push_back(std::move(s));
  }

  // Error when reading file, and thus content might be corrupted
  if (f.bad()) {
    return std::nullopt;
  }

  return v;
}

std::optional<std::vector<std::string>> Fs::readFileByLine(Fd&& fd) {
  auto fp = ::fdopen(std::move(fd).fd(), "r");
  if (fp == nullptr) {
    return std::nullopt;
  }
  std::vector<std::string> v;
  char* line = nullptr;
  size_t len = 0;
  ssize_t read;
  errno = 0;
  while ((read = ::getline(&line, &len, fp)) != -1) {
    OCHECK(line != nullptr);
    if (read > 0 && line[read - 1] == '\n') {
      v.emplace_back(line, read - 1);
    } else {
      v.emplace_back(line);
    }
  }
  bool has_error = errno != 0;

  ::free(line);
  ::fclose(fp);

  // Error when reading file, and thus content might be corrupted
  if (has_error) {
    return std::nullopt;
  }
  return v;
}

std::optional<std::vector<std::string>> Fs::readControllersAt(
    const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kControllersFile))) {
    return Util::split(lines.value()[0], ' ');
  }
  return std::nullopt;
}

std::optional<std::vector<int>> Fs::getPidsAt(const DirFd& dirfd) {
  if (auto str_pids = readFileByLine(Fs::Fd::openat(dirfd, kProcsFile))) {
    std::vector<int> pids;
    for (const auto& sp : str_pids.value()) {
      pids.push_back(std::stoi(sp));
    }
    return pids;
  }
  return std::nullopt;
}

std::optional<bool> Fs::readIsPopulatedAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kEventsFile))) {
    for (const auto& line : lines.value()) {
      std::vector<std::string> toks = Util::split(line, ' ');
      if (toks.size() == 2 && toks[0] == "populated") {
        if (toks[1] == "1") {
          return true;
        } else if (toks[1] == "0") {
          return false;
        } else {
          throw bad_control_file("invalid format");
        }
      }
    }

    throw bad_control_file("invalid format");
  }
  return std::nullopt;
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

ResourcePressure Fs::readRespressureFromLines(
    const std::vector<std::string>& lines,
    PressureType type) {
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

  switch (getPsiFormat(lines)) {
    case PsiFormat::UPSTREAM: {
      // Upstream v4.16+ format
      //
      // some avg10=0.22 avg60=0.17 avg300=1.11 total=58761459
      // full avg10=0.22 avg60=0.16 avg300=1.08 total=58464525
      std::vector<std::string> toks =
          Util::split(lines[pressure_line_index], ' ');
      OCHECK_EXCEPT(toks[0] == type_name, bad_control_file("invalid format"));
      std::vector<std::string> avg10 = Util::split(toks[1], '=');
      OCHECK_EXCEPT(avg10[0] == "avg10", bad_control_file("invalid format"));
      std::vector<std::string> avg60 = Util::split(toks[2], '=');
      OCHECK_EXCEPT(avg60[0] == "avg60", bad_control_file("invalid format"));
      std::vector<std::string> avg300 = Util::split(toks[3], '=');
      OCHECK_EXCEPT(avg300[0] == "avg300", bad_control_file("invalid format"));
      std::vector<std::string> total = Util::split(toks[4], '=');
      OCHECK_EXCEPT(total[0] == "total", bad_control_file("invalid format"));

      return ResourcePressure{
          std::stof(avg10[1]),
          std::stof(avg60[1]),
          std::stof(avg300[1]),
          std::chrono::microseconds(std::stoull(total[1])),
      };
    }
    case PsiFormat::EXPERIMENTAL: {
      // Old experimental format
      //
      // aggr 316016073
      // some 0.00 0.03 0.05
      // full 0.00 0.03 0.05
      std::vector<std::string> toks =
          Util::split(lines[pressure_line_index + 1], ' ');
      OCHECK_EXCEPT(toks[0] == type_name, bad_control_file("invalid format"));

      return ResourcePressure{
          std::stof(toks[1]),
          std::stof(toks[2]),
          std::stof(toks[3]),
          std::nullopt,
      };
    }
    case PsiFormat::MISSING:
      // Missing the control file
      throw bad_control_file("missing file");
    case PsiFormat::INVALID:
      throw bad_control_file("invalid format");
  }

  // To silence g++ compiler warning about enums
  throw std::runtime_error("Not all enums handled");
}

std::optional<int64_t> Fs::readRootMemcurrent() {
  auto meminfo = getMeminfo("/proc/meminfo");
  if (meminfo.size() == 0) {
    return std::nullopt;
  }
  return meminfo["MemTotal"] - meminfo["MemFree"];
}

std::optional<int64_t> Fs::readMemcurrentAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemCurrentFile))) {
    return static_cast<int64_t>(std::stoll(lines.value()[0]));
  }
  return std::nullopt;
}

std::optional<ResourcePressure> Fs::readRootMempressure(PressureType type) {
  if (auto lines = readFileByLine("/proc/pressure/memory")) {
    return readRespressureFromLines(lines.value(), type);
  } else if (auto lines = readFileByLine("/proc/mempressure")) {
    return readRespressureFromLines(lines.value(), type);
  }
  return std::nullopt;
}

std::optional<ResourcePressure> Fs::readMempressureAt(
    const DirFd& dirfd,
    PressureType type) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemPressureFile))) {
    return readRespressureFromLines(lines.value(), type);
  }
  return std::nullopt;
}

int64_t Fs::readMinMaxLowHighFromLines(const std::vector<std::string>& lines) {
  OCHECK_EXCEPT(lines.size() == 1, bad_control_file("missing file"));
  if (lines[0] == "max") {
    return std::numeric_limits<int64_t>::max();
  }
  return static_cast<int64_t>(std::stoll(lines[0]));
}

std::optional<int64_t> Fs::readMemlowAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemLowFile))) {
    return Fs::readMinMaxLowHighFromLines(lines.value());
  }
  return std::nullopt;
}

std::optional<int64_t> Fs::readMemhighAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemHighFile))) {
    return Fs::readMinMaxLowHighFromLines(lines.value());
  }
  return std::nullopt;
}

std::optional<int64_t> Fs::readMemmaxAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemMaxFile))) {
    return Fs::readMinMaxLowHighFromLines(lines.value());
  }
  return std::nullopt;
}

int64_t Fs::readMemhightmpFromLines(const std::vector<std::string>& lines) {
  OCHECK_EXCEPT(lines.size() == 1, bad_control_file("missing file"));
  auto tokens = Util::split(lines[0], ' ');
  OCHECK_EXCEPT(tokens.size() == 2, bad_control_file("invalid format"));
  if (tokens[0] == "max") {
    return std::numeric_limits<int64_t>::max();
  }
  return static_cast<int64_t>(std::stoll(tokens[0]));
}

std::optional<int64_t> Fs::readMemhightmpAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemHighTmpFile))) {
    return readMemhightmpFromLines(lines.value());
  }
  return std::nullopt;
}

std::optional<int64_t> Fs::readMemminAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemMinFile))) {
    return Fs::readMinMaxLowHighFromLines(lines.value());
  }
  return std::nullopt;
}

std::optional<int64_t> Fs::readSwapCurrentAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemSwapCurrentFile))) {
    // The swap controller can be disabled via CONFIG_MEMCG_SWAP=n
    return lines.value().size() == 1
        ? static_cast<int64_t>(std::stoll(lines.value()[0]))
        : 0;
  }
  return std::nullopt;
}

std::unordered_map<std::string, int64_t> Fs::getVmstat(
    const std::string& path) {
  auto lines = readFileByLine(path);
  std::unordered_map<std::string, int64_t> map;
  char space{' '};

  for (auto& line : lines.value_or(std::vector<std::string>{})) {
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
  uint64_t val;
  std::unordered_map<std::string, int64_t> map;

  auto lines = readFileByLine(path);
  for (auto& line : lines.value_or(std::vector<std::string>{})) {
    int ret =
        sscanf(line.c_str(), "%255[^:]:%*[ \t]%" SCNu64 "%*s\n", name, &val);
    if (ret == 2) {
      map[name] = val * 1024;
    }
  }

  return map;
}

std::unordered_map<std::string, int64_t> Fs::getMemstatLikeFromLines(
    const std::vector<std::string>& lines) {
  char name[256] = {0};
  uint64_t val;
  std::unordered_map<std::string, int64_t> map;

  for (const auto& line : lines) {
    int ret = sscanf(line.c_str(), "%255s %" SCNu64 "\n", name, &val);
    if (ret == 2) {
      map[name] = val;
    }
  }

  return map;
}

std::optional<std::unordered_map<std::string, int64_t>> Fs::getMemstatAt(
    const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemStatFile))) {
    return getMemstatLikeFromLines(lines.value());
  }
  return std::nullopt;
}

std::optional<ResourcePressure> Fs::readRootIopressure(PressureType type) {
  if (auto lines = readFileByLine("/proc/pressure/io")) {
    return readRespressureFromLines(lines.value(), type);
  }
  return std::nullopt;
}

std::optional<ResourcePressure> Fs::readIopressureAt(
    const DirFd& dirfd,
    PressureType type) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kIoPressureFile))) {
    return readRespressureFromLines(lines.value(), type);
  }
  return std::nullopt;
}

std::optional<IOStat> Fs::readIostatAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kIoStatFile))) {
    std::vector<DeviceIOStat> io_stat;
    io_stat.reserve(lines.value().size());

    for (const auto& line : lines.value()) {
      // format
      //
      // 0:0 rbytes=0 wbytes=0 rios=0 wios=0 dbytes=0 dios=0
      DeviceIOStat dev_io_stat;
      int major, minor;
      int ret = sscanf(
          line.c_str(),
          "%d:%d rbytes=%" SCNd64 " wbytes=%" SCNd64 " rios=%" SCNd64
          " wios=%" SCNd64 " dbytes=%" SCNd64 " dios=%" SCNd64 "\n",
          &major,
          &minor,
          &dev_io_stat.rbytes,
          &dev_io_stat.wbytes,
          &dev_io_stat.rios,
          &dev_io_stat.wios,
          &dev_io_stat.dbytes,
          &dev_io_stat.dios);

      OCHECK_EXCEPT(ret == 8, bad_control_file("invalid format"));
      dev_io_stat.dev_id = std::to_string(major) + ":" + std::to_string(minor);
      io_stat.push_back(dev_io_stat);
    }
    return io_stat;
  }
  return std::nullopt;
}

void Fs::writeControlFileAt(
    std::optional<Fd>&& fd,
    const std::string& content) {
  char buf[1024];
  buf[0] = '\0';
  if (!fd.has_value()) {
    throw bad_control_file(
        std::string{"open failed: "} + ::strerror_r(errno, buf, sizeof(buf)));
  }
  auto ret = Util::writeFull(fd->fd(), content.c_str(), content.size());
  if (ret < 0) {
    throw bad_control_file(
        std::string{"write failed: "} + ::strerror_r(errno, buf, sizeof(buf)));
  }
}

void Fs::writeMemhighAt(const DirFd& dirfd, int64_t value) {
  auto val_str = std::to_string(value);
  writeControlFileAt(Fs::Fd::openat(dirfd, kMemHighFile, false), val_str);
}

void Fs::writeMemhightmpAt(
    const DirFd& dirfd,
    int64_t value,
    std::chrono::microseconds duration) {
  auto val_str = std::to_string(value) + " " + std::to_string(duration.count());
  writeControlFileAt(Fs::Fd::openat(dirfd, kMemHighTmpFile, false), val_str);
}

std::optional<int64_t> Fs::getNrDyingDescendantsAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kCgroupStatFile))) {
    auto map = getMemstatLikeFromLines(lines.value());
    // Will return 0 for missing entries
    return map["nr_dying_descendants"];
  }
  return std::nullopt;
}

KillPreference Fs::readKillPreferenceAt(const DirFd& path) {
  if (Fs::hasxattrAt(path, kOomdPreferXAttr)) {
    return KillPreference::PREFER;
  } else if (Fs::hasxattrAt(path, kOomdAvoidXAttr)) {
    return KillPreference::AVOID;
  } else {
    return KillPreference::NORMAL;
  }
}

std::optional<bool> Fs::readMemoryOomGroupAt(const DirFd& dirfd) {
  if (auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemOomGroupFile))) {
    return lines.value() == std::vector<std::string>({"1"});
  }
  return std::nullopt;
}

bool Fs::setxattr(
    const std::string& path,
    const std::string& attr,
    const std::string& val) {
  int ret = ::setxattr(path.c_str(), attr.c_str(), val.c_str(), val.size(), 0);
  if (ret == -1) {
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

bool Fs::hasxattrAt(const DirFd& dirfd, const std::string& attr) {
  return ::fgetxattr(dirfd.fd(), attr.c_str(), nullptr, 0) >= 0;
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
  if (auto lines = readFileByLine(path)) {
    for (auto& line : lines.value()) {
      auto parts = Util::split(line, ' ');
      if (parts.size() > 2) {
        if (parts[2] == "cgroup2") {
          return parts[1] + '/';
        }
      }
    }
  }
  return "";
}

DeviceType Fs::getDeviceType(
    const std::string& dev_id,
    const std::string& path) {
  const auto deviceTypeFile =
      path + "/" + dev_id + "/" + kDeviceTypeDir + "/" + kDeviceTypeFile;
  if (auto lines = readFileByLine(deviceTypeFile)) {
    if (lines.value().size() == 1) {
      if (lines.value()[0] == "1") {
        return DeviceType::HDD;
      } else if (lines.value()[0] == "0") {
        return DeviceType::SSD;
      }
    }
  }
  throw bad_control_file(deviceTypeFile + ": invalid format");
}

} // namespace Oomd
