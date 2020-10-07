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
#include "oomd/util/ScopeGuard.h"
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

SystemMaybe<Fs::Fd>
Fs::Fd::openat(const DirFd& dirfd, const std::string& path, bool read_only) {
  int flags = read_only ? O_RDONLY : O_WRONLY;
  const auto fd = ::openat(dirfd.fd(), path.c_str(), flags);
  if (fd == -1) {
    return SYSTEM_ERROR(errno);
  }
  return Fd(fd);
}

SystemMaybe<uint64_t> Fs::Fd::inode() const {
  struct ::stat buf;
  if (::fstat(fd_, &buf) == 0) {
    return static_cast<uint64_t>(buf.st_ino);
  }
  return SYSTEM_ERROR(errno);
}

void Fs::Fd::close() {
  if (fd_ != -1) {
    ::close(fd_);
    fd_ = -1;
  }
}

SystemMaybe<Fs::DirFd> Fs::DirFd::open(const std::string& path) {
  int fd = ::open(path.c_str(), O_RDONLY | O_DIRECTORY);
  if (fd == -1) {
    return SYSTEM_ERROR(errno);
  }
  return DirFd(fd);
}

SystemMaybe<Fs::DirFd> Fs::DirFd::openChildDir(const std::string& path) const {
  int child_fd = ::openat(fd(), path.c_str(), O_RDONLY | O_DIRECTORY);
  if (child_fd == -1) {
    return SYSTEM_ERROR(errno);
  }
  return DirFd(child_fd);
}

bool Fs::isCgroupValid(const DirFd& dirfd) {
  // If cgroup.controllers file exists, the cgroup should still be valid
  return ::faccessat(dirfd.fd(), kControllersFile, F_OK, 0) == 0;
}

SystemMaybe<Fs::DirEnts> Fs::readDirFromDIR(DIR* d, int flags) {
  Fs::DirEnts de;

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

    struct stat buf;
    int ret = ::fstatat(dirfd(d), dir->d_name, &buf, AT_SYMLINK_NOFOLLOW);
    if (ret == -1) {
      return SYSTEM_ERROR(errno);
    }

    if ((flags & DirEntFlags::DE_FILE) && (buf.st_mode & S_IFREG)) {
      de.files.push_back(dir->d_name);
    }
    if ((flags & DirEntFlags::DE_DIR) && (buf.st_mode & S_IFDIR)) {
      de.files.push_back(dir->d_name);
    }
  }

  return de;
}

SystemMaybe<Fs::DirEnts> Fs::readDirAt(const DirFd& dirfd, int flags) {
  // once an fd is passed to fdopendir, it is unusable except through that DIR
  int fresh_fd = ::dup(dirfd.fd());

  DIR* d = ::fdopendir(fresh_fd);
  if (!d) {
    auto e = errno;
    ::close(fresh_fd);
    return SYSTEM_ERROR(e);
  }

  OOMD_SCOPE_EXIT {
    // even though we dup()ed dirfd.fd(), the copied fd shares state with
    // dirfd.fd(). For dirfd to be reusable again, we need to rewind it, which
    // we can do through d.
    ::rewinddir(d);

    ::closedir(d);
  };

  return Fs::readDirFromDIR(d, flags);
}

SystemMaybe<Fs::DirEnts> Fs::readDir(const std::string& path, int flags) {
  DIR* d = ::opendir(path.c_str());
  if (!d) {
    return SYSTEM_ERROR(errno);
  }
  OOMD_SCOPE_EXIT {
    ::closedir(d);
  };
  return readDirFromDIR(d, flags);
}

bool Fs::isDir(const std::string& path) {
  struct stat sb;
  if (!::stat(path.c_str(), &sb) && S_ISDIR(sb.st_mode)) {
    return true;
  }

  return false;
}

SystemMaybe<std::vector<std::string>> Fs::glob(
    const std::string& pattern,
    bool dir_only) {
  glob_t globbuf;
  std::vector<std::string> ret;
  int flags = GLOB_NOSORT | GLOB_BRACE | GLOB_ERR;
  if (dir_only) {
    flags |= GLOB_ONLYDIR;
  }
  auto ec = ::glob(pattern.c_str(), flags, nullptr, &globbuf);
  OOMD_SCOPE_EXIT {
    ::globfree(&globbuf);
  };
  if (!ec) {
    for (size_t i = 0; i < globbuf.gl_pathc; i++) {
      std::string path(globbuf.gl_pathv[i]);
      // GLOB_ONLYDIR is a hint and we need to double check
      if (dir_only && !isDir(path)) {
        continue;
      }
      ret.emplace_back(std::move(path));
    }
    return ret;
  } else if (ec == GLOB_NOMATCH) {
    // Error with GLOB_NOMATCH just means nothing matched, return empty vec
    return ret;
  } else if (ec == GLOB_NOSPACE) {
    return SYSTEM_ERROR(ENOMEM);
  } else if (ec == GLOB_ABORTED) {
    return SYSTEM_ERROR(EINVAL);
  } else {
    // Shouldn't be possible, ENOSYS seems reasonable
    return SYSTEM_ERROR(ENOSYS);
  }
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
SystemMaybe<std::vector<std::string>> Fs::readFileByLine(
    const std::string& path) {
  std::ifstream f(path, std::ios::in);
  if (!f.is_open()) {
    return SYSTEM_ERROR(ENOENT);
  }

  std::string s;
  std::vector<std::string> v;
  while (std::getline(f, s)) {
    v.push_back(std::move(s));
  }

  // Error when reading file, and thus content might be corrupted
  if (f.bad()) {
    return SYSTEM_ERROR(EINVAL);
  }

  return v;
}

SystemMaybe<std::vector<std::string>> Fs::readFileByLine(Fd&& fd) {
  auto fp = ::fdopen(std::move(fd).fd(), "r");
  if (fp == nullptr) {
    return SYSTEM_ERROR(errno);
  }
  std::vector<std::string> v;
  char* line = nullptr;
  size_t len = 0;
  ssize_t read;
  errno = 0;

  OOMD_SCOPE_EXIT {
    ::free(line);
    ::fclose(fp);
  };
  while ((read = ::getline(&line, &len, fp)) != -1) {
    OCHECK(line != nullptr);
    if (read > 0 && line[read - 1] == '\n') {
      v.emplace_back(line, read - 1);
    } else {
      v.emplace_back(line);
    }
  }

  // Error when reading file, and thus content might be corrupted
  if (errno) {
    return SYSTEM_ERROR(errno);
  }
  return v;
}

SystemMaybe<std::vector<std::string>> Fs::readControllersAt(
    const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kControllersFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  return Util::split((*lines)[0], ' ');
}

SystemMaybe<std::vector<int>> Fs::getPidsAt(const DirFd& dirfd) {
  auto str_pids = readFileByLine(Fs::Fd::openat(dirfd, kProcsFile));
  if (!str_pids) {
    return SYSTEM_ERROR(str_pids.error());
  }
  std::vector<int> pids;
  for (const auto& sp : *str_pids) {
    pids.push_back(std::stoi(sp));
  }
  return pids;
}

SystemMaybe<bool> Fs::readIsPopulatedAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kEventsFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  for (const auto& line : *lines) {
    std::vector<std::string> toks = Util::split(line, ' ');
    if (toks.size() == 2 && toks[0] == "populated") {
      if (toks[1] == "1") {
        return true;
      } else if (toks[1] == "0") {
        return false;
      } else {
        return SYSTEM_ERROR(EINVAL);
      }
    }
  }

  return SYSTEM_ERROR(EINVAL);
}

std::string Fs::pressureTypeToString(PressureType type) {
  switch (type) {
    case PressureType::SOME:
      return "some";
    case PressureType::FULL:
      return "full";
  }
  __builtin_unreachable();
}

SystemMaybe<ResourcePressure> Fs::readRespressureFromLines(
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
      if (toks[0] != type_name) {
        return SYSTEM_ERROR(EINVAL);
      }
      std::vector<std::string> avg10 = Util::split(toks[1], '=');
      if (avg10[0] != "avg10") {
        return SYSTEM_ERROR(EINVAL);
      }
      std::vector<std::string> avg60 = Util::split(toks[2], '=');
      if (avg60[0] != "avg60") {
        return SYSTEM_ERROR(EINVAL);
      }
      std::vector<std::string> avg300 = Util::split(toks[3], '=');
      if (avg300[0] != "avg300") {
        return SYSTEM_ERROR(EINVAL);
      }
      std::vector<std::string> total = Util::split(toks[4], '=');
      if (total[0] != "total") {
        return SYSTEM_ERROR(EINVAL);
      }

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
      if (toks[0] != type_name) {
        return SYSTEM_ERROR(EINVAL);
      }

      return ResourcePressure{
          std::stof(toks[1]),
          std::stof(toks[2]),
          std::stof(toks[3]),
          std::nullopt,
      };
    }
    case PsiFormat::MISSING:
      // Missing the control file
      return SYSTEM_ERROR(ENOENT);
    case PsiFormat::INVALID:
      return SYSTEM_ERROR(EINVAL);
  }
  __builtin_unreachable();
}

SystemMaybe<int64_t> Fs::readRootMemcurrent() {
  auto meminfo = getMeminfo("/proc/meminfo");
  if (!meminfo) {
    return SYSTEM_ERROR(meminfo.error());
  }

  if (!meminfo->count("MemTotal") || !meminfo->count("MemFree")) {
    return SYSTEM_ERROR(EINVAL);
  }
  return (*meminfo)["MemTotal"] - (*meminfo)["MemFree"];
}

SystemMaybe<int64_t> Fs::readMemcurrentAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemCurrentFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  return static_cast<int64_t>(std::stoll((*lines)[0]));
}

SystemMaybe<ResourcePressure> Fs::readRootMempressure(PressureType type) {
  auto lines = readFileByLine("/proc/pressure/memory");
  if (!lines) {
    lines = readFileByLine("/proc/mempressure");
  }

  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  return readRespressureFromLines(*lines, type);
}

SystemMaybe<ResourcePressure> Fs::readMempressureAt(
    const DirFd& dirfd,
    PressureType type) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemPressureFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  return readRespressureFromLines(*lines, type);
}

SystemMaybe<int64_t> Fs::readMinMaxLowHighFromLines(
    const std::vector<std::string>& lines) {
  if (lines.size() != 1) {
    return SYSTEM_ERROR(EINVAL);
  }
  if (lines[0] == "max") {
    return std::numeric_limits<int64_t>::max();
  }
  return static_cast<int64_t>(std::stoll(lines[0]));
}

SystemMaybe<int64_t> Fs::readMemlowAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemLowFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  auto ret = Fs::readMinMaxLowHighFromLines(*lines);
  if (!ret) {
    return SYSTEM_ERROR(ret.error());
  }
  return ret;
}

SystemMaybe<int64_t> Fs::readMemhighAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemHighFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  auto ret = Fs::readMinMaxLowHighFromLines(*lines);
  if (!ret) {
    return SYSTEM_ERROR(ret.error());
  }
  return ret;
}

SystemMaybe<int64_t> Fs::readMemmaxAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemMaxFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  auto ret = Fs::readMinMaxLowHighFromLines(*lines);
  if (!ret) {
    return SYSTEM_ERROR(ret.error());
  }
  return ret;
}

SystemMaybe<int64_t> Fs::readMemhightmpFromLines(
    const std::vector<std::string>& lines) {
  if (lines.size() != 1) {
    return SYSTEM_ERROR(ENOENT);
  }
  auto tokens = Util::split(lines[0], ' ');
  if (tokens.size() != 2) {
    return SYSTEM_ERROR(EINVAL);
  }
  if (tokens[0] == "max") {
    return std::numeric_limits<int64_t>::max();
  }
  return static_cast<int64_t>(std::stoll(tokens[0]));
}

SystemMaybe<int64_t> Fs::readMemhightmpAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemHighTmpFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  auto ret = readMemhightmpFromLines(*lines);
  if (!ret) {
    return SYSTEM_ERROR(ret.error());
  }
  return ret;
}

SystemMaybe<int64_t> Fs::readMemminAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemMinFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  auto ret = Fs::readMinMaxLowHighFromLines(*lines);
  if (!ret) {
    return SYSTEM_ERROR(ret.error());
  }
  return ret;
}

SystemMaybe<int64_t> Fs::readSwapCurrentAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemSwapCurrentFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  // The swap controller can be disabled via CONFIG_MEMCG_SWAP=n
  return std::stoll((*lines)[0]);
}

SystemMaybe<std::unordered_map<std::string, int64_t>> Fs::getVmstat(
    const std::string& path) {
  auto lines = readFileByLine(path);
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  std::unordered_map<std::string, int64_t> map;
  char space{' '};

  for (auto& line : *lines) {
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

SystemMaybe<std::unordered_map<std::string, int64_t>> Fs::getMeminfo(
    const std::string& path) {
  char name[256] = {0};
  uint64_t val;
  std::unordered_map<std::string, int64_t> map;

  auto lines = readFileByLine(path);
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  for (auto& line : *lines) {
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

SystemMaybe<std::unordered_map<std::string, int64_t>> Fs::getMemstatAt(
    const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemStatFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }

  return getMemstatLikeFromLines(lines.value());
}

SystemMaybe<ResourcePressure> Fs::readRootIopressure(PressureType type) {
  auto lines = readFileByLine("/proc/pressure/io");
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  return readRespressureFromLines(*lines, type);
}

SystemMaybe<ResourcePressure> Fs::readIopressureAt(
    const DirFd& dirfd,
    PressureType type) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kIoPressureFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  return readRespressureFromLines(*lines, type);
}

SystemMaybe<IOStat> Fs::readIostatAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kIoStatFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }

  std::vector<DeviceIOStat> io_stat;
  io_stat.reserve(lines->size());

  for (const auto& line : *lines) {
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

    if (ret != 8) {
      return SYSTEM_ERROR(EINVAL);
    }
    dev_io_stat.dev_id = std::to_string(major) + ":" + std::to_string(minor);
    io_stat.push_back(dev_io_stat);
  }
  return io_stat;
}

SystemMaybe<Unit> Fs::writeControlFileAt(
    SystemMaybe<Fd>&& fd,
    const std::string& content) {
  if (!fd) {
    return SYSTEM_ERROR(fd.error());
  }
  auto ret = Util::writeFull(fd->fd(), content.c_str(), content.size());
  if (ret < 0) {
    return SYSTEM_ERROR(errno);
  }

  return noSystemError();
}

SystemMaybe<Unit> Fs::writeMemhighAt(const DirFd& dirfd, int64_t value) {
  auto val_str = std::to_string(value);
  auto ret =
      writeControlFileAt(Fs::Fd::openat(dirfd, kMemHighFile, false), val_str);
  if (!ret) {
    return SYSTEM_ERROR(ret.error());
  }
  return noSystemError();
}

SystemMaybe<Unit> Fs::writeMemhightmpAt(
    const DirFd& dirfd,
    int64_t value,
    std::chrono::microseconds duration) {
  auto val_str = std::to_string(value) + " " + std::to_string(duration.count());
  auto ret = writeControlFileAt(
      Fs::Fd::openat(dirfd, kMemHighTmpFile, false), val_str);
  if (!ret) {
    return SYSTEM_ERROR(ret.error());
  }
  return noSystemError();
}

SystemMaybe<int64_t> Fs::getNrDyingDescendantsAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kCgroupStatFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  auto map = getMemstatLikeFromLines(lines.value());
  // Will return 0 for missing entries
  return map["nr_dying_descendants"];
}

SystemMaybe<KillPreference> Fs::readKillPreferenceAt(const DirFd& path) {
  auto maybe = Fs::hasxattrAt(path, kOomdPreferXAttr);
  if (!maybe) {
    return SYSTEM_ERROR(maybe.error());
  }

  if (*maybe) {
    return KillPreference::PREFER;
  }

  maybe = Fs::hasxattrAt(path, kOomdAvoidXAttr);
  if (!maybe) {
    return SYSTEM_ERROR(maybe.error());
  }

  if (*maybe) {
    return KillPreference::AVOID;
  }

  return KillPreference::NORMAL;
}

SystemMaybe<bool> Fs::readMemoryOomGroupAt(const DirFd& dirfd) {
  auto lines = readFileByLine(Fs::Fd::openat(dirfd, kMemOomGroupFile));
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  return *lines == std::vector<std::string>({"1"});
}

SystemMaybe<Unit> Fs::setxattr(
    const std::string& path,
    const std::string& attr,
    const std::string& val) {
  int ret = ::setxattr(path.c_str(), attr.c_str(), val.c_str(), val.size(), 0);
  if (ret == -1) {
    return SYSTEM_ERROR(errno);
  }
  return noSystemError();
}

SystemMaybe<std::string> Fs::getxattr(
    const std::string& path,
    const std::string& attr) {
  std::string val;

  int size = ::getxattr(path.c_str(), attr.c_str(), nullptr, 0);
  if (size == -1) {
    if (errno == ENODATA) {
      return val;
    }
    return SYSTEM_ERROR(errno);
  }

  val.resize(size);
  size = ::getxattr(path.c_str(), attr.c_str(), &val[0], val.size());
  if (size == -1) {
    // We checked above but this could have raced, so check again
    if (errno == ENODATA) {
      return std::string{};
    }
    return SYSTEM_ERROR(errno);
  }
  return val;
}

SystemMaybe<bool> Fs::hasxattrAt(const DirFd& dirfd, const std::string& attr) {
  auto ret = ::fgetxattr(dirfd.fd(), attr.c_str(), nullptr, 0);
  if (ret == -1) {
    if (errno == ENODATA) {
      return false;
    }
    return SYSTEM_ERROR(errno);
  }
  return true;
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

SystemMaybe<std::string> Fs::getCgroup2MountPoint(const std::string& path) {
  auto lines = readFileByLine(path);
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  for (auto& line : *lines) {
    auto parts = Util::split(line, ' ');
    if (parts.size() > 2) {
      if (parts[2] == "cgroup2") {
        return parts[1] + '/';
      }
    }
  }
  return SYSTEM_ERROR(EINVAL, path);
}

SystemMaybe<DeviceType> Fs::getDeviceType(
    const std::string& dev_id,
    const std::string& path) {
  const auto deviceTypeFile =
      path + "/" + dev_id + "/" + kDeviceTypeDir + "/" + kDeviceTypeFile;
  auto lines = readFileByLine(deviceTypeFile);
  if (!lines) {
    return SYSTEM_ERROR(lines.error());
  }
  if (lines->size() == 1) {
    if ((*lines)[0] == "1") {
      return DeviceType::HDD;
    } else if ((*lines)[0] == "0") {
      return DeviceType::SSD;
    }
  }
  return SYSTEM_ERROR(EINVAL, deviceTypeFile);
}

} // namespace Oomd
