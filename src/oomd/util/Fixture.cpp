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

#include <fcntl.h>
#include <ftw.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <array>
#include <cstdlib>
#include <stdexcept>

#include "oomd/util/Fixture.h"
#include "oomd/util/Util.h"

constexpr auto kFixturesDirTemplate = "__oomd_fixtures_XXXXXX";

namespace Oomd {

// static
Fixture::DirEntryPair Fixture::makeFile(
    const std::string& name,
    const std::string& content) {
  return {name,
          DirEntry([content](const std::string& path, const std::string& name) {
            writeChecked(path + "/" + name, content);
          })};
}

// static
Fixture::DirEntryPair Fixture::makeDir(
    const std::string& name,
    std::unordered_map<std::string, DirEntry> entries) {
  return {name,
          DirEntry([entries](const std::string& path, const std::string& name) {
            mkdirsChecked(name, path);
            const auto newPath = path + "/" + name;
            for (const auto& kv : entries) {
              kv.second.materialize(newPath, kv.first);
            }
          })};
}

const char* getTempDir() {
  if (std::getenv("TMPDIR") != nullptr) {
    return std::getenv("TMPDIR");
  } else if (std::getenv("TMP") != nullptr) {
    return std::getenv("TMP");
  } else if (std::getenv("TEMP") != nullptr) {
    return std::getenv("TEMP");
  } else if (std::getenv("TEMPDIR") != nullptr) {
    return std::getenv("TEMPDIR");
  } else {
    return "/tmp";
  }
}

// static
std::string Fixture::mkdtempChecked() {
  std::array<char, 1024> temp;
  ::snprintf(
      temp.data(), temp.size(), "%s/%s", getTempDir(), kFixturesDirTemplate);

  if (::mkdtemp(temp.data()) == nullptr) {
    std::array<char, 1024> buf;
    buf[0] = '\0';
    throw std::runtime_error(
        std::string(temp.data()) +
        ": mkdtemp failed: " + ::strerror_r(errno, buf.data(), buf.size()));
  }
  return temp.data();
}

// static
void Fixture::mkdirsChecked(
    const std::string& path,
    const std::string& prefix) {
  auto dirs = Util::split(path, '/');
  std::string prefix_path;
  // two slashes after each component
  prefix_path.reserve(path.size() + prefix.size() + 2);
  if (path.size() && path[0] == '/') {
    prefix_path += "/";
  } else if (prefix != "") {
    prefix_path += prefix + "/";
  }
  for (const auto& dir : dirs) {
    if (dir == "") {
      continue;
    }
    prefix_path += dir + "/";
    if (::mkdir(prefix_path.c_str(), 0777) == -1 && errno != EEXIST) {
      std::array<char, 1024> buf;
      buf[0] = '\0';
      throw std::runtime_error(
          prefix_path +
          ": mkdir failed: " + ::strerror_r(errno, buf.data(), buf.size()));
    }
  }
}

// static
void Fixture::writeChecked(
    const std::string& path,
    const std::string& content) {
  std::array<char, 1024> buf;
  buf[0] = '\0';
  auto fd = ::open(path.c_str(), O_CREAT | O_WRONLY | O_TRUNC, 0666);
  if (fd < 0) {
    throw std::runtime_error(
        path + ": open failed: " + ::strerror_r(errno, buf.data(), buf.size()));
  }
  auto ret = Util::writeFull(fd, content.c_str(), content.size());
  ::close(fd);
  if (ret < 0) {
    throw std::runtime_error(
        path +
        ": write failed: " + ::strerror_r(errno, buf.data(), buf.size()));
  }
  if ((size_t)ret != content.size()) {
    throw std::runtime_error(
        path + ": write failed: not all bytes are written");
  }
}

int rm(const char* path, const struct stat*, int, struct FTW*) {
  return ::remove(path);
}

// static
void Fixture::rmrChecked(const std::string& path) {
  std::array<char, 1024> buf;
  buf[0] = '\0';
  if (::nftw(path.c_str(), rm, 10, FTW_DEPTH | FTW_PHYS) == -1) {
    switch (errno) {
      case ENOENT:
        return;
      default:
        throw std::runtime_error(
            path +
            ": remove failed: " + ::strerror_r(errno, buf.data(), buf.size()));
    }
  }
}

} // namespace Oomd
