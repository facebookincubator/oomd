/*
 * Copyright (C) 2019-present, Facebook, Inc.
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

#include "oomd/util/Util.h"

#include <unistd.h>

#include <algorithm>
#include <array>
#include <cstring>
#include <random>
#include <sstream>

static constexpr auto kWhitespaceChars = " \t\n\r";

namespace {
void ltrim(std::string& s) {
  s.erase(0, s.find_first_not_of(kWhitespaceChars));
}

void rtrim(std::string& s) {
  s.erase(s.find_last_not_of(kWhitespaceChars) + 1);
}

template <class F, class T>
ssize_t wrapFull(F f, int fd, T buf, size_t count) {
  ssize_t totalBytes = 0;
  ssize_t r;
  do {
    r = f(fd, buf, count);
    if (r == -1) {
      if (errno == EINTR) {
        continue;
      }
      return r;
    }

    totalBytes += r;
    buf += r;
    count -= r;
  } while (r != 0 && count); // 0 means EOF

  return totalBytes;
}
} // namespace

namespace Oomd {

int Util::parseSize(const std::string& input, int64_t* output) {
  bool is_neg = false;
  uint64_t size = 0;
  size_t pos = 0;
  auto istr = input;

  // lower case and get rid of spaces
  transform(istr.begin(), istr.end(), istr.begin(), tolower);
  auto new_end = std::remove_if(istr.begin(), istr.end(), isspace);
  istr.erase(new_end - istr.begin());

  // pop off leading sign
  if (istr[0] == '+') {
    pos++;
  } else if (istr[0] == '-') {
    is_neg = true;
    pos++;
  }

  while (pos < istr.length()) {
    size_t unit_pos;
    size_t end_pos;

    unit_pos = istr.find_first_of("kmgt", pos);
    if (unit_pos == pos) {
      return -1;
    }
    if (unit_pos == std::string::npos) {
      unit_pos = istr.length();
    }

    auto num = istr.substr(pos, unit_pos - pos);
    auto unit = istr.c_str()[unit_pos];

    double v;
    try {
      v = std::stold(num, &end_pos);
    } catch (...) {
      return -1;
    }
    if (end_pos != num.length() || v < 0) {
      return -1;
    }

    switch (unit) {
      case '\0':
        break;
      case 'k':
        v *= 1ULL << 10;
        break;
      case 'm':
        v *= 1ULL << 20;
        break;
      case 'g':
        v *= 1ULL << 30;
        break;
      case 't':
        v *= 1ULL << 40;
        break;
      default:
        return -1;
    }
    size += v;
    pos = unit_pos + 1;
  }
  *output = is_neg ? -size : size;
  return 0;
}

int Util::parseSizeOrPercent(
    const std::string& input,
    int64_t* output,
    int64_t total) {
  try {
    if (input.size() > 0 && input.at(input.size() - 1) == '%') {
      int64_t pct = std::stoi(input.substr(0, input.size() - 1));
      if (pct < 0 || pct > 100) {
        return -1;
      }

      *output = total * pct / 100;
      return 0;
    } else {
      int64_t v;
      size_t end_pos;

      // compat - a bare number is interpreted as megabytes
      v = std::stoll(input, &end_pos);
      if (end_pos == input.length()) {
        *output = v << 20;
        return 0;
      }

      if (Util::parseSize(input, &v) == 0) {
        *output = v;
        return 0;
      }

      return -1;
    }
  } catch (...) {
    return -1;
  }
}

std::vector<std::string> Util::split(const std::string& line, char delim) {
  std::vector<std::string> ret;

  auto emplaceNonEmptySubstr = [&ret, &line](size_t beg, size_t end) {
    if (size_t len = end - beg) {
      ret.emplace_back(line.begin() + beg, line.begin() + end);
    }
  };

  size_t beg = 0;
  size_t n = line.size();
  for (size_t i = 0; i < n; ++i) {
    if (line[i] == delim) {
      emplaceNonEmptySubstr(beg, i);
      beg = i + 1;
    }
  }
  emplaceNonEmptySubstr(beg, n);

  return ret;
}

bool Util::startsWith(const std::string& prefix, const std::string& to_search) {
  if (prefix.size() > to_search.size()) {
    return false;
  }

  for (size_t i = 0; i < prefix.size(); ++i) {
    if (prefix[i] != to_search[i]) {
      return false;
    }
  }

  return true;
}

void Util::trim(std::string& s) {
  rtrim(s);
  ltrim(s);
}

ssize_t Util::readFull(int fd, char* msg_buf, size_t count) {
  return wrapFull(::read, fd, msg_buf, count);
}

ssize_t Util::writeFull(int fd, const char* msg_buf, size_t count) {
  return wrapFull(::write, fd, msg_buf, count);
}

std::string Util::generateUuid() {
  static unsigned int seed = [] {
    try {
      return std::random_device{}();
    } catch (const std::runtime_error&) {
      return std::random_device("/dev/urandom")();
    }
  }();
  static std::mt19937_64 gen(seed);
  static std::uniform_int_distribution<uint64_t> dis;

  // Combine two 64-bit numbers
  std::stringstream ss;
  ss << std::hex << dis(gen) << dis(gen);

  return ss.str();
}

std::string Util::strerror_r() {
  std::array<char, 1024> buf;
  buf[0] = '\0';

  int savedErrno = errno;

  std::string ret(::strerror_r(errno, buf.data(), buf.size()));

  errno = savedErrno;
  return ret;
}

} // namespace Oomd
