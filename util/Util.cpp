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

#include <algorithm>

#include "oomd/util/Util.h"

namespace Oomd {

// "1.5G"    : 1.5 gigabytes - 1610612736 bytes
// "1G 128M" : 1 gigabyte and 128 megabytes - 1207959552 bytes
// "4K 2048" : 4 kilobytes and 2048 bytes - 6144 bytes
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

} // namespace Oomd
