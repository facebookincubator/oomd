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

#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace Oomd {

class Util {
 public:
  static int parseSize(const std::string& input, int64_t* output);

  /* Split string into tokens by delim */
  static std::vector<std::string> split(const std::string& line, char delim);

  static bool startsWith(
      const std::string& prefix,
      const std::string& to_search);

  /* Trim spaces from a string */
  static void trim(std::string& s);
};

} // namespace Oomd
