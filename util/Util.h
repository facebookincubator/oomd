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

/*
 * Class to hold various utility functions.
 *
 * NB: This file *cannot* take any dependency other than standard library
 * otherwise you will cause circular dependencies elsewhere in the codebase.
 */
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

  /*
   * Read and write helpers
   *
   * Note that this is not an atomic operation. Multiple syscalls may be
   * issued to complete this request, and if any syscall fails, the API
   * will return -1.
   *
   * @returns @param count on success, -1 on failure. Will never return
   * any values other than those two.
   */
  static ssize_t readFull(int fd, char* msg_buf, size_t count);
  static ssize_t writeFull(int fd, const char* msg_buf, size_t count);
};

} // namespace Oomd
