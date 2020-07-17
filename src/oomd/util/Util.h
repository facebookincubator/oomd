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

#include <algorithm>
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
  /*
   * Parsing rules:
   *
   * "1.5G"    : 1.5 gigabytes, outputs 1610612736 bytes
   * "1G 128M" : 1 gigabyte and 128 megabytes, outputs 1207959552 bytes
   * "4K 2048" : 4 kilobytes and 2048 bytes, outputs 6144 bytes
   *
   * Supports [kmgt] suffixes.
   *
   * @returns 0 on success, -1 on failure. Parsed output is passed out
   * through @param output
   */
  static int parseSize(const std::string& input, int64_t* output);

  /*
   * Handles the following 3 cases and returns the first valid result:
   *
   * 1) `<int_val>%` and returns the % of @param total
   * 2) `<int_val>` and returns the value in bytes (parsed as megabytes)
   * 3) `<int_val>suffix` and acts the same as @function parseSize
   *
   * @returns 0 on success, -1 on failure. Parsed output is passed out
   * through @param output
   */
  static int
  parseSizeOrPercent(const std::string& input, int64_t* output, int64_t total);

  /* Split string into tokens by delim */
  static std::vector<std::string> split(const std::string& line, char delim);

  static bool startsWith(
      const std::string& prefix,
      const std::string& to_search);

  /* Trim spaces from a string */
  static void trim(std::string& s);

  template <class T, class Functor>
  static std::vector<T> filter(std::vector<T> elems, Functor&& fn) {
    std::vector<T> ret;
    std::copy_if(
        elems.begin(),
        elems.end(),
        std::back_inserter(ret),
        std::forward<Functor>(fn));
    return ret;
  }

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

  /*
   * Returned uuids are random 32 char hex strings
   */
  static std::string generateUuid();

  static std::string strerror_r();
};

} // namespace Oomd
