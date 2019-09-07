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

#pragma once

#include <sys/un.h>
#include <unordered_map>

namespace Oomd {
class StatsClient {
 public:
  explicit StatsClient(const std::string& stats_socket_path);

  /*
   * Connect to socket and return the current stats
   */
  std::optional<std::unordered_map<std::string, int>> getStats();

  /*
   * Connect to socket and reset the current stats
   */
  int resetStats();

  /*
   * Connect to socket and send a closing message
   */
  int closeSocket();

  /*
   * Send a message to the socket. Appends a \n delimiter to the message
   * before sending. Returns the the socket's response.
   */
  std::optional<std::string> msgSocket(std::string msg);

 private:
  std::string stats_socket_path_;
  sockaddr_un serv_addr_;
};

} // namespace Oomd
