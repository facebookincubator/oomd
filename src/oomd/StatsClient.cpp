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

#include "oomd/StatsClient.h"
#include <json/reader.h>
#include <json/value.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <iostream>
#include <optional>
#include <vector>
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

namespace {
std::optional<Json::Value> parseJson(const std::string& input) {
  Json::Value ret;
  std::string errs;
  Json::CharReaderBuilder rbuilder;
  std::istringstream sinput(input);

  bool ok = Json::parseFromStream(rbuilder, sinput, &ret, &errs);
  if (!ok) {
    std::cerr << "Unable to parse JSON: " << errs;
    return std::nullopt;
  }

  return ret;
}
} // namespace

namespace Oomd {

StatsClient::StatsClient(const std::string& stats_socket_path)
    : stats_socket_path_(stats_socket_path) {
  serv_addr_.sun_family = AF_UNIX;
  ::strcpy(serv_addr_.sun_path, stats_socket_path_.c_str());
}

std::optional<std::unordered_map<std::string, int>> StatsClient::getStats() {
  const auto msg = msgSocket("g");
  if (!msg) {
    return std::nullopt;
  }

  auto root = parseJson(*msg);
  if (!root) {
    return std::nullopt;
  }

  try {
    if ((*root)["error"].asInt()) {
      std::cerr << "StatsClient error: received error code="
                << (*root)["error"].toStyledString() << std::endl;
      return std::nullopt;
    }
  } catch (const std::exception&) {
    std::cerr << "StatsClient error: parsed error value not an int"
              << std::endl;
    return std::nullopt;
  }
  const auto& body = (*root)["body"];
  std::unordered_map<std::string, int> ret_map;
  for (const auto& key : body.getMemberNames()) {
    ret_map[key] = body[key].asInt();
  }
  return ret_map;
}

int StatsClient::resetStats() {
  const auto msg = msgSocket("r");
  if (!msg) {
    return 1;
  }

  auto root = parseJson(*msg);
  if (!root) {
    return 1;
  }

  try {
    return (*root)["error"].asInt();
  } catch (const std::exception&) {
    std::cerr << "StatsClient error: parsed error value not an int"
              << std::endl;
    return 1;
  }
}

int StatsClient::closeSocket() {
  const auto msg = msgSocket("0");
  if (!msg) {
    return 1;
  }

  auto root = parseJson(*msg);
  if (!root) {
    return 1;
  }

  try {
    return (*root)["error"].asInt();
  } catch (const std::exception&) {
    std::cerr << "StatsClient error: parsed error value not an int"
              << std::endl;
    return 1;
  }
}

std::optional<std::string> StatsClient::msgSocket(std::string msg) {
  std::array<char, 64> err_buf = {};
  int sockfd = ::socket(AF_UNIX, SOCK_STREAM, 0);
  if (sockfd < 0) {
    std::cerr << "Error: creating client socket: "
              << ::strerror_r(errno, err_buf.data(), err_buf.size())
              << std::endl;
    return std::nullopt;
  }
  OOMD_SCOPE_EXIT {
    if (::close(sockfd) < 0) {
      std::cerr << "Error: shutting down client socket: "
                << ::strerror_r(errno, err_buf.data(), err_buf.size())
                << std::endl;
    }
  };
  const timeval io_timeout{.tv_sec = 2, .tv_usec = 0};
  const void* time_ptr = static_cast<const void*>(&io_timeout);
  ::setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, time_ptr, sizeof io_timeout);
  ::setsockopt(sockfd, SOL_SOCKET, SO_SNDTIMEO, time_ptr, sizeof io_timeout);
  if (::connect(sockfd, (struct sockaddr*)&serv_addr_, sizeof(serv_addr_)) <
      0) {
    std::cerr << "Error: connecting to stats socket: "
              << ::strerror_r(errno, err_buf.data(), err_buf.size())
              << "\nSocket path: " << serv_addr_.sun_path << std::endl;
    return std::nullopt;
  }
  msg += '\n';
  if (Util::writeFull(sockfd, msg.c_str(), strlen(msg.c_str())) < 0) {
    std::cerr << "Error: writing to stats socket: "
              << ::strerror_r(errno, err_buf.data(), err_buf.size())
              << std::endl;
    return std::nullopt;
  }
  std::string ret;
  std::array<char, 512> msg_buf = {};
  while (true) {
    int n = Util::readFull(sockfd, msg_buf.data(), msg_buf.size() - 1);
    if (n < 0) {
      std::cerr << "Error: reading from stats socket: "
                << ::strerror_r(errno, err_buf.data(), err_buf.size())
                << std::endl;
      return std::nullopt;
    } else if (n == 0) {
      break;
    }
    msg_buf[n] = '\0';
    ret += std::string(msg_buf.data());
  }
  return ret;
}

} // namespace Oomd
