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

#include <json/reader.h>
#include <json/value.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#include <chrono>
#include <cstring>
#include <iostream>

#include "oomd/Stats.h"
#include "oomd/StatsClient.h"
#include "oomd/include/Assert.h"
#include "oomd/util/ScopeGuard.h"
#include "oomd/util/Util.h"

namespace Oomd {

Stats::Stats(const std::string& stats_socket_path)
    : stats_socket_path_(stats_socket_path) {
  if (!this->startSocket()) {
    throw std::runtime_error("Socket thread failed to start");
  }
}

Stats::~Stats() {
  std::array<char, 64> err_buf = {};
  statsThreadRunning_ = false;
  auto client = StatsClient(stats_socket_path_);
  client.closeSocket();
  std::unique_lock<std::mutex> lock(thread_mutex_);
  if (!thread_exited_.wait_for(lock, std::chrono::seconds(5), [this] {
        return this->thread_count_ == 0;
      })) {
    OLOG << "Closing stats error: timed out waiting for a thread";
    // Crash here because client threads _must_ timeout in < 5s.
    // If we don't crash then we risk the client threads accessing destructed
    // members. Note this should never happen.
    OCHECK(false);
  }
  lock.unlock();
  if (stats_thread_.joinable()) {
    stats_thread_.join();
  }
  if (::unlink(serv_addr_.sun_path) < 0) {
    OLOG << "Closing stats error: unlinking socket path: "
         << ::strerror_r(errno, err_buf.data(), err_buf.size());
  }
  if (::close(sockfd_) < 0) {
    OLOG << "Closing stats error: closing stats socket: "
         << ::strerror_r(errno, err_buf.data(), err_buf.size());
  }
}

Stats& Stats::get(const std::string& stats_socket_path) {
  static Stats singleton(stats_socket_path);
  return singleton;
}

std::unique_ptr<Stats> Stats::get_for_unittest(
    const std::string& stats_socket_path) {
  return std::unique_ptr<Stats>(new Stats(stats_socket_path));
}

bool Stats::init(const std::string& stats_socket_path) {
  try {
    Stats::get(stats_socket_path);
  } catch (const std::runtime_error& e) {
    OLOG << "Initializing singleton failed: " << e.what();
    return false;
  }

  isInitInternal() = true;
  return true;
}

bool Stats::isInit() {
  return isInitInternal();
}

bool& Stats::isInitInternal() {
  static bool init = false;
  return init;
}

bool Stats::startSocket() {
  std::array<char, 64> err_buf = {};

  sockfd_ = ::socket(AF_UNIX, SOCK_STREAM, 0);
  if (sockfd_ < 0) {
    OLOG << "Error creating socket: "
         << ::strerror_r(errno, err_buf.data(), err_buf.size() - 1);
    return false;
  }
  ::memset(&serv_addr_, '\0', sizeof(serv_addr_));
  serv_addr_.sun_family = AF_UNIX;
  ::strcpy(serv_addr_.sun_path, stats_socket_path_.c_str());
  if (::unlink(serv_addr_.sun_path) < 0 && errno != ENOENT) {
    OLOG << "Pre-unlinking of socket path failed. " << serv_addr_.sun_path
         << ". Errno: " << ::strerror_r(errno, err_buf.data(), err_buf.size());
    return false;
  }
  if (::bind(sockfd_, (struct sockaddr*)&serv_addr_, sizeof(serv_addr_)) < 0) {
    OLOG << "Error binding stats collection socket: "
         << ::strerror_r(errno, err_buf.data(), err_buf.size());
    return false;
  }
  if (::chmod(stats_socket_path_.c_str(), 0666) < 0) {
    OLOG << "Unable to set permissions on " << stats_socket_path_;
    return false;
  }
  if (::listen(sockfd_, 5) < 0) {
    OLOG << "Error listening at socket: "
         << ::strerror_r(errno, err_buf.data(), err_buf.size());
    return false;
  }
  stats_thread_ = std::thread([this] { this->runSocket(); });
  return true;
}

void Stats::runSocket() {
  sockaddr_un cli_addr;
  socklen_t clilen = sizeof(cli_addr);
  std::array<char, 64> err_buf = {};
  while (statsThreadRunning_) {
    int sockfd = ::accept(sockfd_, (struct sockaddr*)&cli_addr, &clilen);
    if (sockfd < 0) {
      OLOG << "Stats server error: accepting connection: "
           << ::strerror_r(errno, err_buf.data(), err_buf.size());
      continue;
    }
    const timeval io_timeout{.tv_sec = 2, .tv_usec = 0};
    const void* time_ptr = static_cast<const void*>(&io_timeout);
    ::setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, time_ptr, sizeof io_timeout);
    ::setsockopt(sockfd, SOL_SOCKET, SO_SNDTIMEO, time_ptr, sizeof io_timeout);
    std::unique_lock<std::mutex> lock(thread_mutex_);
    ++thread_count_;
    std::thread msg_thread_ =
        std::thread([this, sockfd] { this->processMsg(sockfd); });
    msg_thread_.detach();
    lock.unlock();
    thread_exited_.notify_one();
  }
}

void Stats::processMsg(int sockfd) {
  std::array<char, 64> err_buf = {};
  OOMD_SCOPE_EXIT {
    if (::close(sockfd) < 0) {
      OLOG << "Stats server error: closing file descriptor: "
           << ::strerror_r(errno, err_buf.data(), err_buf.size());
    }
  };
  char mode = 'a';
  char byte_buf;
  int num_read = 0;
  for (; num_read < 32; num_read++) {
    int res = ::read(sockfd, &byte_buf, 1);
    if (res < 0) { // Error reading
      OLOG << "Stats server error: reading from socket: "
           << ::strerror_r(errno, err_buf.data(), err_buf.size());
      return;
    } else if (res == 0) { // EOF reached
      break;
    }
    // We read a char
    if (byte_buf == '\n' || byte_buf == '\0') {
      break;
    }
    if (num_read == 0) { // We only care about the first char
      mode = byte_buf;
    }
  }

  if (num_read == 0) {
    OLOG << "Stats server error: no msg received";
  }

  Json::Value root;
  root["error"] = 0;
  Json::Value body(Json::objectValue);
  switch (mode) {
    case 'g':
      for (auto const& pair : getAll()) {
        body[pair.first] = pair.second;
      }
      break;
    case 'r':
      Stats::reset();
      break;
    case '0':
      break;
    default:
      root["error"] = 1;
      OLOG << "Stats server error: received unknown request: " << mode;
  }
  root["body"] = body;
  std::string ret = root.toStyledString();
  if (Util::writeFull(sockfd, ret.c_str(), strlen(ret.c_str())) < 0) {
    OLOG << "Stats server error: writing to socket: "
         << ::strerror_r(errno, err_buf.data(), err_buf.size());
  }
  std::unique_lock<std::mutex> lock(thread_mutex_);
  thread_count_--;
  lock.unlock();
  thread_exited_.notify_one();
}

std::unordered_map<std::string, int> Stats::getAll() {
  std::lock_guard<std::mutex> lock(stats_mutex_);
  return stats_;
}

int Stats::increment(const std::string& key, int val) {
  std::lock_guard<std::mutex> lock(stats_mutex_);
  stats_[key] = stats_[key] + val;
  return 0;
}

int Stats::set(const std::string& key, int val) {
  std::lock_guard<std::mutex> lock(stats_mutex_);
  stats_[key] = val;
  return 0;
}

int Stats::reset() {
  std::lock_guard<std::mutex> lock(stats_mutex_);
  for (const auto& pair : stats_) {
    stats_[pair.first] = 0;
  }
  return 0;
}

std::unordered_map<std::string, int> getStats() {
  if (!Stats::isInit()) {
    OLOG << "Warning: stats module not initialized";
    return {};
  }

  return Stats::get().getAll();
}

int incrementStat(const std::string& key, int val) {
  if (!Stats::isInit()) {
    OLOG << "Warning: stats module not initialized";
    return 1;
  }

  return Stats::get().increment(key, val);
}

int setStat(const std::string& key, int val) {
  if (!Stats::isInit()) {
    OLOG << "Warning: stats module not initialized";
    return 1;
  }

  return Stats::get().set(key, val);
}

int resetStats() {
  if (!Stats::isInit()) {
    OLOG << "Warning: stats module not initialized";
    return 1;
  }

  return Stats::get().reset();
}

} // namespace Oomd
