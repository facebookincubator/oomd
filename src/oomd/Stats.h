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
#include <atomic>
#include <unordered_map>
#include "oomd/Log.h"

namespace Oomd {

class Stats {
 public:
  Stats(const Stats& other) = delete;
  Stats& operator=(const Stats& other) = delete;
  ~Stats();

  /*
   * Initializes stats singleton
   */
  static bool init(const std::string& stats_socket_path);
  static bool isInit();

  /*
   * Returns stats singleton
   */
  static Stats& get(const std::string& stats_socket_path = "");

  /*
   * Returns ptr to Stats object for unit tests
   */
  static std::unique_ptr<Stats> get_for_unittest(
      const std::string& stats_socket_path);

  /*
   * Returns a copy of current stats
   */
  std::unordered_map<std::string, int> getAll();

  /*
   * Increments designated key in stats by passed val.
   * If key is not present, initializes key to val.
   */
  int increment(const std::string& key, int val);

  /*
   * Sets designated key to equal val in stats collection
   */
  int set(const std::string& key, int val);

  /*
   * Sets all existing values in stats to 0
   */
  int reset();

 private:
  static bool& isInitInternal();

  /*
   * Ensure only get() can construct
   */
  explicit Stats(const std::string& stats_socket_path);

  // Opens socket for incoming stats queries
  bool startSocket();

  /*
   * Accepts new connections to socket and delegates
   * created fds to processMsg()
   */
  void runSocket();

  /*
   * Processes a msg on a given fd
   */
  void processMsg(int sockfd);

  // Notifies the stats socket thread to stop
  std::atomic<bool> statsThreadRunning_{true};
  std::mutex stats_mutex_;
  std::string stats_socket_path_;
  sockaddr_un serv_addr_;
  int sockfd_;
  std::unordered_map<std::string, int> stats_;
  std::thread stats_thread_;
  std::mutex thread_mutex_;
  std::atomic<int> thread_count_{0};
  std::condition_variable thread_exited_;
};

/*
 * Public API
 */
std::unordered_map<std::string, int> getStats();
int incrementStat(const std::string& key, int val);
int setStat(const std::string& key, int val);
int resetStats();

} // namespace Oomd
