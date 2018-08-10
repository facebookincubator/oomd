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

#include <algorithm>
#include <condition_variable>
#include <iostream>
#include <mutex>
#include <sstream>
#include <thread>
#include <vector>

namespace Oomd {

class LogStream {
 public:
  LogStream() = default;
  ~LogStream();

  template <typename T>
  LogStream& operator<<(const T& v) {
    stream_ << v;
    return *this;
  }

 private:
  std::ostringstream stream_;
};

class Log {
 public:
  Log(const Log& other) = delete;
  Log& operator=(const Log& other) = delete;
  ~Log();
  static void init_or_die();
  static Log& get(int kmsg_fd = -1, std::ostream& debug_sink = std::cerr);
  static std::unique_ptr<Log> get_for_unittest(
      int kmsg_fd,
      std::ostream& debug_sink);

  void kmsgLog(const std::string& buf, const std::string& prefix) const;
  void debugLog(std::string&& buf);

 private:
  struct AsyncLogState {
    // The IO thread processes one queue, calling thread writes to other
    std::array<std::vector<std::string>, 2> queues;

    // Last bit is used to choose queue to process
    uint64_t ioTick{0};

    // Notifies the I/O thread to stop
    bool ioThreadRunning{true};

    // We only store maxSize bytes before we start dropping messages
    size_t curSize{0};
    const size_t maxSize{1024 * 1024};
    size_t numDiscarded{0};

    std::condition_variable cv;
    std::mutex lock;

    std::vector<std::string>* getCurrentQueue() {
      return &queues[ioTick & 0x01];
    }
  };

  // only get() is allowed to construct
  explicit Log(int kmsg_fd, std::ostream& debug_sink);
  void ioThread(std::ostream& debug_sink);

  int kmsg_fd_{-1};
  std::thread io_thread_;
  AsyncLogState state_;
};

template <typename... Args>
static void OOMD_KMSG_LOG(Args&&... args) {
  Log::get().kmsgLog(std::forward<Args>(args)...);
}

// This has to be a macro so __FILE__ and __LINE__ are captured
#define OLOG ::Oomd::LogStream() << "[" << __FILE__ << ":" << __LINE__ << "] "

} // namespace Oomd
