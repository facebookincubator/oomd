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

class LogBase {
 public:
  virtual ~LogBase() = default;
  virtual void kmsgLog(const std::string& buf, const std::string& prefix)
      const = 0;
  virtual void debugLog(std::string&& buf) = 0;
};

class Log : public LogBase {
 public:
  Log(const Log& other) = delete;
  Log& operator=(const Log& other) = delete;
  ~Log() override;
  static bool init(const std::string& kmsg_path);
  static Log&
  get(int kmsg_fd = -1, std::ostream& debug_sink = std::cerr, bool inl = true);
  static std::unique_ptr<Log>
  get_for_unittest(int kmsg_fd, std::ostream& debug_sink, bool inl);

  void kmsgLog(const std::string& buf, const std::string& prefix)
      const override;
  void debugLog(std::string&& buf) override;

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
  explicit Log(int kmsg_fd, std::ostream& debug_sink, bool inl);
  void ioThread(std::ostream& debug_sink);

  int kmsg_fd_{-1};
  bool inline_{true};
  std::thread io_thread_;
  AsyncLogState state_;
};

class LogStream {
 public:
  enum class Control {
    DISABLE,
    ENABLE,
  };

  LogStream();
  explicit LogStream(LogBase& sink);
  ~LogStream();

  template <typename T>
  LogStream& operator<<(const T& v) {
    // If we've previously received a control token to enable the logs,
    // skip_ should be set b/c we didn't want to print an empty log line.
    // However, in the event that a user follows the control token with
    // actual log messages, we should make sure to process the entire log.
    //
    // Eg
    //    OLOG << LogStream::Control::ENABLE << "we should print this";
    if (skip_) {
      skip_ = false;
    }

    // Don't store logs unless we're enabled. This helps with interleaved
    // disabled and enabled states. Eg
    //    OLOG << LogStream::Control::DISABLE << "blah"
    //      << LogStream::Control::ENABLE << "asdf";
    //
    // Only "asdf" should be printed.
    //
    // Without this check, we'd get "blahasdf".
    if (enabled()) {
      stream_ << v;
    }

    return *this;
  }

 private:
  static bool& enabled();
  // Set this flag to skip processing this log message
  bool skip_{false};
  std::ostringstream stream_;
  LogBase& sink_;
};

// Must declare explicit specialization in *namespace* scope (class scope
// doesn't count) for some weird reason according to the C++ spec.
template <>
LogStream& LogStream::operator<<<LogStream::Control>(const Control& ctrl);

template <typename... Args>
static void OOMD_KMSG_LOG(Args&&... args) {
  Log::get().kmsgLog(std::forward<Args>(args)...);
}

#ifdef __FILE_NAME__
#define FILENAME __FILE_NAME__
#elif defined __BASE_NAME__
#define FILENAME __BASE_NAME__
#else
#define FILENAME __FILE__
#endif

// This has to be a macro so __FILE__ and __LINE__ are captured
#define OLOG ::Oomd::LogStream() << "[" << FILENAME << ":" << __LINE__ << "] "

} // namespace Oomd
