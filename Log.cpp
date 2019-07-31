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

#include "oomd/Log.h"

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <system_error>

#include "oomd/util/Util.h"

namespace Oomd {

bool& LogStream::enabled() {
  static thread_local bool enabled = true;
  return enabled;
}

LogStream::~LogStream() {
  if (!enabled() || skip_) {
    return;
  }

  stream_ << std::endl;
  Log::get().debugLog(stream_.str());
}

Log::Log(int kmsg_fd, std::ostream& debug_sink, bool inl)
    : kmsg_fd_(kmsg_fd), inline_(inl) {
  // Start async debug log flushing thread if we are not inline logging
  if (!inline_) {
    io_thread_ =
        std::thread([this, &debug_sink] { this->ioThread(debug_sink); });
  }
}

Log::~Log() {
  if (kmsg_fd_ >= 0) {
    ::close(kmsg_fd_);
  }

  {
    std::lock_guard<std::mutex> lock(state_.lock);
    state_.ioThreadRunning = false;
  }
  state_.cv.notify_all();
  if (io_thread_.joinable()) {
    io_thread_.join();
  }
}

bool Log::init() {
  int kmsg_fd = ::open("/dev/kmsg", O_WRONLY);
  if (kmsg_fd < 0) {
    perror("open");
    std::cerr << "Unable to open outfile (default=/dev/kmsg), not logging\n";
    return false;
  }

  bool inline_logging = std::getenv("INLINE_LOGGING") ? true : false;
  Log::get(kmsg_fd, std::cerr, inline_logging);
  return true;
}

Log& Log::get(int kmsg_fd, std::ostream& debug_sink, bool inl) {
  static Log singleton(kmsg_fd, debug_sink, inl);
  return singleton;
}

std::unique_ptr<Log>
Log::get_for_unittest(int kmsg_fd, std::ostream& debug_sink, bool inl) {
  return std::unique_ptr<Log>(new Log(kmsg_fd, debug_sink, inl));
}

void Log::kmsgLog(const std::string& buf, const std::string& prefix) const {
  if (kmsg_fd_ >= 0) {
    std::string message(buf);
    if (prefix.size() > 0) {
      message.insert(0, prefix + ": ");
    }
    auto ret = Util::writeFull(kmsg_fd_, message.data(), message.size());
    if (ret == -1) {
      perror("error writing");
      OLOG << "Unable to write log to output file";
    }
  } else {
    OLOG << "kmsg logging disabled b/c kmsg_fd_=" << kmsg_fd_;
  }

  OLOG << buf;
}

void Log::debugLog(std::string&& buf) {
  // If we are doing inline logging, we don't want to pass the buffer to the
  // async queue
  if (inline_) {
    std::cerr << "(inl) " << buf;
    return;
  }

  std::unique_lock<std::mutex> lock(state_.lock);

  if (buf.size() + state_.curSize > state_.maxSize) {
    state_.numDiscarded++;
    return;
  }

  auto* q = state_.getCurrentQueue();
  q->emplace_back(std::move(buf));
  state_.curSize += buf.size();
  state_.cv.notify_one();
}

void Log::ioThread(std::ostream& debug_sink) {
  bool io_thread_running = true;

  while (io_thread_running) {
    std::vector<std::string>* q = nullptr;
    size_t numDiscarded;

    // Swap the rx/tx queues
    {
      std::unique_lock<std::mutex> lock(state_.lock);
      q = state_.getCurrentQueue();

      // Wait until we have stuff to write
      state_.cv.wait(
          lock, [this, q] { return !state_.ioThreadRunning || q->size(); });

      io_thread_running = state_.ioThreadRunning;
      numDiscarded = state_.numDiscarded;

      state_.curSize = 0;
      state_.numDiscarded = 0;
      state_.ioTick++; // flips the last bit that getCurrentQueue uses
    }

    for (auto& buf : *q) {
      debug_sink << buf;
    }

    if (numDiscarded) {
      debug_sink << "...\n" << numDiscarded << " messages dropped\n...\n";
    }

    debug_sink << std::flush;

    // clear() doesn't shrink capacity, only invalidates contents
    q->clear();
  }
}

template <>
LogStream& LogStream::operator<<<LogStream::Control>(const Control& ctrl) {
  switch (ctrl) {
    case Control::DISABLE:
      enabled() = false;
      break;
    case Control::ENABLE:
      skip_ = true;
      enabled() = true;
      break;

      // Missing default to protect against future enum vals
  }

  return *this;
}
} // namespace Oomd
