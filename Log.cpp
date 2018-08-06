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
#include <iomanip>
#include <iostream>
#include <system_error>

#include <folly/logging/xlog.h>

#include "oomd/shared/OomdContext.h"
#include "oomd/util/Fs.h"

namespace Oomd {

LogStream::~LogStream() {
  stream_ << '\n';
  Log::get().debugLog(stream_.str());
}

Log::Log(int kmsg_fd, std::ostream& debug_sink) : kmsg_fd_(kmsg_fd) {
  // Start async debug log flushing thread
  io_thread_ = std::thread([this, &debug_sink] { this->ioThread(debug_sink); });
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

void Log::init_or_die() {
  int kmsg_fd = ::open("/dev/kmsg", O_WRONLY);
  if (kmsg_fd < 0) {
    const int errcode = errno; // prevent errno clobbering
    perror("open");
    XLOG(ERR) << "Unable to open outfile (default=/dev/kmsg), not logging";
    throw std::system_error(errcode, std::generic_category());
  }

  Log::get(kmsg_fd);
}

Log& Log::get(int kmsg_fd, std::ostream& debug_sink) {
  static Log singleton(kmsg_fd, debug_sink);
  return singleton;
}

std::unique_ptr<Log> Log::get_for_unittest(
    int kmsg_fd,
    std::ostream& debug_sink) {
  return std::unique_ptr<Log>(new Log(kmsg_fd, debug_sink));
}

void Log::kmsgLog(const std::string& buf, const std::string& prefix) const {
  if (kmsg_fd_ >= 0) {
    std::string message(buf);
    if (prefix.size() > 0) {
      message.insert(0, prefix + ": ");
    }
    auto ret = Fs::writeFull(kmsg_fd_, message.data(), message.size());
    if (ret == -1) {
      perror("error writing");
      XLOG(ERR) << "Unable to write log to output file";
    }
  }

  XLOG(INFO) << buf;
}

void Log::kmsgLog(
    const std::string& to_kill,
    const std::string& kill_type,
    const CgroupContext& context,
    const OomContext& oom_context,
    bool dry) const {
  // Parse out OOM detection context
  std::ostringstream octx_oss;
  switch (oom_context.type) {
    case OomType::SWAP:
      octx_oss << "swap," << oom_context.stat.swap_free << "MB";
      break;
    case OomType::PRESSURE_10:
      octx_oss << "pressure10," << oom_context.stat.pressure_10_duration << "s";
      break;
    case OomType::PRESSURE_60:
      octx_oss << "pressure60";
      break;
    case OomType::KILL_LIST:
      octx_oss << "killlist";
      break;
    default:
      octx_oss << "none";
      break;
  }

  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << context.pressure.sec_10 << " " << context.pressure.sec_60 << " "
      << context.pressure.sec_600 << " " << to_kill << " "
      << context.current_usage << " "
      << "detector:" << octx_oss.str() << " "
      << "killer:" << (dry ? "(dry)" : "") << kill_type;
  kmsgLog(oss.str(), "oomd kill");
}

void Log::debugLog(std::string&& buf) {
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

    // clear() doesn't shrink capacity, only invalidates contents
    q->clear();
  }
}

} // namespace Oomd
