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

#include <gtest/gtest.h>

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdlib.h>
#include <sys/eventfd.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/timerfd.h>
#include <unistd.h>
#include <array>
#include <atomic>
#include <chrono>
#include <fstream>
#include <memory>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

#include "oomd/Watchdog.h"

namespace Oomd {

struct WatchdogTestClock {
  std::atomic<uint64_t> now_ns{0};
};

class WatchdogTestPeer {
 public:
  static std::unique_ptr<Watchdog> create(
      std::chrono::milliseconds timeout,
      int kmsg_fd,
      const std::string& stack_path,
      WatchdogTestClock& clock) {
    return createImpl(
        timeout,
        kmsg_fd,
        stack_path,
        [&clock] { return clock.now_ns.load(); },
        &Watchdog::armTimer);
  }

  static std::unique_ptr<Watchdog> createWithRealClock(
      std::chrono::milliseconds timeout,
      int kmsg_fd,
      const std::string& stack_path) {
    return createImpl(
        timeout,
        kmsg_fd,
        stack_path,
        &Watchdog::monotonicNowNs,
        &Watchdog::armTimer);
  }

  static std::unique_ptr<Watchdog> createWithArmError(
      std::chrono::milliseconds timeout,
      int kmsg_fd,
      const std::string& stack_path,
      int error) {
    return createImpl(
        timeout,
        kmsg_fd,
        stack_path,
        &Watchdog::monotonicNowNs,
        [error](int, std::chrono::nanoseconds, std::chrono::nanoseconds) {
          return error;
        });
  }

  static std::unique_ptr<Watchdog> createWithArmFn(
      std::chrono::milliseconds timeout,
      int kmsg_fd,
      const std::string& stack_path,
      WatchdogTestClock& clock,
      Watchdog::ArmFn arm_fn) {
    return createImpl(
        timeout,
        kmsg_fd,
        stack_path,
        [&clock] { return clock.now_ns.load(); },
        std::move(arm_fn));
  }

  static bool start(Watchdog& watchdog) {
    return watchdog.start();
  }

  static int armHeartbeatTimer(Watchdog& watchdog) {
    return watchdog.armHeartbeatTimer();
  }

  static bool handleHeartbeatDeadline(Watchdog& watchdog) {
    return watchdog.handleHeartbeatDeadline() ==
        Watchdog::EventAction::Continue;
  }

 private:
  static std::unique_ptr<Watchdog> createImpl(
      std::chrono::milliseconds timeout,
      int kmsg_fd,
      const std::string& stack_path,
      Watchdog::NowFn now_fn,
      Watchdog::ArmFn arm_fn) {
    return std::unique_ptr<Watchdog>(new Watchdog(
        timeout,
        Fs::Fd(kmsg_fd),
        stack_path,
        std::move(now_fn),
        std::move(arm_fn),
        Fs::Fd(::timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC | TFD_NONBLOCK)),
        Fs::Fd(::eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK))));
  }
};

} // namespace Oomd

using namespace Oomd;
using namespace std::chrono_literals;

namespace {

void setNow(WatchdogTestClock& clock, std::chrono::milliseconds value) {
  clock.now_ns.store(
      std::chrono::duration_cast<std::chrono::nanoseconds>(value).count());
}

class TempSink {
 public:
  TempSink() {
    const char* tmpdir = ::getenv("TMPDIR");
    path_ = std::string(tmpdir != nullptr ? tmpdir : "/tmp") +
        "/oomd_watchdogtest.XXXXXX";
    fd_ = ::mkstemp(path_.data());
    if (fd_ >= 0) {
      const int flags = ::fcntl(fd_, F_GETFL);
      if (flags >= 0) {
        ::fcntl(fd_, F_SETFL, flags | O_APPEND);
      }
    }
  }

  ~TempSink() {
    ::unlink(path_.c_str());
  }

  TempSink(const TempSink&) = delete;
  TempSink& operator=(const TempSink&) = delete;

  int fd() const {
    return fd_;
  }

  const std::string& path() const {
    return path_;
  }

  std::string contents() const {
    std::ifstream file(path_);
    std::stringstream out;
    out << file.rdbuf();
    return out.str();
  }

 private:
  std::string path_;
  int fd_{-1};
};

class TempStackSource {
 public:
  explicit TempStackSource(const std::string& contents) {
    const char* tmpdir = ::getenv("TMPDIR");
    path_ = std::string(tmpdir != nullptr ? tmpdir : "/tmp") +
        "/oomd_watchdogstack.XXXXXX";
    const int fd = ::mkstemp(path_.data());
    if (fd < 0) {
      path_.clear();
      return;
    }
    const bool wrote = writeContents(fd, contents);
    const bool closed = ::close(fd) == 0;
    if (!wrote || !closed) {
      ::unlink(path_.c_str());
      path_.clear();
    }
  }

  ~TempStackSource() {
    if (!path_.empty()) {
      ::unlink(path_.c_str());
    }
  }

  TempStackSource(const TempStackSource&) = delete;
  TempStackSource& operator=(const TempStackSource&) = delete;

  const std::string& path() const {
    return path_;
  }

  bool valid() const {
    return !path_.empty();
  }

  bool replace(const std::string& contents) const {
    const int fd = ::open(path_.c_str(), O_WRONLY | O_TRUNC | O_CLOEXEC);
    if (fd < 0) {
      return false;
    }
    const bool wrote = writeContents(fd, contents);
    return ::close(fd) == 0 && wrote;
  }

 private:
  static bool writeContents(int fd, const std::string& contents) {
    size_t written = 0;
    while (written < contents.size()) {
      const ssize_t bytes =
          ::write(fd, contents.data() + written, contents.size() - written);
      if (bytes < 0 && errno == EINTR) {
        continue;
      }
      if (bytes <= 0) {
        return false;
      }
      written += static_cast<size_t>(bytes);
    }
    return true;
  }

  std::string path_;
};

class TempDirectory {
 public:
  TempDirectory() {
    const char* tmpdir = ::getenv("TMPDIR");
    path_ = std::string(tmpdir != nullptr ? tmpdir : "/tmp") +
        "/oomd_watchdogdir.XXXXXX";
    if (::mkdtemp(path_.data()) == nullptr) {
      path_.clear();
    }
  }

  ~TempDirectory() {
    if (!path_.empty()) {
      ::rmdir(path_.c_str());
    }
  }

  const std::string& path() const {
    return path_;
  }

 private:
  std::string path_;
};

class TempFifo {
 public:
  TempFifo() {
    const char* tmpdir = ::getenv("TMPDIR");
    path_ = std::string(tmpdir != nullptr ? tmpdir : "/tmp") +
        "/oomd_watchdogfifo.XXXXXX";
    const int fd = ::mkstemp(path_.data());
    if (fd < 0) {
      path_.clear();
      return;
    }
    const bool closed = ::close(fd) == 0;
    const bool removed = ::unlink(path_.c_str()) == 0;
    if (!closed || !removed || ::mkfifo(path_.c_str(), 0600) != 0) {
      ::unlink(path_.c_str());
      path_.clear();
    }
  }

  ~TempFifo() {
    if (!path_.empty()) {
      ::unlink(path_.c_str());
    }
  }

  TempFifo(const TempFifo&) = delete;
  TempFifo& operator=(const TempFifo&) = delete;

  const std::string& path() const {
    return path_;
  }

 private:
  std::string path_;
};

std::vector<std::string> records(const std::string& output) {
  std::vector<std::string> result;
  std::istringstream lines(output);
  for (std::string line; std::getline(lines, line);) {
    if (line.find("fb-oomd watchdog: kind=stall") != std::string::npos) {
      result.push_back(std::move(line));
    }
  }
  return result;
}

std::string field(const std::string& record, const std::string& name) {
  const size_t begin = record.find(name);
  if (begin == std::string::npos) {
    return "";
  }
  const size_t value_begin = begin + name.size();
  const size_t end = record.find(' ', value_begin);
  return record.substr(value_begin, end - value_begin);
}

void expectRecord(
    const std::string& record,
    const std::string& heartbeat_ms,
    const std::string& stack) {
  EXPECT_EQ(field(record, "heartbeat_mono_ms="), heartbeat_ms);
  EXPECT_EQ(field(record, "timeout_ms="), "100");
  EXPECT_EQ(field(record, "stack_status="), "ok");
  EXPECT_EQ(field(record, "stack="), stack);
  EXPECT_EQ(field(record, "pid="), std::to_string(::getpid()));
}

std::string readAvailable(int fd) {
  std::string output;
  std::array<char, 4096> buffer{};
  while (true) {
    const ssize_t bytes = ::read(fd, buffer.data(), buffer.size());
    if (bytes > 0) {
      output.append(buffer.data(), static_cast<size_t>(bytes));
      continue;
    }
    if (bytes < 0 && errno == EINTR) {
      continue;
    }
    break;
  }
  return output;
}

bool fillPipe(int fd) {
  std::array<char, 4096> fill{};
  while (::write(fd, fill.data(), fill.size()) > 0) {
  }
  return errno == EAGAIN;
}

std::string reportForStackPath(const std::string& stack_path) {
  TempSink sink;
  EXPECT_GE(sink.fd(), 0);
  WatchdogTestClock clock;
  setNow(clock, 100ms);
  {
    auto watchdog =
        WatchdogTestPeer::create(100ms, sink.fd(), stack_path, clock);
    watchdog->beat();
    setNow(clock, 200ms);
    EXPECT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));
  }
  const auto emitted = records(sink.contents());
  EXPECT_EQ(emitted.size(), 1);
  return emitted.empty() ? "" : emitted.front();
}

} // namespace

TEST(WatchdogTest, ConfiguredTimeoutSetsDeadlineAndRepeatInterval) {
  TempSink sink;
  TempStackSource stack("frame\n");
  ASSERT_GE(sink.fd(), 0);
  ASSERT_TRUE(stack.valid());
  WatchdogTestClock clock;
  setNow(clock, 1000ms);
  std::chrono::nanoseconds initial;
  std::chrono::nanoseconds repeat;
  int arm_calls = 0;
  auto watchdog = WatchdogTestPeer::createWithArmFn(
      250ms,
      sink.fd(),
      stack.path(),
      clock,
      [&](int, std::chrono::nanoseconds first, std::chrono::nanoseconds next) {
        ++arm_calls;
        initial = first;
        repeat = next;
        return 0;
      });

  EXPECT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));
  EXPECT_TRUE(sink.contents().empty());

  watchdog->beat();
  ASSERT_EQ(WatchdogTestPeer::armHeartbeatTimer(*watchdog), 0);

  EXPECT_EQ(arm_calls, 1);
  EXPECT_EQ(initial, 250ms);
  EXPECT_EQ(repeat, 250ms);
  setNow(clock, 1249ms);
  EXPECT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));
  EXPECT_EQ(initial, 1ms);
  EXPECT_TRUE(sink.contents().empty());
  setNow(clock, 1250ms);
  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));

  const auto emitted = records(sink.contents());
  ASSERT_EQ(emitted.size(), 1);
  EXPECT_EQ(field(emitted[0], "heartbeat_age_ms="), "250");
  EXPECT_EQ(field(emitted[0], "timeout_ms="), "250");
  EXPECT_EQ(field(emitted[0], "heartbeat_mono_ms="), "1000");
}

TEST(WatchdogTest, RepeatedReportsShareHeartbeatAndIncreaseAge) {
  TempSink sink;
  TempStackSource stack("blocked frame\nsecond+0x1a/0x90\n");
  ASSERT_GE(sink.fd(), 0);
  ASSERT_TRUE(stack.valid());
  WatchdogTestClock clock;
  setNow(clock, 1000ms);
  auto watchdog =
      WatchdogTestPeer::create(100ms, sink.fd(), stack.path(), clock);

  watchdog->beat();
  setNow(clock, 1100ms);
  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));
  setNow(clock, 1200ms);
  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));

  const auto emitted = records(sink.contents());
  ASSERT_EQ(emitted.size(), 2);
  expectRecord(emitted[0], "1000", "blocked_frame;second+0x1a/0x90");
  expectRecord(emitted[1], "1000", "blocked_frame;second+0x1a/0x90");
  EXPECT_EQ(field(emitted[0], "heartbeat_age_ms="), "100");
  EXPECT_EQ(field(emitted[1], "heartbeat_age_ms="), "200");
}

TEST(WatchdogTest, RearmEndsEpisodeAndNextStallUsesNewHeartbeat) {
  TempSink sink;
  TempStackSource stack("frame\n");
  ASSERT_GE(sink.fd(), 0);
  ASSERT_TRUE(stack.valid());
  WatchdogTestClock clock;
  auto watchdog =
      WatchdogTestPeer::create(100ms, sink.fd(), stack.path(), clock);

  setNow(clock, 100ms);
  watchdog->beat();
  setNow(clock, 250ms);
  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));
  setNow(clock, 300ms);
  watchdog->beat();
  setNow(clock, 399ms);
  EXPECT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));
  EXPECT_EQ(records(sink.contents()).size(), 1);
  setNow(clock, 450ms);
  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));

  const auto emitted = records(sink.contents());
  ASSERT_EQ(emitted.size(), 2);
  expectRecord(emitted[0], "100", "frame");
  expectRecord(emitted[1], "300", "frame");
}

TEST(WatchdogTest, EarlyTimerExpirationRearmsForLatestHeartbeat) {
  TempSink sink;
  TempStackSource stack("frame\n");
  ASSERT_GE(sink.fd(), 0);
  ASSERT_TRUE(stack.valid());
  WatchdogTestClock clock;
  std::chrono::nanoseconds initial;
  auto watchdog = WatchdogTestPeer::createWithArmFn(
      100ms,
      sink.fd(),
      stack.path(),
      clock,
      [&](int, std::chrono::nanoseconds first, std::chrono::nanoseconds) {
        initial = first;
        return 0;
      });

  setNow(clock, 100ms);
  watchdog->beat();
  setNow(clock, 125ms);

  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));
  EXPECT_EQ(initial, 75ms);
  EXPECT_TRUE(sink.contents().empty());

  setNow(clock, 200ms);
  ASSERT_EQ(WatchdogTestPeer::armHeartbeatTimer(*watchdog), 0);
  EXPECT_EQ(initial, 1ns);
}

TEST(WatchdogTest, RawStackTruncationRequiresDataBeyondReadLimit) {
  constexpr size_t kRawStackReadLimit = 4096;
  std::string input(kRawStackReadLimit - 1, '\n');
  input[0] = 'x';
  TempStackSource below_limit(input);
  input.push_back('\n');
  TempStackSource exact_limit(input);
  input.push_back('\n');
  TempStackSource above_limit(input);
  ASSERT_TRUE(below_limit.valid());
  ASSERT_TRUE(exact_limit.valid());
  ASSERT_TRUE(above_limit.valid());

  const auto below_record = reportForStackPath(below_limit.path());
  const auto exact_record = reportForStackPath(exact_limit.path());
  const auto above_record = reportForStackPath(above_limit.path());

  EXPECT_EQ(field(below_record, "stack="), "x");
  EXPECT_EQ(field(below_record, "stack_truncated="), "0");
  EXPECT_EQ(field(exact_record, "stack="), "x");
  EXPECT_EQ(field(exact_record, "stack_truncated="), "0");
  EXPECT_EQ(field(above_record, "stack="), "x");
  EXPECT_EQ(field(above_record, "stack_truncated="), "1");
}

TEST(WatchdogTest, TruncatedStackIsSerializedWithinRecordLimit) {
  TempSink sink;
  TempStackSource stack(std::string(1000, 'x'));
  ASSERT_GE(sink.fd(), 0);
  ASSERT_TRUE(stack.valid());
  WatchdogTestClock clock;
  setNow(clock, 100ms);
  auto watchdog =
      WatchdogTestPeer::create(100ms, sink.fd(), stack.path(), clock);

  watchdog->beat();
  setNow(clock, 200ms);
  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));

  const auto emitted = records(sink.contents());
  ASSERT_EQ(emitted.size(), 1);
  EXPECT_EQ(field(emitted[0], "stack_truncated="), "1");
  EXPECT_EQ(field(emitted[0], "stack=").size(), 512);
  EXPECT_LE(emitted[0].size() + 1, 800);
}

TEST(WatchdogTest, ReportsStackOpenReadAndEmptyFailures) {
  TempStackSource empty("");
  TempDirectory directory;
  ASSERT_TRUE(empty.valid());
  ASSERT_FALSE(directory.path().empty());

  EXPECT_EQ(
      field(
          reportForStackPath("/proc/self/definitely/not/a/stack"),
          "stack_status="),
      "open_failed");
  EXPECT_EQ(
      field(reportForStackPath(directory.path()), "stack_status="),
      "read_failed");
  EXPECT_EQ(field(reportForStackPath(empty.path()), "stack_status="), "empty");
}

TEST(WatchdogTest, FailedOutputWriteAllowsLaterSampleForSameHeartbeat) {
  int pipe_fds[2];
  ASSERT_EQ(::pipe2(pipe_fds, O_NONBLOCK | O_CLOEXEC), 0);
  ASSERT_TRUE(fillPipe(pipe_fds[1]));

  TempStackSource stack("original stack\n");
  ASSERT_TRUE(stack.valid());
  WatchdogTestClock clock;
  setNow(clock, 100ms);
  auto watchdog =
      WatchdogTestPeer::create(100ms, pipe_fds[1], stack.path(), clock);
  watchdog->beat();
  setNow(clock, 200ms);
  EXPECT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));

  ASSERT_TRUE(stack.replace("replacement stack\n"));
  EXPECT_TRUE(records(readAvailable(pipe_fds[0])).empty());
  setNow(clock, 250ms);
  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));

  const auto emitted = records(readAvailable(pipe_fds[0]));
  ASSERT_EQ(emitted.size(), 1);
  EXPECT_EQ(field(emitted[0], "heartbeat_age_ms="), "150");
  EXPECT_EQ(field(emitted[0], "heartbeat_mono_ms="), "100");
  EXPECT_EQ(field(emitted[0], "stack="), "replacement_stack");
  watchdog.reset();
  ::close(pipe_fds[0]);
}

TEST(WatchdogTest, BindsWatchedTidAtConstruction) {
  TempSink sink;
  TempStackSource stack("frame\n");
  ASSERT_GE(sink.fd(), 0);
  ASSERT_TRUE(stack.valid());
  WatchdogTestClock clock;
  setNow(clock, 100ms);
  const auto constructing_tid = static_cast<pid_t>(::syscall(SYS_gettid));
  auto watchdog =
      WatchdogTestPeer::create(100ms, sink.fd(), stack.path(), clock);

  watchdog->beat();
  setNow(clock, 200ms);
  ASSERT_TRUE(WatchdogTestPeer::handleHeartbeatDeadline(*watchdog));

  const auto emitted = records(sink.contents());
  ASSERT_EQ(emitted.size(), 1);
  EXPECT_EQ(field(emitted[0], "tid="), std::to_string(constructing_tid));
}

TEST(WatchdogTest, HeartbeatDuringStackCaptureDropsStaleReport) {
  TempSink sink;
  TempFifo stack;
  ASSERT_GE(sink.fd(), 0);
  ASSERT_FALSE(stack.path().empty());
  WatchdogTestClock clock;
  setNow(clock, 100ms);
  auto watchdog =
      WatchdogTestPeer::create(100ms, sink.fd(), stack.path(), clock);
  watchdog->beat();
  setNow(clock, 200ms);

  bool continued = false;
  std::thread reporter([&] {
    continued = WatchdogTestPeer::handleHeartbeatDeadline(*watchdog);
  });
  int writer = -1;
  for (int attempt = 0; attempt < 200 && writer < 0; ++attempt) {
    writer = ::open(stack.path().c_str(), O_WRONLY | O_NONBLOCK | O_CLOEXEC);
    if (writer < 0 && errno != ENXIO && errno != EINTR) {
      break;
    }
    if (writer < 0) {
      ::poll(nullptr, 0, 10);
    }
  }
  if (writer < 0) {
    const int unblock =
        ::open(stack.path().c_str(), O_RDWR | O_NONBLOCK | O_CLOEXEC);
    if (unblock >= 0) {
      constexpr char kUnblock[] = "unblock\n";
      ::write(unblock, kUnblock, sizeof(kUnblock) - 1);
    }
    reporter.join();
    if (unblock >= 0) {
      ::close(unblock);
    }
    FAIL() << "reporter did not open the stack FIFO";
  }

  setNow(clock, 300ms);
  watchdog->beat();
  constexpr char kStack[] = "old frame\n";
  const ssize_t bytes = ::write(writer, kStack, sizeof(kStack) - 1);
  const int close_result = ::close(writer);
  reporter.join();

  EXPECT_EQ(bytes, static_cast<ssize_t>(sizeof(kStack) - 1));
  EXPECT_EQ(close_result, 0);
  EXPECT_TRUE(continued);
  EXPECT_TRUE(sink.contents().empty());
}

TEST(WatchdogTest, TimerFailureIsReportedOnceByWorker) {
  int pipe_fds[2];
  ASSERT_EQ(::pipe2(pipe_fds, O_NONBLOCK | O_CLOEXEC), 0);
  TempStackSource stack("frame\n");
  ASSERT_TRUE(stack.valid());
  auto watchdog = WatchdogTestPeer::createWithArmError(
      100ms, pipe_fds[1], stack.path(), EBADF);
  ASSERT_TRUE(WatchdogTestPeer::start(*watchdog));

  watchdog->beat();

  struct pollfd report_fd{.fd = pipe_fds[0], .events = POLLIN, .revents = 0};
  ASSERT_EQ(::poll(&report_fd, 1, 2000), 1);
  EXPECT_TRUE(report_fd.revents & POLLIN);
  EXPECT_EQ(
      readAvailable(pipe_fds[0]),
      "<3>fb-oomd watchdog: kind=error operation=timerfd_settime error=" +
          std::to_string(EBADF) + "\n");

  watchdog->beat();
  report_fd.revents = 0;
  EXPECT_EQ(::poll(&report_fd, 1, 100), 0);

  watchdog.reset();
  ::close(pipe_fds[0]);
}

TEST(WatchdogTest, RunningThreadRepeatsHeartbeatAndJoinsOnShutdown) {
  int pipe_fds[2];
  ASSERT_EQ(::pipe2(pipe_fds, O_NONBLOCK | O_CLOEXEC), 0);
  TempStackSource stack("thread frame\n");
  ASSERT_TRUE(stack.valid());
  auto watchdog =
      WatchdogTestPeer::createWithRealClock(100ms, pipe_fds[1], stack.path());
  ASSERT_TRUE(WatchdogTestPeer::start(*watchdog));
  watchdog->beat();

  struct pollfd report_fd{.fd = pipe_fds[0], .events = POLLIN, .revents = 0};
  ASSERT_EQ(::poll(&report_fd, 1, 2000), 1);
  EXPECT_TRUE(report_fd.revents & POLLIN);
  std::string output = readAvailable(pipe_fds[0]);

  report_fd.revents = 0;
  ASSERT_EQ(::poll(&report_fd, 1, 2000), 1);
  EXPECT_TRUE(report_fd.revents & POLLIN);
  output += readAvailable(pipe_fds[0]);

  const auto emitted = records(output);
  ASSERT_GE(emitted.size(), 2);
  EXPECT_EQ(
      field(emitted[0], "heartbeat_mono_ms="),
      field(emitted.back(), "heartbeat_mono_ms="));
  EXPECT_LT(
      std::stoull(field(emitted[0], "heartbeat_age_ms=")),
      std::stoull(field(emitted.back(), "heartbeat_age_ms=")));

  watchdog.reset();
  ::close(pipe_fds[0]);
}

TEST(WatchdogTest, CreateRejectsInvalidConfiguration) {
  EXPECT_EQ(Watchdog::create(0ms, "/dev/null"), nullptr);
  EXPECT_EQ(Watchdog::create(-1ms, "/dev/null"), nullptr);
  EXPECT_EQ(
      Watchdog::create(std::chrono::milliseconds::max(), "/dev/null"), nullptr);
  EXPECT_EQ(Watchdog::create(1s, "/proc/self/definitely/not/a/path"), nullptr);
}

TEST(WatchdogTest, CreateStartsAndStopsWorker) {
  TempSink sink;
  ASSERT_GE(sink.fd(), 0);
  ASSERT_EQ(::close(sink.fd()), 0);

  const auto watchdog = Watchdog::create(1s, sink.path());

  EXPECT_NE(watchdog, nullptr);
}
