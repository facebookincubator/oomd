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

#include "oomd/Watchdog.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/eventfd.h>
#include <sys/stat.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>
#include <cstdio>
#include <system_error>
#include <utility>

namespace Oomd {

namespace {

static_assert(std::atomic<uint64_t>::is_always_lock_free);
constexpr auto kDefaultKmsgPath = "/dev/kmsg";
constexpr size_t kRecordMax = 800;
constexpr size_t kRawStackMax = 4096;
// A zero initial expiration disarms timerfd instead of firing immediately.
constexpr auto kImmediateTimerDelay = std::chrono::nanoseconds(1);

struct timespec toTimespec(std::chrono::nanoseconds ns) {
  const auto secs = std::chrono::duration_cast<std::chrono::seconds>(ns);
  struct timespec ts{};
  ts.tv_sec = secs.count();
  ts.tv_nsec = (ns - secs).count();
  return ts;
}

void writeRecord(int fd, const char* record, size_t len) {
  const int output_fd = fd >= 0 ? fd : STDERR_FILENO;
  ssize_t written;
  do {
    written = ::write(output_fd, record, len);
  } while (written < 0 && errno == EINTR);
}

void writeError(int fd, const char* operation, int error) {
  char record[256];
  const int n = ::snprintf(
      record,
      sizeof(record),
      "<3>fb-oomd watchdog: kind=error operation=%s error=%d\n",
      operation,
      error);
  if (n <= 0 || static_cast<size_t>(n) >= sizeof(record)) {
    return;
  }

  writeRecord(fd, record, static_cast<size_t>(n));
}

void writePollError(int fd, short timer_revents, short control_revents) {
  char record[256];
  const int n = ::snprintf(
      record,
      sizeof(record),
      "<3>fb-oomd watchdog: kind=error operation=poll "
      "timer_revents=%d control_revents=%d\n",
      static_cast<int>(timer_revents),
      static_cast<int>(control_revents));
  if (n <= 0 || static_cast<size_t>(n) >= sizeof(record)) {
    return;
  }

  writeRecord(fd, record, static_cast<size_t>(n));
}

} // namespace

Watchdog::Watchdog(
    std::chrono::nanoseconds timeout,
    Fs::Fd kmsg_fd,
    std::string stack_path,
    NowFn now_fn,
    ArmFn arm_fn,
    Fs::Fd timer_fd,
    Fs::Fd control_fd)
    : timeout_(timeout),
      kmsg_fd_(std::move(kmsg_fd)),
      stack_path_(std::move(stack_path)),
      now_fn_(std::move(now_fn)),
      arm_fn_(std::move(arm_fn)),
      watched_tid_(::gettid()),
      timer_fd_(std::move(timer_fd)),
      control_fd_(std::move(control_fd)) {}

Watchdog::~Watchdog() {
  if (!thread_.joinable()) {
    return;
  }

  stop_requested_.store(true, std::memory_order_release);
  notifyWorker();
  thread_.join();
}

std::unique_ptr<Watchdog> Watchdog::create(
    std::chrono::milliseconds timeout,
    const std::string& kmsg_path) {
  constexpr auto kMaxMilliseconds =
      std::chrono::duration_cast<std::chrono::milliseconds>(
          std::chrono::nanoseconds::max());
  if (timeout <= std::chrono::milliseconds::zero() ||
      timeout > kMaxMilliseconds) {
    writeError(STDERR_FILENO, "invalid_timing", EINVAL);
    return nullptr;
  }

  Fs::Fd kmsg_fd(
      ::open(kmsg_path.c_str(), O_WRONLY | O_APPEND | O_NONBLOCK | O_CLOEXEC));
  if (kmsg_fd.fd() < 0) {
    writeError(STDERR_FILENO, "open_kmsg", errno);
    return nullptr;
  }
  if (kmsg_path == kDefaultKmsgPath) {
    struct stat st{};
    const int result = ::fstat(kmsg_fd.fd(), &st);
    if (result != 0 || !S_ISCHR(st.st_mode)) {
      writeError(kmsg_fd.fd(), "validate_kmsg", result != 0 ? errno : ENODEV);
      return nullptr;
    }
  }

  Fs::Fd timer_fd(
      ::timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC | TFD_NONBLOCK));
  if (timer_fd.fd() < 0) {
    const int error = errno;
    writeError(kmsg_fd.fd(), "timerfd_create", error);
    return nullptr;
  }
  Fs::Fd control_fd(::eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK));
  if (control_fd.fd() < 0) {
    const int error = errno;
    writeError(kmsg_fd.fd(), "eventfd", error);
    return nullptr;
  }

  auto watchdog = std::unique_ptr<Watchdog>(new Watchdog(
      std::chrono::duration_cast<std::chrono::nanoseconds>(timeout),
      std::move(kmsg_fd),
      "",
      &Watchdog::monotonicNowNs,
      &Watchdog::armTimer,
      std::move(timer_fd),
      std::move(control_fd)));
  watchdog->beat_ns_.store(watchdog->now(), std::memory_order_release);
  if (const int error = watchdog->armHeartbeatTimer()) {
    writeError(watchdog->kmsg_fd_.fd(), "timerfd_settime", error);
    return nullptr;
  }
  if (!watchdog->start()) {
    return nullptr;
  }
  return watchdog;
}

uint64_t Watchdog::monotonicNowNs() {
  struct timespec ts{};
  ::clock_gettime(CLOCK_MONOTONIC, &ts);
  return static_cast<uint64_t>(ts.tv_sec) * 1000000000ULL +
      static_cast<uint64_t>(ts.tv_nsec);
}

int Watchdog::armTimer(
    int fd,
    std::chrono::nanoseconds initial,
    std::chrono::nanoseconds interval) {
  struct itimerspec its{};
  its.it_value = toTimespec(initial);
  its.it_interval = toTimespec(interval);
  int rc;
  do {
    rc = ::timerfd_settime(fd, 0, &its, nullptr);
  } while (rc != 0 && errno == EINTR);
  return rc == 0 ? 0 : errno;
}

uint64_t Watchdog::now() const {
  return now_fn_();
}

void Watchdog::beat() {
  beat_ns_.store(now(), std::memory_order_release);
  notifyWorker();
}

int Watchdog::armHeartbeatTimer() {
  const uint64_t heartbeat_ns = beat_ns_.load(std::memory_order_acquire);
  if (heartbeat_ns == 0) {
    return 0;
  }

  auto initial = timeout_;
  const uint64_t observed_ns = now();
  if (observed_ns >= heartbeat_ns) {
    const uint64_t heartbeat_age_ns = observed_ns - heartbeat_ns;
    const uint64_t timeout_ns = static_cast<uint64_t>(timeout_.count());
    initial = heartbeat_age_ns < timeout_ns
        ? std::chrono::nanoseconds(timeout_ns - heartbeat_age_ns)
        : kImmediateTimerDelay;
  }
  return arm_fn_(timer_fd_.fd(), initial, timeout_);
}

bool Watchdog::start() {
  if (thread_.joinable()) {
    writeError(kmsg_fd_.fd(), "already_started", EALREADY);
    return false;
  }

  try {
    thread_ = std::thread([this] { threadMain(); });
  } catch (const std::system_error& e) {
    writeError(kmsg_fd_.fd(), "thread", e.code().value());
    timer_fd_ = Fs::Fd{};
    control_fd_ = Fs::Fd{};
    return false;
  }
  return true;
}

void Watchdog::notifyWorker() {
  const uint64_t one = 1;
  ssize_t written;
  do {
    written = ::write(control_fd_.fd(), &one, sizeof(one));
  } while (written < 0 && errno == EINTR);
}

Watchdog::EventAction Watchdog::handleControlEvent() {
  uint64_t notifications = 0;
  ssize_t bytes;
  do {
    bytes = ::read(control_fd_.fd(), &notifications, sizeof(notifications));
  } while (bytes < 0 && errno == EINTR);
  if (stop_requested_.load(std::memory_order_acquire)) {
    return EventAction::Stop;
  }
  if (bytes != static_cast<ssize_t>(sizeof(notifications))) {
    writeError(kmsg_fd_.fd(), "control_read", bytes < 0 ? errno : EMSGSIZE);
    return EventAction::Stop;
  }
  if (const int error = armHeartbeatTimer()) {
    writeError(kmsg_fd_.fd(), "timerfd_settime", error);
    return EventAction::Stop;
  }
  return EventAction::Continue;
}

Watchdog::EventAction Watchdog::handleTimerExpiration() {
  uint64_t expirations = 0;
  ssize_t bytes;
  do {
    bytes = ::read(timer_fd_.fd(), &expirations, sizeof(expirations));
  } while (bytes < 0 && errno == EINTR);
  if (bytes < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
    return EventAction::Continue;
  }
  if (bytes != static_cast<ssize_t>(sizeof(expirations))) {
    writeError(kmsg_fd_.fd(), "timer_read", bytes < 0 ? errno : EMSGSIZE);
    return EventAction::Stop;
  }

  return handleHeartbeatDeadline();
}

Watchdog::EventAction Watchdog::handleHeartbeatDeadline() {
  const uint64_t heartbeat_ns = beat_ns_.load(std::memory_order_acquire);
  const uint64_t observed_ns = now();
  const bool heartbeat_expired = heartbeat_ns != 0 &&
      observed_ns >= heartbeat_ns &&
      observed_ns - heartbeat_ns >= static_cast<uint64_t>(timeout_.count());
  if (heartbeat_expired) {
    reportStall(heartbeat_ns, observed_ns);
    return EventAction::Continue;
  }
  if (const int error = armHeartbeatTimer()) {
    writeError(kmsg_fd_.fd(), "timerfd_settime", error);
    return EventAction::Stop;
  }
  return EventAction::Continue;
}

void Watchdog::threadMain() {
  struct pollfd fds[2] = {
      {.fd = timer_fd_.fd(), .events = POLLIN, .revents = 0},
      {.fd = control_fd_.fd(), .events = POLLIN, .revents = 0},
  };

  while (true) {
    fds[0].revents = 0;
    fds[1].revents = 0;
    const int rc = ::poll(fds, 2, -1);
    if (rc < 0) {
      if (errno == EINTR) {
        continue;
      }
      writeError(kmsg_fd_.fd(), "poll", errno);
      return;
    }
    if ((fds[1].revents & POLLIN) &&
        handleControlEvent() == EventAction::Stop) {
      return;
    }
    if (fds[0].revents & (POLLERR | POLLHUP | POLLNVAL) ||
        fds[1].revents & (POLLERR | POLLHUP | POLLNVAL)) {
      writePollError(kmsg_fd_.fd(), fds[0].revents, fds[1].revents);
      return;
    }
    if (!(fds[0].revents & POLLIN)) {
      continue;
    }

    if (handleTimerExpiration() == EventAction::Stop) {
      return;
    }
  }
}

Watchdog::StackCapture Watchdog::captureKernelStack() const {
  char default_path[64];
  const char* path = stack_path_.c_str();
  if (stack_path_.empty()) {
    const int n = ::snprintf(
        default_path,
        sizeof(default_path),
        "/proc/self/task/%d/stack",
        static_cast<int>(watched_tid_));
    if (n <= 0 || static_cast<size_t>(n) >= sizeof(default_path)) {
      StackCapture capture;
      capture.status = "open_failed";
      return capture;
    }
    path = default_path;
  }

  Fs::Fd stack_fd(::open(path, O_RDONLY | O_CLOEXEC));
  if (stack_fd.fd() < 0) {
    StackCapture capture;
    capture.status = "open_failed";
    return capture;
  }

  std::array<char, kRawStackMax + 1> raw{};
  size_t bytes = 0;
  while (bytes < raw.size()) {
    ssize_t result;
    do {
      result = ::read(stack_fd.fd(), raw.data() + bytes, raw.size() - bytes);
    } while (result < 0 && errno == EINTR);
    if (result < 0) {
      StackCapture capture;
      capture.status = "read_failed";
      return capture;
    }
    if (result == 0) {
      break;
    }
    bytes += static_cast<size_t>(result);
  }
  if (bytes == 0) {
    return StackCapture{};
  }
  StackCapture capture;
  capture.status = "ok";
  capture.truncated = bytes > kRawStackMax;
  const size_t input_len = capture.truncated ? kRawStackMax : bytes;
  bool pending_separator = false;

  for (size_t pos = 0; pos < input_len;) {
    const char c = raw[pos];
    if (c == '\n' || c == '\r') {
      pending_separator = capture.stack_len > 0;
      ++pos;
      continue;
    }

    const size_t needed = pending_separator ? 2 : 1;
    if (capture.stack_len + needed > capture.stack.size()) {
      capture.truncated = true;
      break;
    }
    if (pending_separator) {
      capture.stack[capture.stack_len++] = ';';
      pending_separator = false;
    }
    capture.stack[capture.stack_len++] = c == ' ' || c == '\t' ? '_' : c;
    ++pos;
  }

  if (capture.stack_len == 0) {
    capture.status = "empty";
  }
  return capture;
}

bool Watchdog::reportStall(uint64_t heartbeat_ns, uint64_t observed_ns) const {
  const uint64_t timeout_ns = static_cast<uint64_t>(timeout_.count());
  const uint64_t heartbeat_age_ns = observed_ns - heartbeat_ns;
  const auto capture = captureKernelStack();
  if (beat_ns_.load(std::memory_order_acquire) != heartbeat_ns) {
    return false;
  }
  constexpr size_t kMaxUnsignedDecimal = 20;
  constexpr size_t kMaxSignedDecimal = 11;
  constexpr size_t kMaxStackStatus = sizeof("open_failed") - 1;
  constexpr size_t kFixedRecordMax =
      sizeof(
          "<4>fb-oomd watchdog: kind=stall heartbeat_age_ms= timeout_ms= "
          "pid= tid= heartbeat_mono_ms= stack_truncated= stack_status= "
          "stack=\n") -
      1 + 3 * kMaxUnsignedDecimal + 2 * kMaxSignedDecimal + 1 + kMaxStackStatus;
  static_assert(kFixedRecordMax + kStackMax + 1 <= kRecordMax);
  char record[kRecordMax];
  const int n = ::snprintf(
      record,
      sizeof(record),
      "<4>fb-oomd watchdog: kind=stall heartbeat_age_ms=%llu "
      "timeout_ms=%llu pid=%d tid=%d heartbeat_mono_ms=%llu "
      "stack_truncated=%d stack_status=%s stack=%.*s\n",
      static_cast<unsigned long long>(heartbeat_age_ns / 1000000ULL),
      static_cast<unsigned long long>(timeout_ns / 1000000ULL),
      static_cast<int>(::getpid()),
      static_cast<int>(watched_tid_),
      static_cast<unsigned long long>(heartbeat_ns / 1000000ULL),
      capture.truncated ? 1 : 0,
      capture.status,
      static_cast<int>(capture.stack_len),
      capture.stack.data());
  if (n <= 0 || static_cast<size_t>(n) >= sizeof(record)) {
    return false;
  }
  return emit(record, static_cast<size_t>(n));
}

bool Watchdog::emit(const char* buf, size_t len) const {
  if (kmsg_fd_.fd() < 0) {
    return false;
  }
  ssize_t written;
  do {
    written = ::write(kmsg_fd_.fd(), buf, len);
  } while (written < 0 && errno == EINTR);
  return written == static_cast<ssize_t>(len);
}

} // namespace Oomd
