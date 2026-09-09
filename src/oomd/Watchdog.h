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

#include <sys/types.h>
#include <array>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <thread>

#include "oomd/util/Fs.h"

namespace Oomd {

class WatchdogTestPeer;

class Watchdog {
 public:
  ~Watchdog();

  Watchdog(const Watchdog&) = delete;
  Watchdog& operator=(const Watchdog&) = delete;
  Watchdog(Watchdog&&) = delete;
  Watchdog& operator=(Watchdog&&) = delete;

  // Creates a watchdog for the calling thread. The caller must be the
  // event-loop thread because its TID is captured for later stack sampling.
  static std::unique_ptr<Watchdog> create(
      std::chrono::milliseconds timeout,
      const std::string& kmsg_path);

  // Publishes one event-loop heartbeat. If no newer heartbeat arrives within
  // the configured timeout, the watchdog samples the watched thread's kernel
  // stack at that same interval until progress resumes.
  void beat();

 private:
  friend class WatchdogTestPeer;

  using NowFn = std::function<uint64_t()>;
  using ArmFn = std::function<
      int(int, std::chrono::nanoseconds, std::chrono::nanoseconds)>;
  static constexpr size_t kStackMax = 512;

  struct StackCapture {
    std::array<char, kStackMax> stack{};
    size_t stack_len{0};
    bool truncated{false};
    const char* status{"empty"};
  };

  enum class EventAction { Continue, Stop };

  // Test-only dependencies are supplied through WatchdogTestPeer; production
  // callers use create().
  Watchdog(
      std::chrono::nanoseconds timeout,
      Fs::Fd kmsg_fd,
      std::string stack_path,
      NowFn now_fn,
      ArmFn arm_fn,
      Fs::Fd timer_fd,
      Fs::Fd control_fd);

  static uint64_t monotonicNowNs();
  static int armTimer(
      int fd,
      std::chrono::nanoseconds initial,
      std::chrono::nanoseconds interval);
  uint64_t now() const;
  int armHeartbeatTimer();

  bool start();
  void notifyWorker();
  EventAction handleControlEvent();
  EventAction handleTimerExpiration();
  EventAction handleHeartbeatDeadline();
  void threadMain();
  StackCapture captureKernelStack() const;
  static StackCapture
  normalizeStack(const char* input, size_t input_len, bool source_truncated);
  bool reportStall(uint64_t heartbeat_ns, uint64_t observed_ns) const;
  bool emit(const char* buf, size_t len) const;

  const std::chrono::nanoseconds timeout_;
  Fs::Fd kmsg_fd_;
  const std::string stack_path_;
  const NowFn now_fn_;
  const ArmFn arm_fn_;
  const pid_t watched_tid_;

  std::atomic<uint64_t> beat_ns_{0};
  std::atomic<bool> stop_requested_{false};

  std::thread thread_;
  Fs::Fd timer_fd_;
  Fs::Fd control_fd_;
};

} // namespace Oomd
