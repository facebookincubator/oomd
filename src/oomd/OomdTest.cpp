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

#include <pthread.h>
#include <unistd.h>
#include <atomic>
#include <cerrno>
#include <chrono>
#include <condition_variable>
#include <csignal>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

#include "oomd/Oomd.h"
#include "oomd/ReclaimPool.h"
#include "oomd/Watchdog.h"
#include "oomd/config/ConfigTypes.h"
#include "oomd/engine/Engine.h"

using namespace Oomd;
using namespace std::chrono_literals;

namespace {

using Cancel = ReclaimPool::CancelReason;
using Completion = ReclaimPool::Completion;

// Lets a test hold fake writes and see when they start and end.
class Gate {
 public:
  void arrive() {
    std::lock_guard<std::mutex> lock(mutex_);
    ++arrived_;
    cv_.notify_all();
  }

  void arriveAndWait() {
    std::unique_lock<std::mutex> lock(mutex_);
    ++arrived_;
    cv_.notify_all();
    cv_.wait(lock, [&] { return open_; });
  }

  bool waitForArrivals(int n) {
    std::unique_lock<std::mutex> lock(mutex_);
    return cv_.wait_for(lock, 5s, [&] { return arrived_ >= n; });
  }

  void release() {
    {
      std::lock_guard<std::mutex> lock(mutex_);
      open_ = true;
    }
    cv_.notify_all();
  }

 private:
  std::mutex mutex_;
  std::condition_variable cv_;
  int arrived_{0};
  bool open_{false};
};

// Oomd::run() returns after a signal in its mask, which it raises again, so
// the test stops the loop with a signal whose handler does nothing.
int stopSignal() {
  return SIGRTMIN;
}

// Runs Oomd's event loop, with no rulesets and a 1 s tick, on its own thread.
// Only this loop calls the pool's supervise().
class OomdLoop {
 public:
  explicit OomdLoop(std::unique_ptr<ReclaimPool> pool) {
    struct sigaction sa{};
    sa.sa_handler = [](int) {};
    ::sigaction(stopSignal(), &sa, nullptr);
    sigemptyset(&mask_);
    sigaddset(&mask_, stopSignal());
    // The loop thread inherits the block, so sigtimedwait() gets the signal.
    ::pthread_sigmask(SIG_BLOCK, &mask_, nullptr);

    oomd_ = std::make_unique<::Oomd::Oomd>(
        std::make_unique<Config2::IR::Root>(),
        std::make_unique<Engine::Engine>(
            std::vector<std::unique_ptr<Engine::Ruleset>>{},
            std::vector<std::unique_ptr<Engine::PrekillHook>>{}),
        /*interval=*/1,
        "/sys/fs/cgroup",
        /*drop_in_dir=*/"",
        std::unordered_map<std::string, DeviceType>{},
        IOCostCoeffs{},
        IOCostCoeffs{},
        /*watchdog=*/nullptr,
        std::move(pool));
    thread_ = std::thread([this] { oomd_->run(&mask_); });
  }

  ~OomdLoop() {
    ::pthread_kill(thread_.native_handle(), stopSignal());
    thread_.join();
  }

  OomdLoop(const OomdLoop&) = delete;
  OomdLoop& operator=(const OomdLoop&) = delete;

 private:
  sigset_t mask_{};
  std::unique_ptr<::Oomd::Oomd> oomd_;
  std::thread thread_;
};

MemoryReclaimRequest job() {
  MemoryReclaimRequest job;
  job.cgroup_id = 1;
  job.bytes = 4096;
  return job;
}

// Waits for the cancel signal, like a long memory.reclaim.
ReclaimPool::Write longWrite(std::shared_ptr<Gate> ended) {
  return
      [ended](const MemoryReclaimRequest&, const std::atomic<Cancel>& cancel) {
        while (cancel.load() == Cancel::None) {
          ::pause();
        }
        ended->arrive();
        return EINTR;
      };
}

// The cancel signal lands while the write waits at the gate, so the pause()
// after it, like a write that started just after its signal, ends only on a
// signal sent again.
ReclaimPool::Write writeThatMissesItsSignal(
    std::shared_ptr<Gate> started,
    std::shared_ptr<Gate> ended) {
  return [started, ended](
             const MemoryReclaimRequest&, const std::atomic<Cancel>&) {
    started->arriveAndWait();
    ::pause();
    ended->arrive();
    return EINTR;
  };
}

// The pool records a completion right after its writer returns.
std::vector<Completion> drainAfterWrite(
    ReclaimPool& pool,
    ReclaimPool::Owner& owner) {
  std::vector<Completion> done;
  const auto deadline = std::chrono::steady_clock::now() + 5s;
  while (done.empty() && std::chrono::steady_clock::now() < deadline) {
    done = pool.drain(owner);
    std::this_thread::yield();
  }
  return done;
}

} // namespace

TEST(OomdTest, LoopExpiresReclaimWritesThatRunTooLong) {
  auto ended = std::make_shared<Gate>();
  auto pool = std::make_unique<ReclaimPool>(
      1, ReclaimPool::Options{.write = longWrite(ended), .max_run = 50ms});
  auto& reclaim_pool = *pool;
  OomdLoop loop(std::move(pool));
  auto owner = reclaim_pool.registerOwner();

  reclaim_pool.submit(owner, job());
  ASSERT_TRUE(ended->waitForArrivals(1));

  const auto done = drainAfterWrite(reclaim_pool, *owner);
  ASSERT_EQ(done.size(), 1);
  EXPECT_EQ(done[0].err, EINTR);
  EXPECT_EQ(done[0].cancel, Cancel::Expired);
}

TEST(OomdTest, LoopResendsTheCancelSignalToAWriteThatMissedIt) {
  auto started = std::make_shared<Gate>();
  auto ended = std::make_shared<Gate>();
  auto pool = std::make_unique<ReclaimPool>(
      1,
      ReclaimPool::Options{.write = writeThatMissesItsSignal(started, ended)});
  auto& reclaim_pool = *pool;
  OomdLoop loop(std::move(pool));
  auto owner = reclaim_pool.registerOwner();

  const auto id = reclaim_pool.submit(owner, job());
  ASSERT_TRUE(started->waitForArrivals(1));
  reclaim_pool.cancel(id);
  started->release();
  ASSERT_TRUE(ended->waitForArrivals(1));

  const auto done = drainAfterWrite(reclaim_pool, *owner);
  ASSERT_EQ(done.size(), 1);
  EXPECT_EQ(done[0].err, EINTR);
  EXPECT_EQ(done[0].cancel, Cancel::Requested);
}
