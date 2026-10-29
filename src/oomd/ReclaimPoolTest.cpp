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

#include <fcntl.h>
#include <pthread.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <unistd.h>
#include <atomic>
#include <cerrno>
#include <chrono>
#include <condition_variable>
#include <fstream>
#include <iterator>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <string>
#include <system_error>
#include <thread>
#include <utility>
#include <vector>

#include "oomd/ReclaimPool.h"
#include "oomd/util/Fixture.h"

namespace Oomd {

class ReclaimPoolTestPeer {
 public:
  static int running(const ReclaimPool& pool) {
    return pool.counts().running;
  }
};

} // namespace Oomd

using namespace Oomd;
using namespace std::chrono_literals;

namespace {

using Cancel = ReclaimPool::CancelReason;
using Completion = ReclaimPool::Completion;

// Lets a test hold fake writes inside the pool and release them later.
class Gate {
 public:
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

MemoryReclaimRequest makeJob(uint64_t cgroup_id, int64_t bytes = 4096) {
  MemoryReclaimRequest request;
  request.cgroup_id = cgroup_id;
  request.bytes = bytes;
  return request;
}

// Drains until n completions arrive or 5s pass. Calls supervise() while
// waiting, as the event loop does every tick.
std::vector<Completion>
waitForCompletions(ReclaimPool& pool, ReclaimPool::Owner& owner, size_t n) {
  std::vector<Completion> done;
  const auto deadline = std::chrono::steady_clock::now() + 5s;
  while (done.size() < n && std::chrono::steady_clock::now() < deadline) {
    for (auto& completion : pool.drain(owner)) {
      done.push_back(std::move(completion));
    }
    pool.supervise();
    // The pool has no completion callback; like the event loop, this polls
    // it. Only the deadline depends on timing.
    // NOLINTNEXTLINE(facebook-hte-BadCall-sleep_for)
    std::this_thread::sleep_for(10ms);
  }
  return done;
}

bool cancelled(const std::atomic<Cancel>& cancel) {
  return cancel.load() != Cancel::None;
}

int succeed(const MemoryReclaimRequest&, const std::atomic<Cancel>&) {
  return 0;
}

// Runs until cancelled, like a long memory.reclaim that the cancel signal
// interrupts.
int runUntilCancelled(
    const MemoryReclaimRequest&,
    const std::atomic<Cancel>& cancel) {
  while (!cancelled(cancel)) {
    ::pause();
  }
  return EINTR;
}

// A writer that waits at the gate, then reports EINTR if it was cancelled.
ReclaimPool::Write gatedWriter(std::shared_ptr<Gate> gate) {
  return
      [gate](const MemoryReclaimRequest&, const std::atomic<Cancel>& cancel) {
        gate->arriveAndWait();
        return cancelled(cancel) ? EINTR : 0;
      };
}

// A writer that ignores cancellation until the gate opens, like a write stuck
// in the kernel.
ReclaimPool::Write stuckWriter(std::shared_ptr<Gate> gate) {
  return [gate](const MemoryReclaimRequest&, const std::atomic<Cancel>&) {
    gate->arriveAndWait();
    return EINTR;
  };
}

// A directory that looks like a cgroup, with an empty memory.reclaim.
class FakeCgroupDir {
 public:
  FakeCgroupDir() : path_(Fixture::mkdtempChecked()) {
    Fixture::materialize(Fixture::makeFile("memory.reclaim"), path_);
  }

  ~FakeCgroupDir() {
    Fixture::rmrChecked(path_);
  }

  const std::string& path() const {
    return path_;
  }

  uint64_t inode() const {
    struct ::stat st{};
    EXPECT_EQ(::stat(path_.c_str(), &st), 0) << path_;
    return st.st_ino;
  }

  std::string content() const {
    std::ifstream file(path_ + "/memory.reclaim");
    EXPECT_TRUE(file.is_open()) << path_;
    return {std::istreambuf_iterator<char>(file), {}};
  }

 private:
  std::string path_;
};

// Lowers the soft limit on processes, and so on threads, while it lives.
class ProcessLimit {
 public:
  explicit ProcessLimit(rlim_t limit) {
    EXPECT_EQ(::getrlimit(RLIMIT_NPROC, &saved_), 0);
    struct rlimit lowered = saved_;
    lowered.rlim_cur = limit;
    EXPECT_EQ(::setrlimit(RLIMIT_NPROC, &lowered), 0);
  }

  ~ProcessLimit() {
    EXPECT_EQ(::setrlimit(RLIMIT_NPROC, &saved_), 0);
  }

 private:
  struct rlimit saved_{};
};

} // namespace

TEST(ReclaimPoolTest, RunsJobsInOrderOnOneThread) {
  auto order = std::make_shared<std::vector<int64_t>>();
  auto order_mutex = std::make_shared<std::mutex>();
  ReclaimPool pool(1, {.write = [=](const MemoryReclaimRequest& job, auto&) {
                     std::lock_guard<std::mutex> lock(*order_mutex);
                     order->push_back(job.bytes);
                     return 0;
                   }});
  auto owner = pool.registerOwner();

  for (int64_t bytes : {1, 2, 3}) {
    pool.submit(owner, makeJob(bytes, bytes));
  }
  const auto done = waitForCompletions(pool, *owner, 3);

  ASSERT_EQ(done.size(), 3);
  for (const auto& completion : done) {
    EXPECT_EQ(completion.err, 0);
    EXPECT_EQ(completion.cancel, Cancel::None);
    EXPECT_EQ(completion.thread, 0);
  }
  std::lock_guard<std::mutex> lock(*order_mutex);
  EXPECT_EQ(*order, (std::vector<int64_t>{1, 2, 3}));
}

TEST(ReclaimPoolTest, TwoThreadsRunJobsAtTheSameTime) {
  auto gate = std::make_shared<Gate>();
  ReclaimPool pool(2, {.write = gatedWriter(gate)});
  auto owner = pool.registerOwner();

  pool.submit(owner, makeJob(1));
  pool.submit(owner, makeJob(2));
  // Both writes are inside the pool at once only if two threads run them.
  EXPECT_TRUE(gate->waitForArrivals(2));
  EXPECT_EQ(ReclaimPoolTestPeer::running(pool), 2);

  gate->release();
  EXPECT_EQ(waitForCompletions(pool, *owner, 2).size(), 2);
}

TEST(ReclaimPoolTest, NamesThreadsAfterThePool) {
  auto gate = std::make_shared<Gate>();
  auto names = std::make_shared<std::set<std::string>>();
  auto names_mutex = std::make_shared<std::mutex>();
  ReclaimPool pool(
      2,
      {.thread_name = "oomd-senpai",
       .write = [=](const MemoryReclaimRequest&, auto&) {
         char name[16] = {};
         ::pthread_getname_np(::pthread_self(), name, sizeof(name));
         {
           std::lock_guard<std::mutex> lock(*names_mutex);
           names->insert(name);
         }
         gate->arriveAndWait();
         return 0;
       }});
  auto owner = pool.registerOwner();

  pool.submit(owner, makeJob(1));
  pool.submit(owner, makeJob(2));
  ASSERT_TRUE(gate->waitForArrivals(2));
  gate->release();
  waitForCompletions(pool, *owner, 2);

  std::lock_guard<std::mutex> lock(*names_mutex);
  EXPECT_EQ(*names, (std::set<std::string>{"oomd-senpai-0", "oomd-senpai-1"}));
  EXPECT_EQ(pool.threadName(1), "oomd-senpai-1");
}

TEST(ReclaimPoolTest, StartsItsThreadsWhenCreated) {
  ReclaimPool pool(3, {.write = succeed});
  EXPECT_EQ(pool.threads(), 3);
}

TEST(ReclaimPoolTest, ThrowsIfNoThreadCanStart) {
  const ProcessLimit limit(0);
  try {
    std::thread([] {}).join();
    GTEST_SKIP() << "the process limit doesn't apply here (root or a "
                    "capability)";
  } catch (const std::system_error&) {
  }
  EXPECT_THROW(ReclaimPool(2, {.write = succeed}), std::system_error);
}

TEST(ReclaimPoolTest, ReportsWriteErrors) {
  ReclaimPool pool(1, {.write = [](const MemoryReclaimRequest& job, auto&) {
                     return static_cast<int>(job.bytes);
                   }});
  auto owner = pool.registerOwner();

  pool.submit(owner, makeJob(1, EAGAIN));
  pool.submit(owner, makeJob(2, ENOENT));
  pool.submit(owner, makeJob(3, EIO));
  const auto done = waitForCompletions(pool, *owner, 3);

  ASSERT_EQ(done.size(), 3);
  EXPECT_EQ(done[0].err, EAGAIN);
  EXPECT_EQ(done[1].err, ENOENT);
  EXPECT_EQ(done[2].err, EIO);
  for (const auto& completion : done) {
    EXPECT_EQ(completion.cancel, Cancel::None);
  }
}

TEST(ReclaimPoolTest, UpdateChangesOnlyQueuedJobs) {
  auto gate = std::make_shared<Gate>();
  using Seen = std::vector<std::pair<int64_t, std::optional<int64_t>>>;
  auto seen = std::make_shared<Seen>();
  auto seen_mutex = std::make_shared<std::mutex>();
  ReclaimPool pool(1, {.write = [=](const MemoryReclaimRequest& job, auto&) {
                     {
                       std::lock_guard<std::mutex> lock(*seen_mutex);
                       seen->emplace_back(job.bytes, job.swappiness);
                     }
                     gate->arriveAndWait();
                     return 0;
                   }});
  auto owner = pool.registerOwner();

  const auto running = pool.submit(owner, makeJob(1, 100));
  ASSERT_TRUE(gate->waitForArrivals(1));
  const auto queued = pool.submit(owner, makeJob(2, 200));
  EXPECT_FALSE(pool.update(running, 111, 10));
  EXPECT_TRUE(pool.update(queued, 222, 20));

  gate->release();
  const auto done = waitForCompletions(pool, *owner, 2);
  ASSERT_EQ(done.size(), 2);
  EXPECT_EQ(done[1].request.bytes, 222);
  std::lock_guard<std::mutex> lock(*seen_mutex);
  EXPECT_EQ(*seen, (Seen{{100, std::nullopt}, {222, 20}}));
}

TEST(ReclaimPoolTest, CancelledQueuedJobFinishesWithoutRunning) {
  auto gate = std::make_shared<Gate>();
  ReclaimPool pool(1, {.write = gatedWriter(gate)});
  auto owner = pool.registerOwner();

  pool.submit(owner, makeJob(1));
  ASSERT_TRUE(gate->waitForArrivals(1));
  const auto queued = pool.submit(owner, makeJob(2));

  pool.cancel(queued);
  const auto cancelled = pool.drain(*owner);
  ASSERT_EQ(cancelled.size(), 1);
  EXPECT_EQ(cancelled[0].id, queued);
  EXPECT_EQ(cancelled[0].err, ECANCELED);
  EXPECT_EQ(cancelled[0].cancel, Cancel::Requested);
  EXPECT_EQ(cancelled[0].thread, -1);
  EXPECT_EQ(cancelled[0].ran, 0ms);

  gate->release();
  const auto done = waitForCompletions(pool, *owner, 1);
  ASSERT_EQ(done.size(), 1);
  EXPECT_EQ(done[0].err, 0);
  EXPECT_EQ(done[0].cancel, Cancel::None);
}

TEST(ReclaimPoolTest, CancelInterruptsABlockedWrite) {
  // A full pipe makes a blocking write() sleep in the kernel, like a long
  // memory.reclaim. Since v7.2, memory.reclaim returns -ERESTARTSYS on a
  // signal, as a pipe write does, so this also checks that the cancel
  // handler has no SA_RESTART.
  int fds[2];
  ASSERT_EQ(::pipe2(fds, O_CLOEXEC), 0);
  const int flags = ::fcntl(fds[1], F_GETFL);
  ASSERT_EQ(::fcntl(fds[1], F_SETFL, flags | O_NONBLOCK), 0);
  char buf[4096] = {};
  while (::write(fds[1], buf, sizeof(buf)) > 0) {
  }
  ASSERT_EQ(::fcntl(fds[1], F_SETFL, flags), 0);

  auto gate = std::make_shared<Gate>();
  gate->release();
  const int write_fd = fds[1];
  ReclaimPool pool(
      1, {.write = [gate, write_fd](auto&, const std::atomic<Cancel>& cancel) {
        gate->arriveAndWait();
        while (!cancelled(cancel)) {
          if (::write(write_fd, "x", 1) >= 0) {
            return 0;
          }
          if (errno != EINTR) {
            return errno;
          }
        }
        return EINTR;
      }});
  auto owner = pool.registerOwner();

  const auto id = pool.submit(owner, makeJob(1));
  ASSERT_TRUE(gate->waitForArrivals(1));
  pool.cancel(id);
  const auto done = waitForCompletions(pool, *owner, 1);

  ASSERT_EQ(done.size(), 1);
  EXPECT_EQ(done[0].err, EINTR);
  EXPECT_EQ(done[0].cancel, Cancel::Requested);
  ::close(fds[0]);
  ::close(fds[1]);
}

TEST(ReclaimPoolTest, ExpiresWritesThatRunTooLong) {
  ReclaimPool pool(1, {.write = runUntilCancelled, .max_run = 50ms});
  auto owner = pool.registerOwner();

  pool.submit(owner, makeJob(1));
  pool.submit(owner, makeJob(2));
  // waitForCompletions calls supervise() as the event loop does.
  const auto done = waitForCompletions(pool, *owner, 2);

  // The second job finishing shows the thread moved on.
  ASSERT_EQ(done.size(), 2);
  for (const auto& completion : done) {
    EXPECT_EQ(completion.err, EINTR);
    EXPECT_EQ(completion.cancel, Cancel::Expired);
    EXPECT_GE(completion.ran, 50ms);
  }
}

TEST(ReclaimPoolTest, KeepsTheFirstCancelReason) {
  auto gate = std::make_shared<Gate>();
  ReclaimPool pool(1, {.write = stuckWriter(gate), .max_run = 0ms});
  auto owner = pool.registerOwner();
  const auto id = pool.submit(owner, makeJob(1));
  ASSERT_TRUE(gate->waitForArrivals(1));

  pool.supervise();
  pool.cancel(id);
  gate->release();
  const auto done = waitForCompletions(pool, *owner, 1);

  ASSERT_EQ(done.size(), 1);
  EXPECT_EQ(done[0].cancel, Cancel::Expired);
}

TEST(ReclaimPoolTest, DropsCompletionsForAGoneOwner) {
  auto gate = std::make_shared<Gate>();
  ReclaimPool pool(1, {.write = gatedWriter(gate)});
  auto owner = pool.registerOwner();
  auto other_owner = pool.registerOwner();

  pool.submit(owner, makeJob(1));
  ASSERT_TRUE(gate->waitForArrivals(1));
  owner.reset();
  gate->release();

  pool.submit(other_owner, makeJob(1));
  EXPECT_EQ(waitForCompletions(pool, *other_owner, 1).size(), 1);
}

TEST(ReclaimPoolTest, DestructorDoesNotWaitForAStuckWrite) {
  auto gate = std::make_shared<Gate>();
  auto pool = std::make_unique<ReclaimPool>(
      1, ReclaimPool::Options{.write = stuckWriter(gate)});
  auto owner = pool->registerOwner();
  pool->submit(owner, makeJob(1));
  ASSERT_TRUE(gate->waitForArrivals(1));

  const auto start = std::chrono::steady_clock::now();
  pool.reset();
  EXPECT_LT(std::chrono::steady_clock::now() - start, 3s);
  // Let the detached thread finish.
  gate->release();
}

TEST(ReclaimPoolTest, DefaultWriterWritesMemoryReclaim) {
  FakeCgroupDir dir;
  MemoryReclaimRequest request;
  request.cgroup_id = dir.inode();
  request.path = dir.path();
  request.bytes = 12345;
  request.swappiness = 60;
  std::atomic<Cancel> cancel{Cancel::None};

  EXPECT_EQ(ReclaimPool::writeMemoryReclaim(request, cancel), 0);
  EXPECT_EQ(dir.content(), "12345 swappiness=60");
}

TEST(ReclaimPoolTest, DefaultWriterRejectsAnotherCgroupAtThePath) {
  FakeCgroupDir dir;
  MemoryReclaimRequest request;
  // As if the cgroup was removed and another one created at the same path.
  request.cgroup_id = dir.inode() + 1;
  request.path = dir.path();
  request.bytes = 12345;
  std::atomic<Cancel> cancel{Cancel::None};

  EXPECT_EQ(ReclaimPool::writeMemoryReclaim(request, cancel), ENOENT);
  EXPECT_EQ(dir.content(), "");
}

TEST(ReclaimPoolTest, DefaultWriterDoesNotWriteOnceCancelled) {
  FakeCgroupDir dir;
  MemoryReclaimRequest request;
  request.cgroup_id = dir.inode();
  request.path = dir.path();
  request.bytes = 12345;
  std::atomic<Cancel> cancel{Cancel::Requested};

  EXPECT_EQ(ReclaimPool::writeMemoryReclaim(request, cancel), EINTR);
  EXPECT_EQ(dir.content(), "");
}
