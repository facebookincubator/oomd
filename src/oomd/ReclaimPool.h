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

#include <signal.h>
#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace Oomd {

// One memory.reclaim write
struct MemoryReclaimRequest {
  // The inode number of the cgroup's directory. The write fails with ENOENT
  // if the directory at path belongs to another cgroup by then.
  uint64_t cgroup_id{0};
  // Absolute path of the cgroup's directory
  std::string path;
  int64_t bytes{0};
  std::optional<int64_t> swappiness;
};

/*
 * Runs memory.reclaim writes on a few threads named <thread_name>-<i>, so the
 * event loop never waits for them. On some hosts one write takes 10-15 s.
 *
 * - Every method is called on the event-loop thread.
 * - Each plugin registers as an owner, submits requests, and drains its
 *   finished jobs. Jobs run in FIFO order on any idle thread. The queue has
 *   no limit: Senpai keeps at most one job per cgroup.
 * - A queued job's size and swappiness can change; a running job doesn't
 *   change.
 * - A request holds no file descriptor: the write opens the cgroup when it
 *   starts, so a long queue can't use up the fds that kills need.
 * - cancel() finishes a queued job right away. For a running job it sends
 *   kCancelSignal to the job's thread; memory.reclaim then returns EINTR at
 *   its next reclaim batch, and the job finishes when the write returns.
 *   supervise() sends the signal again, in case it landed just before the
 *   write started, and cancels writes that run longer than max_run.
 * - The destructor cancels everything and waits up to 1 s for the threads. A
 *   thread still stuck in the kernel is left to finish on its own, so the
 *   write function and everything it captures must outlive the pool. The pool
 *   must outlive the plugins that submit to it.
 */
class ReclaimPool {
 public:
  using JobId = uint64_t;

  // Why a job was cancelled. The first reason sticks.
  enum class CancelReason { None, Requested, Expired };

  struct Completion {
    JobId id{0};
    // The request as it ran, with any update()
    MemoryReclaimRequest request;
    // errno of the write, 0 on success. A write that succeeded is 0 even if
    // it was cancelled near its end.
    int err{0};
    CancelReason cancel{CancelReason::None};
    std::chrono::milliseconds queued{0};
    std::chrono::milliseconds ran{0};
    // Index of the thread that ran the job, -1 if it never ran
    int thread{-1};
  };

  // Performs one write and returns 0 or an errno. Once cancel is no longer
  // None and the thread gets kCancelSignal, it must return EINTR instead of
  // retrying.
  using Write = std::function<int(
      const MemoryReclaimRequest& request,
      const std::atomic<CancelReason>& cancel)>;

  struct Options {
    std::string thread_name{"oomd-reclaim"};
    // Empty means writeMemoryReclaim()
    Write write;
    std::chrono::milliseconds max_run{kMaxRun};
  };

  // A plugin's handle for its jobs
  class Owner;

  static constexpr int kCancelSignal = SIGUSR2;
  // Writes that run longer are cancelled and end as expired
  static constexpr std::chrono::seconds kMaxRun{60};

  // Starts the threads. If some fail to start the pool runs with the others;
  // if none starts, it throws std::system_error.
  explicit ReclaimPool(int threads);
  ReclaimPool(int threads, Options options);
  ~ReclaimPool();

  ReclaimPool(const ReclaimPool&) = delete;
  ReclaimPool& operator=(const ReclaimPool&) = delete;
  ReclaimPool(ReclaimPool&&) = delete;
  ReclaimPool& operator=(ReclaimPool&&) = delete;

  std::shared_ptr<Owner> registerOwner();

  // Queues a write and returns its job id.
  JobId submit(
      const std::shared_ptr<Owner>& owner,
      MemoryReclaimRequest request);

  // Changes a queued job's size and swappiness and keeps its place in the
  // queue. Returns false if the job is no longer queued.
  bool update(JobId id, int64_t bytes, std::optional<int64_t> swappiness);

  // Returns the owner's finished jobs and forgets them.
  std::vector<Completion> drain(Owner& owner);

  void cancel(JobId id);

  // Call every tick: sends kCancelSignal again to threads still running a
  // cancelled job, and cancels writes that have run longer than max_run.
  void supervise();

  // The number of threads that run
  int threads() const;
  // The name of the thread with this index: <thread_name>-<index>
  std::string threadName(int index) const;

  // The default write: checks that request.path is still the cgroup
  // request.cgroup_id and writes "<bytes>[ swappiness=<n>]" to its
  // memory.reclaim. It retries an EINTR that isn't a cancel and returns EINTR
  // once cancelled. Util::writeFull would retry every EINTR, restarting a
  // cancelled reclaim.
  static int writeMemoryReclaim(
      const MemoryReclaimRequest& request,
      const std::atomic<CancelReason>& cancel);

 private:
  friend class ReclaimPoolTestPeer;

  struct Counts {
    size_t queued{0};
    int running{0};
  };
  // For tests
  Counts counts() const;

  struct State;
  std::shared_ptr<State> state_;
};

} // namespace Oomd
