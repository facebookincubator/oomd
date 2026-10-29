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

#include "oomd/ReclaimPool.h"

#include <fcntl.h>
#include <pthread.h>
#include <sys/stat.h>
#include <unistd.h>
#include <algorithm>
#include <cerrno>
#include <condition_variable>
#include <deque>
#include <mutex>
#include <system_error>
#include <thread>
#include <utility>

#include "oomd/util/Fs.h"

namespace Oomd {

namespace {

using Clock = std::chrono::steady_clock;

std::chrono::milliseconds msBetween(
    Clock::time_point from,
    Clock::time_point to) {
  return std::chrono::duration_cast<std::chrono::milliseconds>(to - from);
}

// A handler for kCancelSignal that does nothing, without SA_RESTART, so that
// an interrupted write returns EINTR. Installing it again is harmless.
void installCancelHandler() {
  struct sigaction sa{};
  sa.sa_handler = [](int) {};
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  ::sigaction(ReclaimPool::kCancelSignal, &sa, nullptr);
}

// Names the calling thread and unblocks kCancelSignal in it. Linux allows 15
// characters; a longer name isn't set, and the thread keeps the process name.
void setUpThread(const std::string& name) {
  ::pthread_setname_np(::pthread_self(), name.c_str());
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, ReclaimPool::kCancelSignal);
  ::pthread_sigmask(SIG_UNBLOCK, &set, nullptr);
}

} // namespace

class ReclaimPool::Owner {
 public:
  // Guarded by State::mutex
  std::vector<Completion> completions;
};

struct ReclaimPool::State {
  struct Task {
    JobId id{0};
    MemoryReclaimRequest request;
    std::weak_ptr<Owner> owner;
    std::atomic<CancelReason> cancel{CancelReason::None};
    // The rest is guarded by mutex.
    Clock::time_point enqueued;
    Clock::time_point started;
    int thread{-1};
  };

  struct Worker {
    std::thread thread;
    // The job this thread runs. Its run() loop owns it, and clears this
    // under mutex before destroying it.
    Task* running{nullptr};
    bool exited{false};
  };

  State(int threads, Options o) : options(std::move(o)), workers(threads) {}

  const Options options;

  mutable std::mutex mutex;
  std::condition_variable work_cv;
  std::condition_variable exit_cv;
  std::deque<std::unique_ptr<Task>> queue;
  // Sized once: each thread keeps a reference to its slot.
  std::vector<Worker> workers;
  int started{0};
  JobId next_id{1};
  bool stopping{false};

  // All *Locked() methods require mutex.

  void finishLocked(const Task& task, int err, Clock::time_point now) {
    const auto owner = task.owner.lock();
    if (!owner) {
      return;
    }
    Completion completion;
    completion.id = task.id;
    completion.request = task.request;
    completion.err = err;
    completion.cancel = task.cancel.load();
    completion.thread = task.thread;
    if (task.thread < 0) {
      completion.queued = msBetween(task.enqueued, now);
    } else {
      completion.queued = msBetween(task.enqueued, task.started);
      completion.ran = msBetween(task.started, now);
    }
    owner->completions.push_back(std::move(completion));
  }

  // Keeps the first reason a running job was cancelled for, and signals its
  // thread.
  void cancelRunningLocked(Worker& worker, CancelReason reason) {
    auto none = CancelReason::None;
    worker.running->cancel.compare_exchange_strong(none, reason);
    ::pthread_kill(worker.thread.native_handle(), kCancelSignal);
  }

  void cancelQueuedLocked(
      const std::deque<std::unique_ptr<Task>>::iterator& it) {
    auto task = std::move(*it);
    queue.erase(it);
    task->cancel = CancelReason::Requested;
    finishLocked(*task, ECANCELED, Clock::now());
  }

  void run(int index) {
    setUpThread(options.thread_name + "-" + std::to_string(index));

    std::unique_lock<std::mutex> lock(mutex);
    auto& worker = workers.at(index);
    while (true) {
      work_cv.wait(lock, [&] { return stopping || !queue.empty(); });
      if (queue.empty()) {
        break;
      }
      auto task = std::move(queue.front());
      queue.pop_front();
      if (task->owner.expired()) {
        continue;
      }
      task->thread = index;
      task->started = Clock::now();
      worker.running = task.get();
      lock.unlock();

      int err = EINTR;
      if (task->cancel.load() == CancelReason::None) {
        try {
          err = options.write(task->request, task->cancel);
        } catch (...) {
          err = EIO;
        }
      }

      lock.lock();
      worker.running = nullptr;
      finishLocked(*task, err, Clock::now());
    }
    worker.exited = true;
    exit_cv.notify_all();
  }
};

bool ReclaimPool::blockCancelSignal() {
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, kCancelSignal);
  return ::pthread_sigmask(SIG_BLOCK, &set, nullptr) == 0;
}

ReclaimPool::ReclaimPool(int threads) : ReclaimPool(threads, Options{}) {}

ReclaimPool::ReclaimPool(int threads, Options options) {
  if (!options.write) {
    options.write = &ReclaimPool::writeMemoryReclaim;
  }
  installCancelHandler();
  state_ = std::make_shared<State>(std::max(1, threads), std::move(options));

  auto& s = *state_;
  std::lock_guard<std::mutex> lock(s.mutex);
  for (int i = 0; i < static_cast<int>(s.workers.size()); ++i) {
    auto& worker = s.workers.at(i);
    try {
      worker.thread = std::thread([state = state_, i] { state->run(i); });
      ++s.started;
    } catch (const std::system_error&) {
      worker.exited = true;
    }
  }
  if (s.started == 0) {
    throw std::system_error(
        EAGAIN, std::generic_category(), "no reclaim thread could start");
  }
}

ReclaimPool::~ReclaimPool() {
  // How long to wait for threads to finish their writes
  constexpr auto kStopWait = std::chrono::seconds(1);
  auto& s = *state_;
  std::vector<std::thread> to_join;
  std::vector<std::thread> to_detach;
  {
    std::unique_lock<std::mutex> lock(s.mutex);
    s.stopping = true;
    // Owners are normally gone by now, so queued jobs end without completions.
    s.queue.clear();
    for (auto& worker : s.workers) {
      if (worker.running != nullptr) {
        s.cancelRunningLocked(worker, CancelReason::Requested);
      }
    }
    s.work_cv.notify_all();
    s.exit_cv.wait_for(lock, kStopWait, [&] {
      return std::all_of(
          s.workers.begin(), s.workers.end(), [](const State::Worker& w) {
            return w.exited;
          });
    });
    for (auto& worker : s.workers) {
      if (worker.thread.joinable()) {
        (worker.exited ? to_join : to_detach)
            .push_back(std::move(worker.thread));
      }
    }
  }
  for (auto& thread : to_join) {
    thread.join();
  }
  // Joining a thread stuck in the kernel could block shutdown forever. Each
  // thread holds a reference to the state, so it can finish safely.
  for (auto& thread : to_detach) {
    // NOLINTNEXTLINE(facebook-hte-BadCall-detach)
    thread.detach();
  }
}

std::shared_ptr<ReclaimPool::Owner> ReclaimPool::registerOwner() {
  return std::make_shared<Owner>();
}

ReclaimPool::JobId ReclaimPool::submit(
    const std::shared_ptr<Owner>& owner,
    MemoryReclaimRequest request) {
  auto& s = *state_;
  std::lock_guard<std::mutex> lock(s.mutex);
  auto task = std::make_unique<State::Task>();
  task->id = s.next_id++;
  task->request = std::move(request);
  task->owner = owner;
  task->enqueued = Clock::now();
  const auto id = task->id;
  s.queue.push_back(std::move(task));
  s.work_cv.notify_one();
  return id;
}

bool ReclaimPool::update(
    JobId id,
    int64_t bytes,
    std::optional<int64_t> swappiness) {
  auto& s = *state_;
  std::lock_guard<std::mutex> lock(s.mutex);
  for (auto& task : s.queue) {
    if (task->id == id) {
      task->request.bytes = bytes;
      task->request.swappiness = swappiness;
      return true;
    }
  }
  return false;
}

std::vector<ReclaimPool::Completion> ReclaimPool::drain(Owner& owner) {
  std::lock_guard<std::mutex> lock(state_->mutex);
  return std::exchange(owner.completions, {});
}

void ReclaimPool::cancel(JobId id) {
  auto& s = *state_;
  std::lock_guard<std::mutex> lock(s.mutex);
  const auto it =
      std::find_if(s.queue.begin(), s.queue.end(), [&](const auto& task) {
        return task->id == id;
      });
  if (it != s.queue.end()) {
    s.cancelQueuedLocked(it);
    return;
  }
  for (auto& worker : s.workers) {
    if (worker.running != nullptr && worker.running->id == id) {
      s.cancelRunningLocked(worker, CancelReason::Requested);
      return;
    }
  }
}

void ReclaimPool::supervise() {
  auto& s = *state_;
  std::lock_guard<std::mutex> lock(s.mutex);
  const auto now = Clock::now();
  for (auto& worker : s.workers) {
    if (worker.running == nullptr) {
      continue;
    }
    if (worker.running->cancel.load() != CancelReason::None) {
      ::pthread_kill(worker.thread.native_handle(), kCancelSignal);
    } else if (now - worker.running->started >= s.options.max_run) {
      s.cancelRunningLocked(worker, CancelReason::Expired);
    }
  }
}

int ReclaimPool::threads() const {
  return state_->started;
}

std::string ReclaimPool::threadName(int index) const {
  return state_->options.thread_name + "-" + std::to_string(index);
}

ReclaimPool::Counts ReclaimPool::counts() const {
  auto& s = *state_;
  std::lock_guard<std::mutex> lock(s.mutex);
  Counts counts;
  counts.queued = s.queue.size();
  for (const auto& worker : s.workers) {
    counts.running += worker.running != nullptr ? 1 : 0;
  }
  return counts;
}

int ReclaimPool::writeMemoryReclaim(
    const MemoryReclaimRequest& request,
    const std::atomic<CancelReason>& cancel) {
  const int dirfd =
      ::open(request.path.c_str(), O_RDONLY | O_DIRECTORY | O_CLOEXEC);
  if (dirfd < 0) {
    return errno;
  }
  const Fs::Fd dir(dirfd);
  struct ::stat st{};
  if (::fstat(dirfd, &st) != 0) {
    return errno;
  }
  // The cgroup was removed and another one created at the same path.
  if (static_cast<uint64_t>(st.st_ino) != request.cgroup_id) {
    return ENOENT;
  }
  const int fd = ::openat(dirfd, Fs::kMemReclaimFile, O_WRONLY | O_CLOEXEC);
  if (fd < 0) {
    return errno;
  }
  const Fs::Fd file(fd);
  std::string line = std::to_string(request.bytes);
  if (request.swappiness) {
    line += " swappiness=" + std::to_string(*request.swappiness);
  }
  // Retry an EINTR only if it wasn't a cancel. A cgroup file takes a write
  // whole, and the rest of a short write would be a new request, so a short
  // write is an error.
  while (cancel.load() == CancelReason::None) {
    const ssize_t written = ::write(fd, line.data(), line.size());
    if (written == static_cast<ssize_t>(line.size())) {
      return 0;
    }
    if (written >= 0) {
      return EIO;
    }
    if (errno != EINTR) {
      return errno;
    }
  }
  return EINTR;
}

} // namespace Oomd
