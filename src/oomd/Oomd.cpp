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

#include "oomd/Oomd.h"

#include <signal.h>
#include <unistd.h>

#include <sys/epoll.h>
#include <sys/inotify.h>
#include <sys/timerfd.h>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <thread>

#include "oomd/Log.h"
#include "oomd/PluginConstructionContext.h"
#include "oomd/config/ConfigCompiler.h"
#include "oomd/config/JsonConfigParser.h"
#include "oomd/include/Assert.h"
#include "oomd/include/Defines.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

static constexpr auto kMaxEvents = 10;

namespace Oomd {

Oomd::Oomd(
    std::unique_ptr<Config2::IR::Root> ir_root,
    std::unique_ptr<Engine::Engine> engine,
    int interval,
    const std::string& cgroup_fs,
    const std::string& drop_in_dir,
    const std::unordered_map<std::string, DeviceType>& io_devs,
    const IOCostCoeffs& hdd_coeffs,
    const IOCostCoeffs& ssd_coeffs)
    : interval_(interval),
      cgroup_fs_(cgroup_fs),
      ir_root_(std::move(ir_root)),
      engine_(std::move(engine)),
      drop_in_dir_(drop_in_dir) {
  ContextParams params{
      .io_devs = io_devs,
      .hdd_coeffs = hdd_coeffs,
      .ssd_coeffs = ssd_coeffs,
  };
  ctx_ = OomdContext(params);
  // Sanitize the drop in dir a little
  if (drop_in_dir_.size() && drop_in_dir_.at(drop_in_dir_.size() - 1) == '/') {
    // Delete the trailing '/'
    drop_in_dir_.erase(drop_in_dir.size() - 1);
  }
}

void Oomd::updateContext() {
  // Update information about swapfree
  SystemContext system_ctx;
  auto swaps =
      Fs::readFileByLine("/proc/swaps").value_or(std::vector<std::string>{});
  // For each swap, tally up used and total
  for (size_t i = 1; i < swaps.size(); ++i) {
    auto parts = Util::split(swaps[i], '\t');
    // The /proc/swaps format is pretty bad. The first field is padded by
    // spaces but the rest of the fields are padded by '\t'. Since we don't
    // really care about the first field, we'll just split by '\t'.
    OCHECK_EXCEPT(
        parts.size() == 4, std::runtime_error("/proc/swaps malformed"));
    system_ctx.swaptotal += std::stoll(parts[1]) * 1024; // Values are in KB
    system_ctx.swapused += std::stoll(parts[2]) * 1024; // Values are in KB
  }

  ctx_.setSystemContext(system_ctx);
  ctx_.refresh();
}
int Oomd::deregisterDropInWatcherFromEventLoop() {
  char buf[1024];
  int ret = 0;

  // Remove watch for stale fd and wd and reset them to their
  // initial values. Caveat: If we try to remove the watch for
  // drop_in_dir that has been deleted, inotify_rm_watch() returns
  // EINVAL as the watch is automatically deleted when the directory
  // was removed. Only remove watches if they're files in the directory
  // and not the directory itself.
  if (!drop_in_dir_deleted_ && ::inotify_rm_watch(inotifyfd_, inotifywd_) < 0) {
    OLOG << "inotify_rm_watch: " << ::strerror_r(errno, buf, sizeof(buf));
    ret = 1;
  }
  if (::epoll_ctl(epollfd_, EPOLL_CTL_DEL, inotifyfd_, nullptr) < 0) {
    OLOG << "epoll_ctl: " << ::strerror_r(errno, buf, sizeof(buf));
    ret = 1;
  }

  inotifyfd_ = -1;
  inotifywd_ = -1;

  return ret;
}

int Oomd::prepDropInWatcherEventLoop(const std::string& dir) {
  char buf[1024];

  inotifyfd_ = ::inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
  if (inotifyfd_ < 0) {
    OLOG << "inotify_init1: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  uint32_t mask = IN_DELETE | IN_MODIFY | IN_MOVE | IN_ONLYDIR | IN_MOVE_SELF |
      IN_DELETE_SELF;
  if ((inotifywd_ = ::inotify_add_watch(inotifyfd_, dir.c_str(), mask)) < 0) {
    OLOG << "inotify_add_watch: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  // Add inotifyfd to epoll set
  struct epoll_event ev;
  std::memset(&ev, 0, sizeof(ev));
  ev.events = EPOLLIN;
  ev.data.fd = inotifyfd_;
  if (::epoll_ctl(epollfd_, EPOLL_CTL_ADD, inotifyfd_, &ev) < 0) {
    OLOG << "epoll_ctl: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  return 0;
}

int Oomd::prepDropInWatcher(const std::string& dir) {
  if (!Fs::isDir(dir)) {
    OLOG << "Error: " << dir << " is not a directory";
    return 1;
  }

  // Load existing drop in configs before setting up inotify hooks
  // so we don't race with configs dropping in
  auto de = Fs::readDir(dir, Fs::DE_FILE);
  std::sort(de.files.begin(), de.files.end()); // Provide some determinism
  for (const auto& config : de.files) {
    processDropInAdd(config);
  }

  if (prepDropInWatcherEventLoop(dir)) {
    return 1;
  }

  return 0;
}

int Oomd::prepEventLoop(const std::chrono::seconds& interval) {
  char buf[1024];

  timerfd_ = ::timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
  if (timerfd_ < 0) {
    OLOG << "timerfd_create: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  struct itimerspec ts;
  std::memset(&ts, 0, sizeof(ts));
  ts.it_value.tv_sec = interval.count(); // initial expiration
  ts.it_interval.tv_sec = interval.count(); // periodic expiration

  if (::timerfd_settime(timerfd_, 0, &ts, nullptr) < 0) {
    OLOG << "timerfd_settime: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  epollfd_ = ::epoll_create1(EPOLL_CLOEXEC);
  if (epollfd_ < 0) {
    OLOG << "epoll_create1: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  // Add timerfd to epoll set
  struct epoll_event ev;
  std::memset(&ev, 0, sizeof(ev));
  ev.events = EPOLLIN;
  ev.data.fd = timerfd_;
  if (::epoll_ctl(epollfd_, EPOLL_CTL_ADD, timerfd_, &ev) < 0) {
    OLOG << "epoll_ctl: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  // Set up drop in config watcher if necessary
  if (drop_in_dir_.size()) {
    int ret = prepDropInWatcher(drop_in_dir_);
    if (ret) {
      return ret;
    }
  }

  return 0;
}

void Oomd::processDropInRemove(const std::string& file) {
  // Ignore dot files
  if (file.empty() || (file.size() && file.at(0) == '.')) {
    return;
  }

  OLOG << "Removing drop in config=" << file;
  size_t tag = std::hash<std::string>{}(file);
  engine_->removeDropInConfig(tag);
}

void Oomd::processDropInAdd(const std::string& file) {
  // Ignore dot files
  if (file.empty() || (file.size() && file.at(0) == '.')) {
    return;
  }

  // First remove then re-add. We don't do in place modifications as it'll
  // be complicated for the code and it probably wouldn't be what the user
  // expects. The user probably expects the entire drop in config is reset
  // and added to the front of the LIFO queue.
  processDropInRemove(file);

  OLOG << "Adding drop in config=" << file;

  std::ifstream dropin_file(drop_in_dir_ + '/' + file, std::ios::in);
  if (!dropin_file.is_open()) {
    OLOG << "Could not open drop in config=" << file;
    return;
  }
  std::stringstream buf;
  buf << dropin_file.rdbuf();
  Config2::JsonConfigParser json_parser;
  std::unique_ptr<Config2::IR::Root> dropin_root;
  try {
    dropin_root = json_parser.parse(buf.str());
  } catch (const std::exception& e) {
    OLOG << "Caught: " << e.what();
    OLOG << "Failed to inject drop in config into engine";
    return;
  }
  if (!dropin_root) {
    OLOG << "Could not parse drop in config=" << file;
    OLOG << "Failed to inject drop in config into engine";
    return;
  }

  const PluginConstructionContext compile_context(cgroup_fs_);
  auto unit = Config2::compileDropIn(*ir_root_, *dropin_root, compile_context);
  if (!unit.has_value()) {
    OLOG << "Could not compile drop in config";
    OLOG << "Failed to inject drop in config into engine";
    return;
  }

  size_t tag = std::hash<std::string>{}(file);
  for (size_t i = 0; i < unit->rulesets.size(); ++i) {
    if (!engine_->addDropInConfig(tag, std::move(unit->rulesets.at(i)))) {
      OLOG << "Failed to inject drop in config into engine";
      return;
    }
  }
}

int Oomd::processDropInWatcher(int fd) {
  const struct inotify_event* event;
  char buf[4096] __attribute__((aligned(__alignof__(struct inotify_event))));

  while (true) {
    int len = ::read(fd, buf, sizeof(buf));
    if (len < 0 && errno != EAGAIN) {
      OLOG << "read: " << ::strerror_r(errno, buf, sizeof(buf));
      return 1;
    }

    if (len <= 0) {
      break;
    }

    for (char* ptr = buf; ptr < (buf + len);
         ptr += sizeof(struct inotify_event) + event->len) {
      event = reinterpret_cast<const struct inotify_event*>(ptr);

      if (event->mask & (IN_MOVED_TO | IN_MODIFY)) {
        // Remove and re-add drop in if a file has been added to the
        // watched directory
        processDropInAdd(event->name);
      } else if (event->mask & (IN_DELETE | IN_MOVED_FROM)) {
        // Remove drop in if file has been moved from or removed from
        // the watched directory
        processDropInRemove(event->name);
      } else if (event->mask & (IN_DELETE_SELF | IN_MOVE_SELF)) {
        // Remove stale watch descriptor for drop in if watched file or
        // directory itself is moved or deleted
        drop_in_dir_deleted_ = true;
        if (deregisterDropInWatcherFromEventLoop()) {
          return 1;
        }
      }
    }
  }

  return 0;
}

int Oomd::processEventLoop() {
  struct epoll_event events[kMaxEvents];
  char buf[1024];
  int ret;

  int n;
  do {
    n = ::epoll_wait(epollfd_, events, kMaxEvents, -1);
  } while (n < 0 && errno == EINTR);
  if (n < 0) {
    OLOG << "epoll_wait: " << ::strerror_r(errno, buf, sizeof(buf));
    return 1;
  }

  for (int i = 0; i < n; ++i) {
    int fd = events[i].data.fd;
    if (fd == timerfd_) {
      uint64_t expired = 0;
      ret = ::read(fd, &expired, sizeof(expired));
      if (ret < 0) {
        OLOG << "read: " << ::strerror_r(errno, buf, sizeof(buf));
        return 1;
      }
      // If drop_in_dir_ exists again and contains config files,
      // set up the watch
      if (drop_in_dir_deleted_ && Fs::isDir(drop_in_dir_)) {
        ret = prepDropInWatcher(drop_in_dir_);
        if (ret) {
          return ret;
        }
        drop_in_dir_deleted_ = false;
      }
    } else if (fd == inotifyfd_) {
      ret = processDropInWatcher(fd);
      if (ret) {
        return ret;
      }
    } else {
      OLOG << "Unknown fd=" << fd << " in event loop";
      return 1;
    }
  }

  return 0;
}

int Oomd::run() {
  int ret;

  if (!engine_) {
    OLOG << "Could not run engine. Your config file is probably invalid\n";
    return EXIT_CANT_RECOVER;
  }

  ret = prepEventLoop(interval_);
  if (ret) {
    return ret;
  }

  OLOG << "Running oomd";

  while (true) {
    try {
      // Sleeps and handles events
      ret = processEventLoop();
      if (ret) {
        return ret;
      }

      updateContext();

      // Prerun all the plugins
      engine_->prerun(ctx_);

      // Run all the plugins
      engine_->runOnce(ctx_);

    } catch (const std::exception& ex) {
      // In case logging was disabled before exception is thrown
      OLOG << LogStream::Control::ENABLE;
      OLOG << "Caught exception: " << ex.what();
      return 1;
    }
  }

  return 0;
}

} // namespace Oomd
