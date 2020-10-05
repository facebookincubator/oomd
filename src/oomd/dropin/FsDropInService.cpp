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

#include "oomd/dropin/FsDropInService.h"

#include <signal.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/inotify.h>
#include <unistd.h>

#include <cstring>
#include <fstream>

#include "oomd/Log.h"
#include "oomd/config/JsonConfigParser.h"
#include "oomd/include/Assert.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

static constexpr auto kMaxEvents = 10;

namespace Oomd {

std::unique_ptr<FsDropInService> FsDropInService::create(
    const std::string& cgroup_fs,
    const Config2::IR::Root& root,
    Engine::Engine& engine,
    const std::string& drop_in_dir) {
  if (drop_in_dir.size() == 0) {
    return nullptr;
  }
  Fs::Fd epollfd(::epoll_create1(EPOLL_CLOEXEC));
  if (epollfd.fd() < 0) {
    OLOG << "epoll_create1: " << Util::strerror_r();
    return nullptr;
  }
  Fs::Fd terminatefd(::eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK));
  if (terminatefd.fd() < 0) {
    OLOG << "eventfd: " << Util::strerror_r();
    return nullptr;
  }
  struct epoll_event ev;
  ev.events = EPOLLIN;
  ev.data.fd = terminatefd.fd();
  if (::epoll_ctl(epollfd.fd(), EPOLL_CTL_ADD, terminatefd.fd(), &ev) < 0) {
    OLOG << "epoll_ctl: " << Util::strerror_r();
    return nullptr;
  }
  return std::make_unique<FsDropInService>(
      Tag{},
      cgroup_fs,
      root,
      engine,
      drop_in_dir,
      std::move(epollfd).fd(),
      std::move(terminatefd).fd());
}

FsDropInService::FsDropInService(
    const Tag&,
    const std::string& cgroup_fs,
    const Config2::IR::Root& root,
    Engine::Engine& engine,
    const std::string& drop_in_dir,
    int epollfd,
    int terminatefd)
    : DropInServiceAdaptor(cgroup_fs, root, engine),
      epollfd_(epollfd),
      terminatefd_(terminatefd),
      drop_in_dir_(drop_in_dir) {
  // Sanitize the drop in dir a little
  if (drop_in_dir_.size() && drop_in_dir_.back() == '/') {
    // Delete the trailing '/'
    drop_in_dir_.pop_back();
  }
  if (prepDropInWatcher(drop_in_dir_)) {
    drop_in_dir_deleted_ = true;
  }
  event_loop_ = std::thread(&FsDropInService::run, this);
}

FsDropInService::~FsDropInService() {
  uint64_t val = 1;
  if (sizeof(val) == ::write(terminatefd_, &val, sizeof(val))) {
    event_loop_.join();
  } else {
    OLOG << "Failed to join event loop thread";
  }
  ::close(terminatefd_);
  if (!drop_in_dir_deleted_) {
    // May race but we don't really care
    deregisterDropInWatcherFromEventLoop();
  }
  ::close(epollfd_);
}

void FsDropInService::tick() {
  if (drop_in_dir_deleted_) {
    if (prepDropInWatcher(drop_in_dir_) == 0) {
      drop_in_dir_deleted_ = false;
    }
  }
}

void FsDropInService::handleDropInAddResult(const std::string& tag, bool ok) {
  if (ok) {
    OLOG << "Drop in config=" << tag << " injected into engine";
  } else {
    OLOG << "Failed to inject drop in config=" << tag << " into engine";
  }
}

void FsDropInService::handleDropInRemoveResult(
    const std::string& tag,
    bool ok) {
  if (ok) {
    OLOG << "Drop in config=" << tag << " removed from engine";
  } else {
    OLOG << "Failed to remove drop in config=" << tag << " from engine";
  }
}

int FsDropInService::deregisterDropInWatcherFromEventLoop() {
  int ret = 0;

  if (::epoll_ctl(epollfd_, EPOLL_CTL_DEL, inotifyfd_, nullptr) < 0) {
    OLOG << "epoll_ctl: " << Util::strerror_r();
    ret = 1;
  }

  ::close(inotifyfd_);
  inotifyfd_ = -1;
  inotifywd_ = -1;

  return ret;
}

int FsDropInService::prepDropInWatcherEventLoop(const std::string& dir) {
  inotifyfd_ = ::inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
  if (inotifyfd_ < 0) {
    OLOG << "inotify_init1: " << Util::strerror_r();
    return 1;
  }

  uint32_t mask = IN_DELETE | IN_MODIFY | IN_MOVE | IN_ONLYDIR | IN_MOVE_SELF |
      IN_DELETE_SELF;
  if ((inotifywd_ = ::inotify_add_watch(inotifyfd_, dir.c_str(), mask)) < 0) {
    OLOG << "inotify_add_watch: " << Util::strerror_r();
    return 1;
  }

  // Add inotifyfd to epoll set
  struct epoll_event ev;
  std::memset(&ev, 0, sizeof(ev));
  ev.events = EPOLLIN;
  ev.data.fd = inotifyfd_;
  if (::epoll_ctl(epollfd_, EPOLL_CTL_ADD, inotifyfd_, &ev) < 0) {
    OLOG << "epoll_ctl: " << Util::strerror_r();
    return 1;
  }

  return 0;
}

int FsDropInService::prepDropInWatcher(const std::string& dir) {
  if (!Fs::isDir(dir)) {
    OLOG << "Error: " << dir << " is not a directory";
    return 1;
  }

  /*
   * This lock blocks the event loop from processing incoming watcher events,
   * but allows them to become pending. Then we add all existing drop in configs
   * in the directory. This ensures us not missing any drop in event after we
   * add the existing ones, although some may overlap, i.e. file A added to
   * directory triggers inotify and we also consider it an "existing" file. This
   * causes us to add the same file twice, but the net effect is the same.
   * Drop in removal is fine too, as both add and remove are idempotent.
   */
  std::lock_guard<std::mutex> lock(event_loop_mutex_);
  if (prepDropInWatcherEventLoop(dir)) {
    return 1;
  }

  auto de = Fs::readDir(dir, Fs::DE_FILE);
  std::sort(de.files.begin(), de.files.end()); // Provide some determinism
  for (const auto& config : de.files) {
    processDropInAdd(config);
  }

  return 0;
}

void FsDropInService::processDropInRemove(const std::string& file) {
  // Ignore dot files
  if (file.empty() || (file.size() && file.at(0) == '.')) {
    return;
  }

  OLOG << "Removing drop in config=" << file;
  scheduleDropInRemove(file);
}

void FsDropInService::processDropInAdd(const std::string& file) {
  // Ignore dot files
  if (file.empty() || (file.size() && file.at(0) == '.')) {
    return;
  }

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

  if (!scheduleDropInAdd(file, *dropin_root)) {
    OLOG << "Could not compile drop in config";
    OLOG << "Failed to inject drop in config into engine";
  }
}

int FsDropInService::processDropInWatcher(int fd) {
  const struct inotify_event* event;
  alignas(struct inotify_event) std::array<char, 4096> buf;

  while (true) {
    int len = ::read(fd, buf.data(), sizeof(buf));
    if (len < 0 && errno != EAGAIN) {
      OLOG << "read: " << Util::strerror_r();
      return 1;
    }

    if (len <= 0) {
      break;
    }

    for (char* ptr = buf.data(); ptr < (buf.data() + len);
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
        if (deregisterDropInWatcherFromEventLoop()) {
          return 1;
        }
        drop_in_dir_deleted_ = true;
        return 0;
      }
    }
  }

  return 0;
}

int FsDropInService::processEventLoop() {
  std::array<struct epoll_event, kMaxEvents> events;

  int n;
  do {
    n = ::epoll_wait(epollfd_, events.data(), kMaxEvents, -1);
  } while (n < 0 && errno == EINTR);
  if (n < 0) {
    OLOG << "epoll_wait: " << Util::strerror_r();
    return 1;
  }

  // This will only contend when drop in dir is recreated and some event fires,
  // which is very rare. See comment above in prepDropInWatcher().
  std::lock_guard<std::mutex> lock(event_loop_mutex_);

  for (int i = 0; i < n; ++i) {
    int fd = events[i].data.fd;
    if (fd == terminatefd_) {
      uint64_t val;
      ssize_t len = ::read(fd, &val, sizeof(val));
      if (len != sizeof(val)) {
        OLOG << "read: " << Util::strerror_r();
        return 1;
      }
      if (val != 1) {
        OLOG << "Unexpected terminatefd value=" << val;
        return 1;
      }
      // Special value for termination
      return 2;
    } else if (fd == inotifyfd_) {
      if (processDropInWatcher(fd)) {
        return 1;
      }
    } else {
      OLOG << "Unknown fd=" << fd << " in event loop";
      return 1;
    }
  }

  return 0;
}

void FsDropInService::run() {
  int ret = 0;
  while (ret != 2) {
    ret = processEventLoop();

    // Crash for unrecoverable errors
    OCHECK(ret != 1);
  }
}

} // namespace Oomd
