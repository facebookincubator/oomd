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
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <iomanip>
#include <sstream>
#include <thread>
#include <unordered_set>

#include "oomd/Log.h"
#include "oomd/config/ConfigCompiler.h"
#include "oomd/config/JsonConfigParser.h"
#include "oomd/include/Assert.h"
#include "oomd/include/Defines.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

static constexpr auto kMaxEvents = 10;

namespace {
/*
 * Helper function that resolves a set of wildcarded cgroup paths.
 *
 * @returns a set of resolved cgroup paths
 */
std::unordered_set<Oomd::CgroupPath> resolveCgroupPaths(
    const std::unordered_set<Oomd::CgroupPath>& cgroups) {
  std::unordered_set<Oomd::CgroupPath> ret;

  for (const auto& cgroup : cgroups) {
    // TODO: see what the performance penalty of walking the FS
    // (in resolveWildcardPath) every time is. If it's a big penalty,
    // we could search `absolutePath` for any fnmatch operators and
    // only resolve if we find symbols.
    auto resolved_paths = Oomd::Fs::resolveWildcardPath(cgroup);

    for (const auto& resolved_path : resolved_paths) {
      size_t idx = 0;

      // Use idx to mark where non-cgroupfs part of path begins
      idx += cgroup.cgroupFs().size();
      if (resolved_path.size() > cgroup.cgroupFs().size()) {
        // Remove '/' after cgroupfs in path
        idx += 1;
      }

      if (idx <= resolved_path.size()) {
        std::string cgroup_relative = resolved_path.substr(idx);
        ret.emplace(cgroup.cgroupFs(), std::move(cgroup_relative));
      }
    }
  }

  return ret;
}
} // namespace

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
      drop_in_dir_(drop_in_dir),
      io_devs_(io_devs),
      hdd_coeffs_(hdd_coeffs),
      ssd_coeffs_(ssd_coeffs) {
  // Ensure that each monitored cgroup's cgroup fs is the same as the one
  // passed in by the command line
  if (engine_) { // Tests will pass in a nullptr
    // First ensure cgroup fs is uniform
    for (const auto& cgroup : engine_->getMonitoredResources()) {
      if (cgroup.cgroupFs() != cgroup_fs_) {
        resources_.emplace(cgroup_fs_, cgroup.relativePath());
      } else {
        resources_.emplace(cgroup);
      }
    }

    // Then make sure all parent cgroups are pulled in as well. This is
    // necessary so plugins can walk a prepopulated tree.
    for (const auto& cgroup : resources_) {
      if (cgroup.isRoot()) {
        continue;
      }

      CgroupPath parent = cgroup.getParent();
      while (!parent.isRoot()) {
        resources_.emplace(parent);
        parent = parent.getParent();
      }
    }
  }

  // Sanitize the drop in dir a little
  if (drop_in_dir_.size() && drop_in_dir_.at(drop_in_dir_.size() - 1) == '/') {
    // Delete the trailing '/'
    drop_in_dir_.erase(drop_in_dir.size() - 1);
  }
}

int64_t Oomd::calculateProtection(
    const CgroupPath& cgroup,
    OomdContext& ctx,
    std::unordered_map<CgroupPath, int64_t>& cache) {
  if (cache.find(cgroup) != cache.end()) {
    return cache.at(cgroup);
  }

  auto node = ctx.getCgroupNode(cgroup);
  OCHECK_EXCEPT(node, std::runtime_error("cgroup missing from OomdContext"));

  auto l_func = [](const CgroupContext& c) -> double {
    return std::min(c.current_usage, std::max(c.memory_min, c.memory_low));
  };

  std::function<double(const std::shared_ptr<CgroupNode>)> p_func =
      [&](const std::shared_ptr<CgroupNode> node) -> double {
    auto parent = node->parent.lock();
    if (parent->path.isRoot()) {
      // We're at a top level cgroup where P(cgrp) == L(cgrp)
      return l_func(node->ctx);
    }

    double l_sum_children = 0;
    for (const auto& child : parent->children) {
      l_sum_children += l_func(child->ctx);
    }

    // If the cgroup isn't using any memory then it's trivially true it's
    // not receiving any protection
    if (l_sum_children == 0) {
      return 0;
    }

    return std::min(
        l_func(node->ctx), p_func(parent) * l_func(node->ctx) / l_sum_children);
  };

  int64_t ret = p_func(node);
  cache[cgroup] = ret;

  return ret;
}

double Oomd::calculateIOCostCumulative(const IOStat& io_stat) {
  double cost_cumulative = 0.0;

  // calculate the sum of cumulative io cost on all devices.
  for (auto& stat : io_stat) {
    // only keep stats from devices we care
    if (io_devs_.find(stat.dev_id) == io_devs_.end()) {
      continue;
    }
    IOCostCoeffs coeffs;
    switch (io_devs_.at(stat.dev_id)) {
      case DeviceType::SSD:
        coeffs = ssd_coeffs_;
        break;
      case DeviceType::HDD:
        coeffs = hdd_coeffs_;
        break;
    }
    // Dot product between dev io stat and io cost coeffs. A more sensible way
    // is to do dot product between rate of change (bandwidth, iops) with coeffs
    // but since the coeffs are constant, we can calculate rate of change later.
    cost_cumulative += stat.rios * coeffs.read_iops +
        stat.rbytes * coeffs.readbw + stat.wios * coeffs.write_iops +
        stat.wbytes * coeffs.writebw + stat.dios * coeffs.trim_iops +
        stat.dbytes * coeffs.trimbw;
  }
  return cost_cumulative;
}

ResourcePressure Oomd::readIopressureWarnOnce(const std::string& path) {
  ResourcePressure io_pressure;
  // Older kernels don't have io.pressure, nan them out
  io_pressure = {std::nanf(""), std::nanf(""), std::nanf("")};

  try {
    if (!warned_io_pressure_.count(path)) {
      io_pressure = Fs::readIopressure(path);
    }
  } catch (const std::exception& ex) {
    warned_io_pressure_.emplace(path);
    OLOG << "IO pressure unavailable on " << path << ": " << ex.what();
  }

  return io_pressure;
}

bool Oomd::updateContextRoot(const CgroupPath& path, OomdContext& ctx) {
  // Note we not not collect _every_ piece of data that makes
  // sense for the root host. Feel free to add more as needed.
  auto pressures = Fs::readMempressure("/");
  auto io_pressure = readIopressureWarnOnce("/");
  auto current = Fs::readMemcurrent("/");

  ctx.setCgroupContext(
      path,
      {.pressure = pressures,
       .io_pressure = io_pressure,
       .current_usage = current});

  return true;
}

bool Oomd::updateContextCgroup(const CgroupPath& path, OomdContext& ctx) {
  std::string absolute_cgroup_path = path.absolutePath();

  // Warn once if memory controller is not enabled on target cgroup
  auto controllers = Fs::readControllers(absolute_cgroup_path);
  if (!std::any_of(controllers.begin(), controllers.end(), [](std::string& s) {
        return s == "memory";
      })) {
    if (!warned_mem_controller_.count(absolute_cgroup_path)) {
      OLOG << "WARNING: cgroup memory controller not enabled on "
           << absolute_cgroup_path << ". oomd will ignore it.";
      warned_mem_controller_.emplace(absolute_cgroup_path);
    }

    // Can't extract much info if memory controller isn't enabled
    return false;
  }

  auto current = Fs::readMemcurrent(absolute_cgroup_path);
  auto pressures = Fs::readMempressure(absolute_cgroup_path);
  auto memlow = Fs::readMemlow(absolute_cgroup_path);
  auto memmin = Fs::readMemmin(absolute_cgroup_path);
  auto memhigh = Fs::readMemhigh(absolute_cgroup_path);
  auto memmax = Fs::readMemmax(absolute_cgroup_path);
  auto swap_current = Fs::readSwapCurrent(absolute_cgroup_path);
  auto memory_stats = Fs::getMemstat(absolute_cgroup_path);
  auto anon_usage = memory_stats["anon"];
  auto io_pressure = readIopressureWarnOnce(absolute_cgroup_path);
  int64_t memhigh_tmp = 0;
  try {
    memhigh_tmp = Fs::readMemhightmp(absolute_cgroup_path);
  } catch (const Fs::bad_control_file& ex) {
  }
  double io_cost_cumulative = 0;
  if (io_devs_.size() != 0) {
    IOStat io_stat;
    if (std::any_of(controllers.begin(), controllers.end(), [](std::string& s) {
          return s == "io";
        })) {
      io_stat = Fs::readIostat(absolute_cgroup_path);
    } else {
      if (!warned_io_stat_.count(absolute_cgroup_path)) {
        warned_io_stat_.emplace(absolute_cgroup_path);
        OLOG << "WARNING: cgroup io controller unavailable on "
             << absolute_cgroup_path << ". io cost will be zero.";
      }
      io_stat = {};
    }
    io_cost_cumulative = calculateIOCostCumulative(io_stat);
  }

  ctx.setCgroupContext(
      path,
      {.pressure = pressures,
       .io_pressure = io_pressure,
       .current_usage = current,
       .memory_low = memlow,
       .swap_usage = swap_current,
       .anon_usage = anon_usage,
       .memory_min = memmin,
       .memory_high = memhigh,
       .memory_high_tmp = memhigh_tmp,
       .memory_max = memmax,
       .io_cost_cumulative = io_cost_cumulative});

  return true;
}

void Oomd::updateContext(
    const std::unordered_set<CgroupPath>& cgroups,
    OomdContext& ctx) {
  OomdContext new_ctx;
  // Caching results helps reduce tree walks
  std::unordered_map<CgroupPath, int64_t> protection_cache;

  auto resolved = resolveCgroupPaths(cgroups);
  for (const auto& resolved_cgroup : resolved) {
    // Only care about subtree cgroups, not the cgroup files
    if (!Fs::isDir(resolved_cgroup.absolutePath())) {
      continue;
    }

    // Despite checking just above if the cgroup directory is valid,
    // we can still race with a cgroup disappearing. When that happens,
    // simply abort updating the cgroup.
    try {
      if (resolved_cgroup.isRoot()) {
        updateContextRoot(resolved_cgroup, new_ctx);
      } else {
        updateContextCgroup(resolved_cgroup, new_ctx);
      }
    } catch (const Fs::bad_control_file& ex) {
      OLOG << "Caught bad_control_file: " << ex.what() << ". This is OK -- "
           << "cgroups can disappear when a managed process exits";
      continue;
    }
  }

  // Update values that depend on the rest of OomdContext being up to date
  for (auto& key : new_ctx.cgroups()) {
    // This information isn't really relevant to the root host. Skip it.
    if (key.isRoot()) {
      continue;
    }

    auto new_cgroup_ctx = new_ctx.getCgroupContext(key); // copy

    float prev_avg = 0;
    // make the initial io cost zero
    double prev_io_cost_cum = new_cgroup_ctx.io_cost_cumulative;
    if (ctx.hasCgroupContext(key)) {
      prev_avg = ctx.getCgroupContext(key).average_usage;
      prev_io_cost_cum = ctx.getCgroupContext(key).io_cost_cumulative;
    }

    // Update running average
    new_cgroup_ctx.average_usage =
        prev_avg * ((average_size_decay_ - 1) / average_size_decay_) +
        (new_cgroup_ctx.current_usage / average_size_decay_);

    // Update io cost
    new_cgroup_ctx.io_cost_rate =
        (new_cgroup_ctx.io_cost_cumulative - prev_io_cost_cum) /
        interval_.count();

    // Update protection overage
    new_cgroup_ctx.memory_protection =
        calculateProtection(key, new_ctx, protection_cache);

    new_ctx.setCgroupContext(key, new_cgroup_ctx);
  }

  // Update information about swapfree
  SystemContext system_ctx;
  auto swaps = Fs::readFileByLine("/proc/swaps");

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

  new_ctx.setSystemContext(system_ctx);
  // update object state
  ctx = std::move(new_ctx);
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

  auto unit = Config2::compileDropIn(*ir_root_, *dropin_root);
  if (!unit.has_value()) {
    OLOG << "Could not compile drop in config";
    OLOG << "Failed to inject drop in config into engine";
    return;
  }

  size_t tag = std::hash<std::string>{}(file);
  for (size_t i = 0; i < unit->rulesets.size(); ++i) {
    if (engine_->addDropInConfig(tag, std::move(unit->rulesets.at(i)))) {
      resources_.insert(unit->resources.cbegin(), unit->resources.cend());
    } else {
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
  OomdContext ctx;
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

      updateContext(resources_, ctx);

      // Run all the plugins
      engine_->runOnce(ctx);

    } catch (const std::exception& ex) {
      OLOG << "Caught exception: " << ex.what();
      return 1;
    }
  }

  return 0;
}

} // namespace Oomd
