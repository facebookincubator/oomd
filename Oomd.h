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

#include <atomic>
#include <chrono>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "oomd/OomDetector.h"
#include "oomd/OomKiller.h"
#include "oomd/shared/KillList.h"
#include "oomd/shared/Tunables.h"

namespace Oomd {

struct Cgroup {
  Cgroup(std::unique_ptr<OomDetector> d, std::unique_ptr<OomKiller> k) {
    detector = std::move(d);
    killer = std::move(k);
  }

  std::unique_ptr<OomDetector> detector;
  std::unique_ptr<OomKiller> killer;
  OomdContext context;
};

class Oomd {
 public:
  Oomd() = default;
  virtual ~Oomd() = default;
  int run();

  virtual void setVerbose(bool b = true) {
    verbose_ = b;
  }
  virtual void setTunables(std::shared_ptr<Tunables> t) {
    tunables_ = std::move(t);
  }
  virtual void addCgroup(
      std::unique_ptr<OomDetector> d,
      std::unique_ptr<OomKiller> k) {
    cgroups_.emplace_back(std::make_unique<Cgroup>(std::move(d), std::move(k)));
  }

  bool prepareRun();
  bool registerHandlers() const;
  void updateTunables();
  void updateContext(const std::string& cgroup_path, OomdContext& ctx);

 private:
  // runtime settings
  std::chrono::seconds interval_{0};
  std::chrono::seconds post_kill_delay_{0};
  double average_size_decay_{0};
  int verbose_ticks_{60};

  // config args
  bool verbose_{false};
  std::shared_ptr<Tunables> tunables_;
  std::vector<std::unique_ptr<Cgroup>> cgroups_;

  bool warned_io_pressure_{false};
};

} // namespace Oomd
