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

#include <functional>
#include <vector>

#include "oomd/shared/OomdContext.h"
#include "oomd/shared/Plugin.h"

namespace Oomd {

class OomKiller {
 public:
  OomKiller() = delete;
  explicit OomKiller(const PluginArgs& args);
  virtual ~OomKiller() = default;
  virtual bool tryToKillSomething(OomdContext& ctx);
  virtual bool tryToKillCgroup(const std::string& id);
  virtual bool tryToKillCgroupRecursively(const std::string& id);
  virtual int tryToKillPids(const std::vector<int>& procs);
  virtual void reportToXattr(const std::string& id, int num_procs_killed);

  static OomKiller* create(const PluginArgs& args) {
    return new OomKiller(args);
  }

 protected:
  bool isBlacklisted(const std::string& target);
  void removeBlacklisted(
      std::vector<std::pair<std::string, CgroupContext>>& list);

  std::string cgroup_path_;
  std::shared_ptr<KillList> kill_list_;
  std::shared_ptr<Tunables> tunables_;
  bool dry_;
};

} // namespace Oomd
