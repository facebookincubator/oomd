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

#include "oomd/engine/BasePlugin.h"

namespace Oomd {

/*
 * This abstract base class provides an overridable set of methods that
 * enables reuse of kill mechanism code. All plugins that kill processes
 * need code that can traverse a cgroup directory and start killing PIDs.
 * There is no reason for every plugin to rewrite that code.
 *
 * Ideally all killing plugins should inherit from this base class.
 * If customized behavior is desired, then simply override the relevant
 * methods.
 */
class BaseKillPlugin : public Oomd::Engine::BasePlugin {
 protected:
  /*
   * Kills a cgroup
   *
   * @param cgroup_path is the absolute path to a cgroup (eg /sys/fs/cgroup/...)
   * @param recursive, if true, recursively kills every process (ie children
   * cgroups) starting at @param cgroup_path
   * @param dry sets whether or not we should actually issue SIGKILLs
   */
  virtual bool
  tryToKillCgroup(const std::string& cgroup_path, bool recursive, bool dry);

  /*
   * Sends SIGKILL to every PID in @param procs
   */
  virtual int tryToKillPids(const std::vector<int>& procs);

  /*
   * Sets the "trusted.oomd_kill" extended attribute key to @param
   * num_procs_killed on @param cgroup_path
   */
  virtual void reportToXattr(
      const std::string& cgroup_path,
      int num_procs_killed);

  /*
   * Logs a structured kill message to kmsg and stderr
   */
  virtual void logKill(
      const std::string& killed_group,
      const CgroupContext& context,
      bool dry = false) const;
};

} // namespace Oomd
