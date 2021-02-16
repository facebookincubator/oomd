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

#include <chrono>
#include <map>
#include <unordered_set>

#include "oomd/engine/BasePlugin.h"

namespace Oomd {

template <class T>
struct CgroupState;

// Common base class for Senpai plugins to inherit from
template <class T>
class SenpaiCommon : public Engine::BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& ctx) override;

  virtual ~SenpaiCommon() override = default;

 protected:
  std::ostream& log(std::ostream& os, CgroupState<T>& state);

  std::optional<bool> hasMemoryHighTmp(const CgroupContext& cgroup_ctx);
  std::optional<int64_t> readMemhigh(const CgroupContext& cgroup_ctx);
  bool writeMemhigh(const CgroupContext& cgroup_ctx, int64_t value);
  bool writeMemhighTimeout(
      const CgroupContext& cgroup_ctx,
      int64_t value,
      std::chrono::milliseconds timeout);
  bool resetMemhigh(const CgroupContext& cgroup_ctx);
  SystemMaybe<int64_t> getReclaimableBytes(const CgroupContext& cgroup_ctx);
  std::optional<int64_t> getLimitMinBytes(const CgroupContext& cgroup_ctx);
  std::optional<int64_t> getLimitMaxBytes(const CgroupContext& cgroup_ctx);

  std::unordered_set<CgroupPath> cgroups_;
  std::map<CgroupContext::Id, CgroupState<T>> tracked_cgroups_;

  std::optional<bool> has_memory_high_tmp_{};
  int64_t host_mem_total_{0};

  // cgroup size limits
  int64_t limit_min_bytes_{100ull << 20};
  int64_t limit_max_bytes_{10ull << 30};

  std::chrono::milliseconds memory_high_timeout_{};
  // TODO(dschatzberg): These two knobs are used by both algorithms,
  // but not in the same way - they should probably be separated out
  // into separate knobs.

  // - max_probe is reached when stalling falls below pressure /
  // coeff_probe
  double max_probe_{0.01};
  // pressure target - stall time over sampling period
  int64_t interval_{6};

  int64_t log_interval_{60};
  int64_t log_ticks_{0};

  bool verbose_{false};
};

} // namespace Oomd

#include "oomd/plugins/SenpaiCommon-inl.h"
