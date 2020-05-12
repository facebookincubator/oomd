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

#include <deque>
#include <memory>
#include <optional>
#include <unordered_map>

#include "oomd/include/CgroupPath.h"
#include "oomd/include/Types.h"
#include "oomd/util/Fs.h"

namespace Oomd {

class OomdContext;
/*
 * Storage class for cgroup states. Data are retrieved from cgroupfs on access
 * and cached until refresh() is called.
 */
class NewCgroupContext {
 public:
  // Error type when retrieving CgroupContext fields
  enum class Error {
    NO_ERROR = 0,
    // Cgroup no longer exists
    INVALID_CGROUP,
  };

  /*
   * Create a cgroup context from its containing OomdContext and its path, which
   * must not be a glob pattern.
   *
   * OomdContext is required because some data are sibling-aware, i.e. values of
   * sibling cgroups affects its own value. It also stores ContextParams so
   * CgroupContext won't have to duplicate it.
   *
   * As OomdContext is guaranteed to last longer than CgroupContext, use
   * raw reference instead of smart pointer.
   */
  NewCgroupContext(OomdContext& ctx, const CgroupPath& cgroup);

  /*
   * Check if cgroup still exists and archive current data for temporal
   * counters. Only called by the containing OomdContext, which has access to
   * the mutable instance.
   */
  bool refresh();

  bool isValid() const {
    return cgroup_dir_.isValid();
  }

  const CgroupPath& cgroup() const {
    return cgroup_;
  }

  // Accessors to cgroup fields. If error is encountered, std::nullopt will be
  // returned and err set to corresponding error enum if it's not nullptr.
  // Otherwise, err will stay the same and an optional with value returned.

  // Names of child cgroups (not full path)
  const std::optional<std::vector<std::string>>& children(
      Error* err = nullptr) const;
  const std::optional<ResourcePressure>& mem_pressure(
      Error* err = nullptr) const;
  const std::optional<ResourcePressure>& io_pressure(
      Error* err = nullptr) const;
  const std::optional<std::unordered_map<std::string, int64_t>>& memory_stat(
      Error* err = nullptr) const;
  const std::optional<IOStat>& io_stat(Error* err = nullptr) const;
  std::optional<int64_t> current_usage(Error* err = nullptr) const;
  std::optional<int64_t> swap_usage(Error* err = nullptr) const;
  std::optional<int64_t> memory_low(Error* err = nullptr) const;
  std::optional<int64_t> memory_min(Error* err = nullptr) const;
  std::optional<int64_t> memory_high(Error* err = nullptr) const;
  std::optional<int64_t> memory_high_tmp(Error* err = nullptr) const;
  std::optional<int64_t> memory_max(Error* err = nullptr) const;
  std::optional<int64_t> nr_dying_descendants(Error* err = nullptr) const;
  // memory_{min,low} taking into account the distribution of it
  std::optional<int64_t> memory_protection(Error* err = nullptr) const;
  // Dot product between io stat and coeffs
  std::optional<double> io_cost_cumulative(Error* err = nullptr) const;

  // Below are temporal data counters, which need to be retrieved every interval
  // to become accurate. Must invoke them in plugin prerun() if they may be used
  // in run().

  // Moving average memory usage
  std::optional<int64_t> average_usage(Error* err = nullptr) const;
  // Change of cumulative io cost between intervals (not normalized by time)
  std::optional<double> io_cost_rate(Error* err = nullptr) const;

  // Non-cached derived counters
  std::optional<int64_t> anon_usage(Error* err = nullptr) const;
  std::optional<int64_t> effective_usage(
      Error* err = nullptr,
      int64_t memory_scale = 1,
      int64_t memory_adj = 0) const;

 private:
  // Test only
  friend class TestHelper;

  std::vector<std::string> getChildren() const;
  ResourcePressure getMemPressure() const;
  ResourcePressure getIoPressure() const;
  int64_t getMemcurrent() const;
  std::optional<int64_t> getMemoryProtection(Error* err) const;
  std::optional<double> getIoCostCumulative(Error* err) const;
  std::optional<int64_t> getAverageUsage(Error* err) const;
  std::optional<double> getIoCostRate(Error* err) const;

  struct CgroupData {
    std::optional<std::vector<std::string>> children;
    std::optional<ResourcePressure> mem_pressure;
    std::optional<ResourcePressure> io_pressure;
    std::optional<std::unordered_map<std::string, int64_t>> memory_stat;
    std::optional<IOStat> io_stat;
    std::optional<int64_t> current_usage;
    std::optional<int64_t> swap_usage;
    std::optional<int64_t> memory_low;
    std::optional<int64_t> memory_min;
    std::optional<int64_t> memory_high;
    std::optional<int64_t> memory_high_tmp;
    std::optional<int64_t> memory_max;
    std::optional<int64_t> nr_dying_descendants;
    // Cached derived data
    std::optional<int64_t> memory_protection;
    std::optional<double> io_cost_cumulative;
    // Temporal counters
    std::optional<int64_t> average_usage;
    std::optional<double> io_cost_rate;
  };

  // Data required to calculate temporal counters
  struct CgroupArchivedData {
    std::optional<int64_t> average_usage;
    std::optional<double> io_cost_cumulative;
  };

  OomdContext& ctx_;
  CgroupPath cgroup_;
  // This dir fd will be invalid whenever the cgroup is gone. Store it to
  // prevent race when a cgroup with exact same name is recreated after removal.
  // We check validity in refresh(). If invalid, the dir fd will be closed and
  // OomdContext will remove this CgroupContext.
  Fs::DirFd cgroup_dir_;
  std::unique_ptr<CgroupData> data_;

  CgroupArchivedData archive_{};
};

} // namespace Oomd
