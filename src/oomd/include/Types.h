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
#include <cstdint>
#include <vector>

namespace Oomd {

enum struct ResourceType {
  MEMORY,
  IO,
};

enum struct DeviceType {
  HDD,
  SSD,
};

struct DeviceIOStat {
  std::string dev_id{""};
  int64_t rbytes{0};
  int64_t wbytes{0};
  int64_t rios{0};
  int64_t wios{0};
  int64_t dbytes{0};
  int64_t dios{0};
};

using IOStat = std::vector<DeviceIOStat>;

struct IOCostCoeffs {
  double read_iops{0};
  double readbw{0};
  double write_iops{0};
  double writebw{0};
  double trim_iops{0};
  double trimbw{0};
};

struct ResourcePressure {
  float sec_10{0};
  float sec_60{0};
  float sec_600{0};
  std::optional<std::chrono::microseconds> total{std::nullopt};
};

// If you update this class with something that could be valuable to know
// when debugging, please remember to update OomdContext::dumpOomdContext
// as well.
class CgroupContext {
 public:
  ResourcePressure pressure;
  ResourcePressure io_pressure;
  int64_t current_usage{0};
  int64_t average_usage{0};
  int64_t memory_low{0};
  int64_t memory_protection{0};
  int64_t swap_usage{0};
  int64_t anon_usage{0};
  int64_t memory_min{0};
  int64_t memory_high{0};
  int64_t memory_high_tmp{0};
  int64_t memory_max{0};
  float memory_scale{1};
  int64_t memory_adj{0};
  double io_cost_cumulative{0}; // Dot product between io stat and coeffs
  double io_cost_rate{0}; // change of cumulative divided by interval in seconds
  int64_t nr_dying_descendants{0};

  int64_t effective_usage() const {
    return current_usage * memory_scale - memory_protection + memory_adj;
  }
};

struct SystemContext {
  uint64_t swaptotal{0};
  uint64_t swapused{0};
};

} // namespace Oomd
