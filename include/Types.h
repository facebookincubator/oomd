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

namespace Oomd {

enum struct ResourceType {
  MEMORY,
  IO,
};

struct ResourcePressure {
  float sec_10{0};
  float sec_60{0};
  float sec_600{0};
  std::chrono::microseconds total{0};
};

struct CgroupContext {
  ResourcePressure pressure;
  ResourcePressure io_pressure;
  int64_t current_usage{0};
  int64_t average_usage{0};
  int64_t memory_low{0};
  int64_t swap_usage{0};
  int64_t anon_usage{0};
  int64_t memory_min{0};
  // Amount of memory over actual received protection
  int64_t protection_overage{0};
};

} // namespace Oomd
