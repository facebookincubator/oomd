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

#include <cstdint>

namespace Oomd {

enum struct OomType {
  NONE,
  SWAP,
  PRESSURE_10,
  PRESSURE_60,
  KILL_LIST,
};

union OomStat {
  int64_t swap_free; // in MB
  int32_t pressure_10_duration; // in seconds
};

struct OomContext {
  OomType type{OomType::NONE};
  OomStat stat;
};

struct ResourcePressure {
  float sec_10{0};
  float sec_60{0};
  float sec_600{0};
};

struct CgroupContext {
  ResourcePressure pressure;
  int64_t current_usage{0};
  int64_t average_usage{0};
  int64_t memory_low{0};
  int64_t swap_usage{0};
};

} // namespace Oomd
