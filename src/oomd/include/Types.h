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
#include <iostream>
#include <optional>
#include <unordered_map>
#include <vector>

namespace Oomd {

namespace Engine {
using PluginArgs = std::unordered_map<std::string, std::string>;
}

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

  bool operator==(const DeviceIOStat& rhs) const {
    return dev_id == rhs.dev_id && rbytes == rhs.rbytes &&
        wbytes == rhs.wbytes && rios == rhs.rios && wios == rhs.wios &&
        dbytes == rhs.dbytes && dios == rhs.dios;
  }
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

struct __attribute__((__packed__)) ResourcePressure {
  float sec_10{0};
  float sec_60{0};
  float sec_300{0};
  std::optional<std::chrono::microseconds> total{std::nullopt};

  bool operator==(const ResourcePressure& rhs) const {
    return sec_10 == rhs.sec_10 && sec_60 == rhs.sec_60 &&
        sec_300 == rhs.sec_300 && total == rhs.total;
  }
};

struct __attribute__((__packed__)) SystemContext {
  uint64_t swaptotal{0};
  uint64_t swapused{0};
  int swappiness{0};
  std::unordered_map<std::string, int64_t> vmstat{};
  // moving avg swap out rate derived from vmstat[pswpout]
  double swapout_bps{0};
  double swapout_bps_60{0};
  double swapout_bps_300{0};
};

enum struct KillPreference {
  PREFER = 1,
  NORMAL = 0,
  AVOID = -1,
};
inline std::ostream& operator<<(std::ostream& os, KillPreference kp) {
  switch (kp) {
    case KillPreference::PREFER:
      os << "PREFER";
      break;
    case KillPreference::NORMAL:
      os << "NORMAL";
      break;
    case KillPreference::AVOID:
      os << "AVOID";
      break;
  }
  return os;
}

} // namespace Oomd
