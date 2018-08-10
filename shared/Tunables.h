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

#include <string>
#include <vector>

#include "oomd/Log.h"
#include "oomd/util/Fs.h"

namespace Oomd {

constexpr auto kTunablesOverrideFile = "/etc/oomd_tunables.override";

struct Tunables {
  enum Tunable {
    FIRST = 0, // Always keep this first
    INTERVAL = FIRST,
    VERBOSE_INTERVAL,
    POST_KILL_DELAY,
    THRESHOLD,
    HIGH_THRESHOLD,
    HIGH_THRESHOLD_DURATION,
    LARGER_THAN,
    GROWTH_ABOVE,
    AVERAGE_SIZE_DECAY,
    FAST_FALL_RATIO,
    MIN_SWAP_PCT,
    FBTAX2_WORKLOAD_THRESHOLD,
    FBTAX2_MEM_THRESHOLD,
    FBTAX2_MEM_DURATION,
    FBTAX2_WORKLOAD_THRESHOLD2,
    FBTAX2_MEM_THRESHOLD2,
    FBTAX2_MEM_DURATION2,
    FBTAX2_WORKLOAD_IO_THRESHOLD,
    FBTAX2_IO_THRESHOLD,
    FBTAX2_IO_DURATION,
    FBTAX2_WORKLOAD_IO_THRESHOLD2,
    FBTAX2_IO_THRESHOLD2,
    FBTAX2_IO_DURATION2,
    LAST, // Always keep this last
  };

  struct TunableEntry {
    Tunable knob;
    std::string env_name;
    std::string default_value;
    std::string value;
  };

  Tunables();
  ~Tunables() = default;
  void dump();
  void parseEnvVars();
  void loadOverrides(const std::string& fname = kTunablesOverrideFile);

  /*
   * It is up to the caller to know what type they want.
   * This was done so future tunables can be added in a controlled
   * and modular fashion.
   */
  template <typename T>
  T get(Tunable knob) {
    for (auto& k : knobs) {
      if (k.knob == knob) {
        /*
         * Since T is a numeric type, we first cast the string to a double
         * before static casting to the desired type.
         */
        return static_cast<T>(std::stod(k.value));
      }
    }
    OLOG << "FATAL: Unknown knob=" << knob << "is being retrieved";
    std::abort();
  }

  std::vector<TunableEntry> knobs = {
      {INTERVAL, "OOMD_INTERVAL", "5", ""},
      {VERBOSE_INTERVAL, "OOMD_VERBOSE_INTERVAL", "300", ""},
      {POST_KILL_DELAY, "OOMD_POST_KILL_DELAY", "15", ""},
      {THRESHOLD, "OOMD_THRESHOLD", "60", ""},
      {HIGH_THRESHOLD, "OOMD_HIGH_THRESHOLD", "80", ""},
      {HIGH_THRESHOLD_DURATION, "OOMD_HIGH_THRESHOLD_DURATION", "10", ""},
      {LARGER_THAN, "OOMD_LARGER_THAN", "50", ""},
      {GROWTH_ABOVE, "OOMD_GROWTH_ABOVE", "80", ""},
      {AVERAGE_SIZE_DECAY, "OOMD_AVERAGE_SIZE_DECAY", "4", ""},
      {FAST_FALL_RATIO, "OOMD_FAST_FALL_RATIO", "0.85", ""},
      {MIN_SWAP_PCT, "OOMD_MIN_SWAP_PCT", "15", ""},
      {FBTAX2_WORKLOAD_THRESHOLD, "OOMD_FBTAX2_WORKLOAD_THRESHOLD", "0", ""},
      {FBTAX2_MEM_THRESHOLD, "OOMD_FBTAX2_MEM_THRESHOLD", "100", ""},
      {FBTAX2_MEM_DURATION, "OOMD_FBTAX2_MEM_DURATION", "0", ""},
      {FBTAX2_WORKLOAD_THRESHOLD2, "OOMD_FBTAX2_WORKLOAD_THRESHOLD2", "0", ""},
      {FBTAX2_MEM_THRESHOLD2, "OOMD_FBTAX2_MEM_THRESHOLD2", "100", ""},
      {FBTAX2_MEM_DURATION2, "OOMD_FBTAX2_MEM_DURATION2", "0", ""},
      {FBTAX2_WORKLOAD_IO_THRESHOLD, "OOMD_FBTAX2_WORKLOAD_IO_THRESHOLD", "0", ""},
      {FBTAX2_IO_THRESHOLD, "OOMD_FBTAX2_IO_THRESHOLD", "100", ""},
      {FBTAX2_IO_DURATION, "OOMD_FBTAX2_IO_DURATION", "0", ""},
      {FBTAX2_WORKLOAD_IO_THRESHOLD2, "OOMD_FBTAX2_WORKLOAD_IO_THRESHOLD2", "0", ""},
      {FBTAX2_IO_THRESHOLD2, "OOMD_FBTAX2_IO_THRESHOLD2", "100", ""},
      {FBTAX2_IO_DURATION2, "OOMD_FBTAX2_IO_DURATION2", "0", ""},
  };
};

} // namespace Oomd
