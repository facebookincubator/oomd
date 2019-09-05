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

#include <array>

namespace Oomd {

class CoreStats {
 public:
  static constexpr auto kKillsKey = "oomd.kills_structured";
  static constexpr auto kNumDropInAdds = "oomd.dropin.added";

  // List of all the stats keys. Useful for operations that need to know
  // all the available core keys.
  static constexpr std::array<const char*, 2> kAllKeys = {
      kKillsKey,
      kNumDropInAdds,
  };
};

} // namespace Oomd
