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

#include "oomd/include/Types.h"
#include "oomd/shared/OomdContext.h"

namespace Oomd {

class Log {
 public:
  Log(const Log& other) = delete;
  Log& operator=(const Log& other) = delete;
  ~Log();
  static void init_or_die();
  static Log& get();
  static Log& get(const char* outfile, bool recreate);

  void set_kmsg(bool b) noexcept {
    kmsg_ = b;
  }
  void set_stderr(bool b) noexcept {
    stderr_ = b;
  }

  void log(const std::string& buf, const std::string& prefix) const;

  void log(
      const std::string& to_kill,
      const std::string& kill_type,
      const CgroupContext& context,
      const OomContext& oom_context,
      bool dry = false) const;

 private:
  explicit Log(
      const char* outfile = "/dev/kmsg"); // only get() is allowed to construct

  bool kmsg_{true};
  bool stderr_{true};

  int kmsg_fd_{-1};
};

template <typename... Args>
static void OOMD_LOG(Args&&... args) {
  Log::get().log(std::forward<Args>(args)...);
}

} // namespace Oomd
