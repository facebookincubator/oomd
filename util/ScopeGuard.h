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

#include <functional>

// Need two levels of indirection here for __LINE__ to correctly expand
#define OOMD_CONCAT2(a, b) a##b
#define OOMD_CONCAT(a, b) OOMD_CONCAT2(a, b)
#define OOMD_ANON_VAR(str) OOMD_CONCAT(str, __LINE__)

namespace Oomd {

enum class ScopeGuardExit {};

class ScopeGuard {
 public:
  explicit ScopeGuard(std::function<void()> fn) {
    fn_ = fn;
  };

  ~ScopeGuard() {
    if (fn_) {
      fn_();
    }
  };

 private:
  std::function<void()> fn_;
};

inline ScopeGuard operator+(ScopeGuardExit, std::function<void()> fn) {
  return ScopeGuard(fn);
}

} // namespace Oomd

#define OOMD_SCOPE_EXIT \
  auto OOMD_ANON_VAR(SCOPE_EXIT_STATE) = ScopeGuardExit() + [&]()
