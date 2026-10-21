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

#include <cerrno>

namespace Oomd {

// How one memory.reclaim write ended.
enum class ReclaimOutcome {
  Ok,
  // The kernel couldn't reclaim the full amount (EAGAIN)
  Again,
  // The cgroup went away (ENOENT or ENODEV)
  Gone,
  // Any other error
  Error,
  // Cancelled by whoever asked for it, before or during the write
  Cancelled,
};

// Maps a write's errno (0 on success) to its outcome. EINTR and ECANCELED
// only mean Cancelled when the write was cancelled.
inline ReclaimOutcome reclaimOutcomeOf(int err, bool cancelled) {
  if (err == 0) {
    return ReclaimOutcome::Ok;
  }
  if (cancelled && (err == EINTR || err == ECANCELED)) {
    return ReclaimOutcome::Cancelled;
  }
  if (err == EAGAIN) {
    return ReclaimOutcome::Again;
  }
  if (err == ENOENT || err == ENODEV) {
    return ReclaimOutcome::Gone;
  }
  return ReclaimOutcome::Error;
}

} // namespace Oomd
