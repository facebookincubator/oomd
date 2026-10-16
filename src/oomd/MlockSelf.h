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

#include <cstddef>
#include <thread>

namespace Oomd {

// Locks the read-only file mappings of the binary and its shared libraries
// into memory and logs the result. Best effort. Returns the bytes locked.
size_t mlockSelf();

// Runs mlockSelf() on a new thread. Returns an empty thread if one can't be
// started.
std::thread mlockSelfAsync();

} // namespace Oomd
