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

[[noreturn]] void
__OCHECK_FAIL(const char* expr, const char* file, int line, const char* func);

#define OCHECK(expr)       \
  (static_cast<bool>(expr) \
       ? void(0)           \
       : __OCHECK_FAIL(#expr, __FILE__, __LINE__, __PRETTY_FUNCTION__))
