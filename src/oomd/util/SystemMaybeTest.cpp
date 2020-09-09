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

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "oomd/util/SystemMaybe.h"

using namespace Oomd;

namespace {
SystemMaybe<int> someFailingFunction() {
  return systemError(EBUSY, "something ", "is busy");
}

SystemMaybe<int> someFailingFunctionMacro() {
  return SYSTEM_ERROR(EBUSY);
}

SystemMaybe<int> someSuccessfulFunction() {
  return 42;
}
} // namespace

TEST(SystemMaybeTest, ErrorTest) {
  auto maybe = someFailingFunction();
  ASSERT_FALSE(maybe);
  ASSERT_EQ(maybe.error().code().value(), EBUSY);
  ASSERT_NE(
      std::string(maybe.error().what()).find("something is busy"),
      std::string::npos);
}

TEST(SystemMaybeTest, ValueTest) {
  auto maybe = someSuccessfulFunction();
  ASSERT_TRUE(maybe);
  ASSERT_EQ(maybe.value(), 42);
  ASSERT_EQ(*maybe, 42);
}

TEST(SystemMaybeTest, noSystemErrorTest) {
  auto v = noSystemError();
  ASSERT_TRUE(v);
}

TEST(SystemMaybeTest, MacroTest) {
  auto maybe = someFailingFunctionMacro();
  ASSERT_FALSE(maybe);
  ASSERT_EQ(maybe.error().code().value(), EBUSY);
}
