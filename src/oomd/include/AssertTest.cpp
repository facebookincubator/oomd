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

#include <exception>

#include "oomd/include/Assert.h"

TEST(OomdAssertTest, Death) {
  // Shouldn't die
  OCHECK(true);
  OCHECK(1 == 1);

  int x = 1;
  int y = x;
  OCHECK(x == y);

  ASSERT_DEATH(OCHECK(false), "Assertion.*failed");
}

TEST(OomdAssertTest, ExprDeath) {
  ASSERT_DEATH(OCHECK(true == false), "Assertion.*failed");
}

TEST(OomdAssertExceptionTest, Throws) {
  ASSERT_NO_THROW(OCHECK_EXCEPT(true, std::runtime_error("x")));
  ASSERT_THROW(
      OCHECK_EXCEPT(false, std::runtime_error("x")), std::runtime_error);
  ASSERT_THROW(OCHECK_EXCEPT(false, std::exception()), std::exception);
}
