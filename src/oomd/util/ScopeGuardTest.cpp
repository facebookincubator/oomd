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

#include "oomd/util/ScopeGuard.h"

#include <exception>

using namespace Oomd;
using namespace testing;

TEST(ScopeGuardTest, InnerScope) {
  int x = 5;

  {
    OOMD_SCOPE_EXIT {
      x++;
    };
  }

  EXPECT_EQ(x, 6);
}

TEST(ScopeGuardTest, FunctionReturn) {
  int x = 5;

  [&]() {
    OOMD_SCOPE_EXIT {
      x++;
    };

    x++;
    return;
  }();

  EXPECT_EQ(x, 7);
}

TEST(ScopeGuardTest, ExceptionContext) {
  int x = 5;

  try {
    [&]() {
      OOMD_SCOPE_EXIT {
        x++;
      };

      throw std::runtime_error("exception");
    }();
  } catch (const std::exception&) {
  }

  EXPECT_EQ(x, 6);
}

TEST(ScopeGuardTest, MultipleGuards) {
  int x = 5;

  {
    OOMD_SCOPE_EXIT {
      x++;
    };

    OOMD_SCOPE_EXIT {
      x++;
    };
  }

  EXPECT_EQ(x, 7);
}
