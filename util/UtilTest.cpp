/*
 * Copyright (C) 2019-present, Facebook, Inc.
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

#include "oomd/util/Util.h"

using namespace Oomd;
using namespace testing;

TEST(UtilTest, ParseSizeTest) {
  int64_t v;

  EXPECT_EQ(Util::parseSize("8192", &v), 0);
  EXPECT_EQ(v, 8192);

  EXPECT_EQ(Util::parseSize("8K", &v), 0);
  EXPECT_EQ(v, 8192);

  EXPECT_EQ(Util::parseSize("1.5M 32K 512", &v), 0);
  EXPECT_EQ(v, (1 << 20) * 3 / 2 + (1 << 10) * 32 + 512);

  EXPECT_EQ(Util::parseSize("1.5MK", &v), -1);
  EXPECT_EQ(Util::parseSize("??", &v), -1);
  EXPECT_EQ(Util::parseSize("+-123M", &v), -1);
}

TEST(UtilTest, Split) {
  auto toks = Util::split("one by two", ' ');
  ASSERT_EQ(toks.size(), 3);
  EXPECT_THAT(toks, Contains("one"));
  EXPECT_THAT(toks, Contains("by"));
  EXPECT_THAT(toks, Contains("two"));

  toks = Util::split(" by two", ' ');
  ASSERT_EQ(toks.size(), 2);
  EXPECT_THAT(toks, Contains("by"));
  EXPECT_THAT(toks, Contains("two"));

  toks = Util::split("     by        two", ' ');
  ASSERT_EQ(toks.size(), 2);
  EXPECT_THAT(toks, Contains("by"));
  EXPECT_THAT(toks, Contains("two"));

  toks = Util::split("one two three", ',');
  ASSERT_EQ(toks.size(), 1);
  EXPECT_EQ(toks[0], "one two three");
}

TEST(StripTest, StripTest) {
  std::string s = "   123   \t";
  std::string s1 = "\t\t   123   ";
  std::string s2 = "123";
  std::string s3 = "";

  Util::strip(s);
  EXPECT_EQ(s, "123");
  Util::strip(s1);
  EXPECT_EQ(s1, "123");
  Util::strip(s2);
  EXPECT_EQ(s2, "123");
  Util::strip(s3);
  EXPECT_EQ(s3, "");
}

TEST(BlankTest, BlankTest) {
  EXPECT_TRUE(Util::isBlank(""));
  EXPECT_TRUE(Util::isBlank("        "));
  EXPECT_FALSE(Util::isBlank("             d"));
  EXPECT_TRUE(Util::isBlank("             \t\n"));
}
