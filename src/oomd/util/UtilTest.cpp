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

#include <linux/memfd.h>
#include <sys/syscall.h>
#include <unistd.h>

#include <cstring>

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

TEST(UtilTest, ParseSizeOrPercentTest) {
  int64_t v;

  EXPECT_EQ(Util::parseSizeOrPercent("1%", &v, 100), 0);
  EXPECT_EQ(v, 1);

  EXPECT_EQ(Util::parseSizeOrPercent("5M", &v, 100), 0);
  EXPECT_EQ(v, 5 << 20);

  EXPECT_EQ(Util::parseSizeOrPercent("5", &v, 100), 0);
  EXPECT_EQ(v, 5 << 20);

  EXPECT_EQ(Util::parseSizeOrPercent("5K", &v, 100), 0);
  EXPECT_EQ(v, 5 << 10);

  EXPECT_NE(Util::parseSizeOrPercent("5%z", &v, 100), 0);
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

  toks = Util::split("", ',');
  ASSERT_EQ(toks.size(), 0);

  toks = Util::split("     ", ' ');
  ASSERT_EQ(toks.size(), 0);

  toks = Util::split("one two three   ", ' ');
  ASSERT_EQ(toks.size(), 3);
  EXPECT_THAT(toks, Contains("one"));
  EXPECT_THAT(toks, Contains("two"));
  EXPECT_THAT(toks, Contains("three"));
}

TEST(UtilTest, StartsWith) {
  EXPECT_TRUE(Util::startsWith("prefix", "prefixThis!"));
  EXPECT_TRUE(Util::startsWith("x", "xx"));
  EXPECT_TRUE(Util::startsWith("", "xx"));
  EXPECT_TRUE(Util::startsWith("", ""));

  EXPECT_FALSE(Util::startsWith("prefix", "prefiyThat!"));
  EXPECT_FALSE(Util::startsWith("xx", "x"));
  EXPECT_FALSE(Util::startsWith("x", ""));
}

TEST(UtilTest, Trim) {
  std::string s = "  sdf  ";
  Util::trim(s);
  EXPECT_EQ(s, "sdf");

  std::string s1 = "  as df  ";
  Util::trim(s1);
  EXPECT_EQ(s1, "as df");

  std::string s2 = "  asdf";
  Util::trim(s2);
  EXPECT_EQ(s2, "asdf");

  std::string s3 = "asdf ";
  Util::trim(s3);
  EXPECT_EQ(s3, "asdf");

  std::string s4 = "asdf";
  Util::trim(s4);
  EXPECT_EQ(s4, "asdf");

  std::string s5 = "";
  Util::trim(s5);
  EXPECT_EQ(s5, "");

  std::string s6 = " \t   \n";
  Util::trim(s6);
  EXPECT_EQ(s6, "");
}

TEST(UtilTest, ReadWriteFull) {
  int fd = ::syscall(SYS_memfd_create, "myfile", MFD_CLOEXEC);
  ASSERT_GE(fd, 0);

  // Write a bunch of data in
  std::string start(1234567, 'z');
  EXPECT_EQ(Util::writeFull(fd, start.data(), start.size()), start.size());

  // Seek back to beginning
  ::lseek(fd, 0, SEEK_SET);

  // Read data back out
  std::string end(12345678, 'x'); // Note the extra 8
  EXPECT_EQ(Util::readFull(fd, end.data(), start.size()), start.size());
  EXPECT_EQ(std::memcmp(start.data(), end.data(), start.size()), 0);
}
