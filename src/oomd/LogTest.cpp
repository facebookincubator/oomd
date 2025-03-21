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

#include <stdio.h>
#include <fstream>
#include <sstream>
#include "oomd/Log.h"
#include "oomd/OomdContext.h"

using namespace Oomd;

class MockLog : public LogBase {
 public:
  ~MockLog() override = default;

  void kmsgLog(const std::string& /* unused */, const std::string& /* unused */)
      const override {
    return;
  }

  void debugLog(std::string&& buf) override {
    lines.emplace_back(std::move(buf));
  }

  std::vector<std::string> lines;
};

class LogTestKmsg : public ::testing::Test {
 public:
  std::string test_string{"Testing basic logger output!"};
  std::string test_prefix{"oomd"};
  std::string test_complex_string{
      "oomd kill: 10.00 60.00 600.00 Evil  1999 detector:swap,12345MB killer:Evaporate "};

 protected:
  std::pair<std::unique_ptr<Log>, std::ifstream> get_logger_and_file() {
    std::string outfile = "/tmp/logtest.XXXXXX";
    int fd = ::mkstemp(&outfile[0]);

    auto logger = Log::get_for_unittest(fd, std::cerr, /* inl= */ false);
    std::ifstream result_file(outfile);

    if (::remove(outfile.c_str()) != 0) {
      perror("remove");
    }

    return std::make_pair(std::move(logger), std::move(result_file));
  }
};

/*
  Test the plain kmsgLog(buf) interface.
*/
TEST_F(LogTestKmsg, VerifyOutputSimple) {
  auto logger_and_file = get_logger_and_file();
  auto& logger = logger_and_file.first;
  auto& result_file = logger_and_file.second;

  logger->kmsgLog(test_string, test_prefix);

  /* check output */
  std::string compare_string;
  getline(result_file, compare_string);

  /* verify log contents */
  EXPECT_EQ(compare_string, test_prefix.append(": " + test_string));
}

TEST(LogTestAsyncDebug, CoupleLines) {
  std::stringstream sstr;
  {
    // Do not use inline logging for our log tests because
    //   1) we want to test async logging behavior
    //   2) we correctly destroy the Log object during the test which
    //      correctly joins the async thread
    auto logger = Log::get_for_unittest(STDERR_FILENO, sstr, /* inl= */ false);

    logger->debugLog(std::string("line one\n"));
    logger->debugLog(std::string("line2\n"));
  }

  EXPECT_EQ(sstr.str(), "line one\nline2\n");
}

TEST(LogTestAsyncDebug, LotsOfLines) {
  for (int i = 0; i < 10; ++i) {
    std::stringstream expected;
    std::stringstream sstr;
    {
      // See comment in CoupleLines
      auto logger =
          Log::get_for_unittest(STDERR_FILENO, sstr, /* inl= */ false);
      for (int j = 0; j < 150; ++j) {
        logger->debugLog(std::to_string(j) + "_line\n");
        expected << j << "_line\n";
      }
    }

    EXPECT_EQ(sstr.str(), expected.str());
  }
}

TEST(LogStream, Basic) {
  MockLog log;
  LogStream(log) << "hello world!";
  LogStream(log) << "hello world 2!";
  ASSERT_EQ(log.lines.size(), 2);
  EXPECT_EQ(log.lines[0], "hello world!\n");
  EXPECT_EQ(log.lines[1], "hello world 2!\n");
}

TEST(LogStream, ControlDisable) {
  MockLog log;
  LogStream(log) << "one";
  LogStream(log) << LogStream::Control::DISABLE;
  LogStream(log) << "two";
  LogStream(log) << "three";
  LogStream(log) << LogStream::Control::ENABLE;
  LogStream(log) << "four";
  LogStream(log) << "five";

  ASSERT_EQ(log.lines.size(), 3);
  EXPECT_EQ(log.lines[0], "one\n");
  EXPECT_EQ(log.lines[1], "four\n");
  EXPECT_EQ(log.lines[2], "five\n");
}

TEST(LogStream, ControlDisableEnableAdvanced) {
  MockLog log;
  LogStream(log) << "one";
  LogStream(log) << LogStream::Control::DISABLE << "after disable";
  LogStream(log) << "two";
  LogStream(log) << "three";
  LogStream(log) << LogStream::Control::ENABLE << "after enable";

  ASSERT_EQ(log.lines.size(), 2);
  EXPECT_EQ(log.lines[0], "one\n");
  EXPECT_EQ(log.lines[1], "after enable\n");

  log.lines.clear();

  LogStream(log) << LogStream::Control::DISABLE << "after disable"
                 << LogStream::Control::ENABLE << "after enable";
  ASSERT_EQ(log.lines.size(), 1);
  EXPECT_EQ(log.lines[0], "after enable\n");
}

TEST(LogStream, ControlMany) {
  MockLog log;
  LogStream(log) << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << LogStream::Control::DISABLE << LogStream::Control::ENABLE
                 << "hello world";

  ASSERT_EQ(log.lines.size(), 1);
  EXPECT_EQ(log.lines[0], "hello world\n");
}

TEST(LogStream, logTime) {
  time_t t = 0;
  struct tm tm;
  ::gmtime_r(&t, &tm);
  auto ts = OOMD_TIME_STR(&tm);
  EXPECT_EQ(ts, "0101 00:00:00");
}
