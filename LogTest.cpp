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

#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <fstream>
#include "oomd/Log.h"
#include "oomd/shared/OomdContext.h"

using namespace Oomd;
using namespace testing;

class LogTest : public ::testing::Test {
 public:
  std::string test_string{"Testing basic logger output!"};
  std::string test_prefix{"oomd"};
  std::string test_complex_string{
      "oomd kill: 10.00 60.00 600.00 Evil  1999 detector:swap,12345MB killer:Evaporate "};
};

/*
  Test the plain log(buf) interface.
*/
TEST_F(LogTest, VerifyOutputSimple) {
  std::string outfile = "/tmp/logtest1.out";
  Log& logger = Log::get(outfile.c_str(), true);

  logger.log(test_string, test_prefix);
  /* ensure that output file is closed */
  logger.~Log();
  /* give file a sane protection mask */
  chmod(outfile.c_str(), 0644);

  /* check output */
  std::string compare_string;
  std::ifstream result_file(outfile);
  getline(result_file, compare_string);

  /* verify log contents */
  EXPECT_EQ(compare_string, test_prefix.append(": " + test_string));

  /* cleanup */
  result_file.close();
  if (remove(outfile.c_str()) != 0) {
    perror("remove");
  }
}

/*
  Test the memory status custom interface.
*/

TEST_F(LogTest, VerifyOutputComplex) {
  std::string outfile = "/tmp/logtest2.out";
  Log& logger = Log::get(outfile.c_str(), true);
  CgroupContext mcontext;
  OomContext ocontext;

  mcontext.pressure.sec_10 = 10.0;
  mcontext.pressure.sec_60 = 60.0;
  mcontext.pressure.sec_600 = 600.0;
  mcontext.current_usage = 1999;
  ocontext.type = OomType::SWAP;
  ocontext.stat.swap_free = 12345;

  logger.log("Evil ", "Evaporate \n", mcontext, ocontext, false);
  /* ensure that output file is closed */
  logger.~Log();
  /* give file a sane protection mask */
  chmod(outfile.c_str(), 0644);

  /* check output */
  std::string compare_string;
  std::ifstream result_file(outfile);
  getline(result_file, compare_string);
  /* verify log contents */
  EXPECT_EQ(compare_string, test_complex_string);

  /* cleanup */
  result_file.close();
  if (remove(outfile.c_str()) != 0) {
    perror("remove");
  }
}
