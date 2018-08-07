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

#include "oomd/util/Fs.h"

using namespace Oomd;
using namespace testing;

constexpr auto kCgroupDataDir = "oomd/fixtures/cgroup";
constexpr auto kFsDataDir = "oomd/fixtures/fs_data";
constexpr auto kFsVmstatFile = "oomd/fixtures/proc/vmstat";
constexpr auto kFsMeminfoFile = "oomd/fixtures/proc/meminfo";

class FsTest : public ::testing::Test {
 public:
  bool existsInVec(const std::vector<std::string>& hay, std::string needle) {
    return std::any_of(hay.begin(), hay.end(), [&](const std::string& fname) {
      return fname == needle;
    });
  }
};

TEST_F(FsTest, FindDirectories) {
  std::string dir(kFsDataDir);
  auto dirs = Fs::readDir(dir, Fs::EntryType::DIRECTORY);

  ASSERT_EQ(dirs.size(), 3);
  EXPECT_TRUE(existsInVec(dirs, std::string("dir1")));
  EXPECT_TRUE(existsInVec(dirs, std::string("dir2")));
  EXPECT_TRUE(existsInVec(dirs, std::string("dir3")));
  EXPECT_FALSE(existsInVec(dirs, std::string("dir21")));
  EXPECT_FALSE(existsInVec(dirs, std::string("dir22")));
}

TEST_F(FsTest, FindFiles) {
  std::string dir(kFsDataDir);
  auto files = Fs::readDir(dir, Fs::EntryType::REG_FILE);

  ASSERT_EQ(files.size(), 4);
  EXPECT_TRUE(existsInVec(files, std::string("file1")));
  EXPECT_TRUE(existsInVec(files, std::string("file2")));
  EXPECT_TRUE(existsInVec(files, std::string("file3")));
  EXPECT_TRUE(existsInVec(files, std::string("file4")));
  EXPECT_FALSE(existsInVec(files, std::string("file5")));
}

TEST_F(FsTest, ReadFile) {
  auto file = std::string(kFsDataDir) + "/dir1/stuff";
  auto lines = Fs::readFileByLine(file);

  ASSERT_EQ(lines.size(), 4);
  EXPECT_EQ(lines[0], "hello world");
  EXPECT_EQ(lines[1], "my good man");
  EXPECT_EQ(lines[2], "");
  EXPECT_EQ(lines[3], "1");
}

TEST_F(FsTest, ReadFileBad) {
  auto file = std::string(kFsDataDir) + "/ksldjfksdlfdsjf";
  auto lines = Fs::readFileByLine(file);
  ASSERT_EQ(lines.size(), 0);
}

TEST_F(FsTest, GetPids) {
  std::string dir(kCgroupDataDir);
  auto pids = Fs::getPids(dir);
  EXPECT_EQ(pids.size(), 1);
  EXPECT_THAT(pids, Contains(123));

  auto dir2 = dir + "/service1.service";
  auto pids2 = Fs::getPids(dir2);
  EXPECT_EQ(pids2.size(), 2);
  EXPECT_THAT(pids2, Contains(456));
  EXPECT_THAT(pids2, Contains(789));

  auto pids_r = Fs::getPids(dir, true);
  EXPECT_EQ(pids_r.size(), 7);
  EXPECT_THAT(pids_r, Contains(123));
  EXPECT_THAT(pids_r, Contains(456));
  EXPECT_THAT(pids_r, Contains(789));
}

TEST_F(FsTest, ReadMemoryCurrent) {
  std::string dir(kCgroupDataDir);
  EXPECT_EQ(Fs::readMemcurrent(dir), 987654321);
}

TEST_F(FsTest, ReadMemoryLow) {
  std::string dir(kCgroupDataDir);
  EXPECT_EQ(Fs::readMemlow(dir), 333333);
}

TEST_F(FsTest, ReadSwapCurrent) {
  std::string dir(kCgroupDataDir);
  EXPECT_EQ(Fs::readSwapCurrent(dir), 321321);
}

TEST_F(FsTest, ReadControllers) {
  std::string dir(kCgroupDataDir);
  auto controllers = Fs::readControllers(dir);

  ASSERT_EQ(controllers.size(), 4);
  EXPECT_TRUE(existsInVec(controllers, std::string("cpu")));
  EXPECT_TRUE(existsInVec(controllers, std::string("io")));
  EXPECT_TRUE(existsInVec(controllers, std::string("memory")));
  EXPECT_TRUE(existsInVec(controllers, std::string("pids")));
  EXPECT_FALSE(existsInVec(controllers, std::string("block")));
}

TEST_F(FsTest, ReadMemoryPressure) {
  // v4.16+ upstream format
  std::string dir(kCgroupDataDir);
  auto pressure = Fs::readMempressure(dir);

  EXPECT_FLOAT_EQ(pressure.sec_10, 4.44);
  EXPECT_FLOAT_EQ(pressure.sec_60, 5.55);
  EXPECT_FLOAT_EQ(pressure.sec_600, 6.66);

  // old experimental format
  auto dir2 = dir + "/service2.service";
  auto pressure2 = Fs::readMempressure(dir2);

  EXPECT_FLOAT_EQ(pressure2.sec_10, 4.44);
  EXPECT_FLOAT_EQ(pressure2.sec_60, 5.55);
  EXPECT_FLOAT_EQ(pressure2.sec_600, 6.66);
}

TEST_F(FsTest, GetVmstat) {
  std::string vmstatfile(kFsVmstatFile);
  auto vmstat = Fs::getVmstat(vmstatfile);

  EXPECT_EQ(vmstat["first_key"], 12345);
  EXPECT_EQ(vmstat["second_key"], 678910);
  EXPECT_EQ(vmstat["thirdkey"], 999999);

  // we expect the key is missing (ie default val = 0)
  EXPECT_EQ(vmstat["asdf"], 0);
}

TEST_F(FsTest, GetMeminfo) {
  std::string meminfofile(kFsMeminfoFile);
  auto meminfo = Fs::getMeminfo(meminfofile);

  EXPECT_EQ(meminfo.size(), 49);
  EXPECT_EQ(meminfo["SwapTotal"], 2097148 * 1024);
  EXPECT_EQ(meminfo["SwapFree"], 1097041 * 1024);
  EXPECT_EQ(meminfo["HugePages_Total"], 0);

  // we expect the key is missing (ie default val = 0)
  EXPECT_EQ(meminfo["asdf"], 0);
}
