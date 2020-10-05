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

#include <utility>
#include <vector>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "oomd/fixtures/FsFixture.h"
#include "oomd/util/Fs.h"
#include "oomd/util/TestHelper.h"

using namespace Oomd;
using namespace testing;

class FsTest : public ::testing::Test {
 protected:
  void SetUp() override {
    try {
      fixture_.materialize();
    } catch (const std::exception& e) {
      FAIL() << "SetUpTestSuite: " << e.what();
    }
  }

  void TearDown() override {
    try {
      fixture_.teardown();
    } catch (const std::exception& e) {
      FAIL() << "TearDownTestSuite: " << e.what();
    }
  }

  FsFixture fixture_{};
};

TEST_F(FsTest, FindDirectories) {
  auto dir = fixture_.fsDataDir();
  auto de = ASSERT_SYS_OK(Fs::readDir(dir, Fs::DE_DIR));

  ASSERT_EQ(de.dirs.size(), 4);
  EXPECT_THAT(de.dirs, Contains(std::string("dir1")));
  EXPECT_THAT(de.dirs, Contains(std::string("dir2")));
  EXPECT_THAT(de.dirs, Contains(std::string("dir3")));
  EXPECT_THAT(de.dirs, Contains(std::string("wildcard")));
  EXPECT_THAT(de.dirs, Not(Contains(std::string("dir21"))));
  EXPECT_THAT(de.dirs, Not(Contains(std::string("dir22"))));
}

TEST_F(FsTest, IsDir) {
  auto dir = fixture_.fsDataDir();
  EXPECT_TRUE(Fs::isDir(dir + "/dir1"));
  EXPECT_FALSE(Fs::isDir(dir + "/dir1/stuff"));
  EXPECT_FALSE(Fs::isDir(dir + "/NOTINFS"));
}

TEST_F(FsTest, RemovePrefix) {
  std::string s = "long string like this";
  Fs::removePrefix(s, "long string ");
  EXPECT_EQ(s, "like this");

  std::string ss = "random string";
  Fs::removePrefix(ss, "asdf");
  EXPECT_EQ(ss, "random string");

  std::string sss = "asdf";
  Fs::removePrefix(sss, "asdf");
  EXPECT_EQ(sss, "");

  std::string path = "./var/log/messages";
  Fs::removePrefix(path, "var/log/");
  EXPECT_EQ(path, "messages");

  std::string path2 = "./var/log/messages";
  Fs::removePrefix(path2, "./var/log/");
  EXPECT_EQ(path2, "messages");
}

TEST_F(FsTest, FindFiles) {
  auto dir = fixture_.fsDataDir();
  auto de = ASSERT_SYS_OK(Fs::readDir(dir, Fs::DE_FILE));

  ASSERT_EQ(de.files.size(), 4);
  EXPECT_THAT(de.files, Contains(std::string("file1")));
  EXPECT_THAT(de.files, Contains(std::string("file2")));
  EXPECT_THAT(de.files, Contains(std::string("file3")));
  EXPECT_THAT(de.files, Contains(std::string("file4")));
  EXPECT_THAT(de.files, Not(Contains(std::string("file5"))));
}

TEST_F(FsTest, Glob) {
  auto dir = fixture_.fsDataDir();
  dir += "/wildcard";

  auto wildcarded_path_some = dir + "/dir*";
  auto resolved = ASSERT_SYS_OK(Fs::glob(wildcarded_path_some));
  ASSERT_EQ(resolved.size(), 2);
  EXPECT_THAT(resolved, Contains(dir + "/dir1"));
  EXPECT_THAT(resolved, Contains(dir + "/dir2"));

  auto wildcarded_path_dir_only = dir + "/*";
  resolved =
      ASSERT_SYS_OK(Fs::glob(wildcarded_path_dir_only, /* dir_only */ true));
  ASSERT_EQ(resolved.size(), 3);
  EXPECT_THAT(resolved, Contains(dir + "/dir1"));
  EXPECT_THAT(resolved, Contains(dir + "/dir2"));
  EXPECT_THAT(resolved, Contains(dir + "/different_dir"));

  auto wildcarded_path_all = dir + "/*";
  resolved = ASSERT_SYS_OK(Fs::glob(wildcarded_path_all));
  ASSERT_EQ(resolved.size(), 4);
  EXPECT_THAT(resolved, Contains(dir + "/dir1"));
  EXPECT_THAT(resolved, Contains(dir + "/dir2"));
  EXPECT_THAT(resolved, Contains(dir + "/different_dir"));
  EXPECT_THAT(resolved, Contains(dir + "/file"));

  auto nonexistent_path = dir + "/not/a/valid/dir";
  resolved = ASSERT_SYS_OK(Fs::glob(nonexistent_path));
  ASSERT_EQ(resolved.size(), 0);
}

TEST_F(FsTest, ReadFile) {
  auto file = fixture_.fsDataDir() + "/dir1/stuff";
  auto lines = ASSERT_SYS_OK(Fs::readFileByLine(file));

  ASSERT_EQ(lines.size(), 4);
  EXPECT_EQ(lines[0], "hello world");
  EXPECT_EQ(lines[1], "my good man");
  EXPECT_EQ(lines[2], "");
  EXPECT_EQ(lines[3], "1");

  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(fixture_.fsDataDir() + "/dir1"));

  ASSERT_EQ(
      ASSERT_SYS_OK(Fs::readFileByLine(Fs::Fd::openat(dir, "stuff"))), lines);
}

TEST_F(FsTest, ReadFileBad) {
  auto file = fixture_.fsDataDir() + "/ksldjfksdlfdsjf";
  EXPECT_FALSE(Fs::readFileByLine(file));

  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(fixture_.fsDataDir()));
  EXPECT_FALSE(Fs::readFileByLine(Fs::Fd::openat(dir, "ksldjfksdlfdsjf")));
}

TEST_F(FsTest, GetPids) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto pids = ASSERT_SYS_OK(Fs::getPidsAt(dir));
  EXPECT_EQ(pids.size(), 1);
  EXPECT_THAT(pids, Contains(123));

  auto path2 = path + "/service1.service";
  auto dir2 = ASSERT_SYS_OK(Fs::DirFd::open(path2));
  auto pids2 = ASSERT_SYS_OK(Fs::getPidsAt(dir2));
  EXPECT_EQ(pids2.size(), 2);
  EXPECT_THAT(pids2, Contains(456));
  EXPECT_THAT(pids2, Contains(789));
}

TEST_F(FsTest, ReadIsPopulated) {
  auto dir1 = ASSERT_SYS_OK(Fs::DirFd::open(fixture_.cgroupDataDir()));
  EXPECT_EQ(ASSERT_SYS_OK(Fs::readIsPopulatedAt(dir1)), true);

  auto dir2 = ASSERT_SYS_OK(
      Fs::DirFd::open(fixture_.cgroupDataDir() + "/service3.service"));
  EXPECT_EQ(ASSERT_SYS_OK(Fs::readIsPopulatedAt(dir2)), false);
}

TEST_F(FsTest, GetNrDying) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto nr_dying = ASSERT_SYS_OK(Fs::getNrDyingDescendantsAt(dir));
  EXPECT_EQ(nr_dying, 27);
}

TEST_F(FsTest, ReadMemoryCurrent) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto memcurrent = ASSERT_SYS_OK(Fs::readMemcurrentAt(dir));
  EXPECT_EQ(memcurrent, 987654321);
}

TEST_F(FsTest, ReadMemoryLow) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto memlow = ASSERT_SYS_OK(Fs::readMemlowAt(dir));
  EXPECT_EQ(memlow, 333333);
}

TEST_F(FsTest, ReadMemoryMin) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto memmin = ASSERT_SYS_OK(Fs::readMemminAt(dir));
  EXPECT_EQ(memmin, 666);
}

TEST_F(FsTest, ReadMemoryHigh) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto memhigh = ASSERT_SYS_OK(Fs::readMemhighAt(dir));
  EXPECT_EQ(memhigh, 1000);
}

TEST_F(FsTest, ReadMemoryMax) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto memmax = ASSERT_SYS_OK(Fs::readMemmaxAt(dir));
  EXPECT_EQ(memmax, 654);
}

TEST_F(FsTest, ReadMemoryHighTmp) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto memtmphigh = ASSERT_SYS_OK(Fs::readMemhightmpAt(dir));
  EXPECT_EQ(memtmphigh, 2000);
}

TEST_F(FsTest, ReadSwapCurrent) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto swap_current = ASSERT_SYS_OK(Fs::readSwapCurrentAt(dir));
  EXPECT_EQ(swap_current, 321321);
}

TEST_F(FsTest, ReadControllers) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto controllers = ASSERT_SYS_OK(Fs::readControllersAt(dir));

  ASSERT_EQ(controllers.size(), 4);
  EXPECT_THAT(controllers, Contains(std::string("cpu")));
  EXPECT_THAT(controllers, Contains(std::string("io")));
  EXPECT_THAT(controllers, Contains(std::string("memory")));
  EXPECT_THAT(controllers, Contains(std::string("pids")));
  EXPECT_THAT(controllers, Not(Contains(std::string("block"))));
}

TEST_F(FsTest, ReadMemoryPressure) {
  // v4.16+ upstream format
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto pressure = ASSERT_SYS_OK(Fs::readMempressureAt(dir));

  EXPECT_FLOAT_EQ(pressure.sec_10, 4.44);
  EXPECT_FLOAT_EQ(pressure.sec_60, 5.55);
  EXPECT_FLOAT_EQ(pressure.sec_300, 6.66);

  // old experimental format
  auto path2 = path + "/service2.service";
  auto dir2 = ASSERT_SYS_OK(Fs::DirFd::open(path2));
  auto pressure2 = ASSERT_SYS_OK(Fs::readMempressureAt(dir2));

  EXPECT_FLOAT_EQ(pressure2.sec_10, 4.44);
  EXPECT_FLOAT_EQ(pressure2.sec_60, 5.55);
  EXPECT_FLOAT_EQ(pressure2.sec_300, 6.66);

  // old experimental format w/ debug info on
  auto path3 = path + "/service3.service";
  auto dir3 = ASSERT_SYS_OK(Fs::DirFd::open(path3));
  auto pressure3 = ASSERT_SYS_OK(Fs::readMempressureAt(dir3));

  EXPECT_FLOAT_EQ(pressure3.sec_10, 4.44);
  EXPECT_FLOAT_EQ(pressure3.sec_60, 5.55);
  EXPECT_FLOAT_EQ(pressure3.sec_300, 6.66);
}

TEST_F(FsTest, ReadMemoryPressureSome) {
  // v4.16+ upstream format
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto pressure =
      ASSERT_SYS_OK(Fs::readMempressureAt(dir, Fs::PressureType::SOME));

  EXPECT_FLOAT_EQ(pressure.sec_10, 1.11);
  EXPECT_FLOAT_EQ(pressure.sec_60, 2.22);
  EXPECT_FLOAT_EQ(pressure.sec_300, 3.33);

  // old experimental format
  auto path2 = path + "/service2.service";
  auto dir2 = ASSERT_SYS_OK(Fs::DirFd::open(path2));
  auto pressure2 =
      ASSERT_SYS_OK(Fs::readMempressureAt(dir2, Fs::PressureType::SOME));

  EXPECT_FLOAT_EQ(pressure2.sec_10, 1.11);
  EXPECT_FLOAT_EQ(pressure2.sec_60, 2.22);
  EXPECT_FLOAT_EQ(pressure2.sec_300, 3.33);
}

TEST_F(FsTest, GetVmstat) {
  auto vmstatfile = fixture_.fsVmstatFile();
  auto vmstat = ASSERT_SYS_OK(Fs::getVmstat(vmstatfile));

  EXPECT_EQ(vmstat["first_key"], 12345);
  EXPECT_EQ(vmstat["second_key"], 678910);
  EXPECT_EQ(vmstat["thirdkey"], 999999);

  // we expect the key is missing (ie default val = 0)
  EXPECT_EQ(vmstat["asdf"], 0);
}

TEST_F(FsTest, GetMeminfo) {
  auto meminfofile = fixture_.fsMeminfoFile();
  auto meminfo = ASSERT_SYS_OK(Fs::getMeminfo(meminfofile));

  EXPECT_EQ(meminfo.size(), 49);
  EXPECT_EQ(meminfo["SwapTotal"], 2097148 * 1024);
  EXPECT_EQ(meminfo["SwapFree"], 1097041 * 1024);
  EXPECT_EQ(meminfo["HugePages_Total"], 0);

  // we expect the key is missing (ie default val = 0)
  EXPECT_EQ(meminfo["asdf"], 0);
}

TEST_F(FsTest, GetMemstat) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto meminfo = ASSERT_SYS_OK(Fs::getMemstatAt(dir));

  EXPECT_EQ(meminfo.size(), 29);
  EXPECT_EQ(meminfo["anon"], 1294168064);
  EXPECT_EQ(meminfo["file"], 3870687232);
  EXPECT_EQ(meminfo["pglazyfree"], 0);

  // we expect the key is missing (ie default val = 0)
  EXPECT_EQ(meminfo["asdf"], 0);
}

TEST_F(FsTest, ReadIoPressure) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto pressure = ASSERT_SYS_OK(Fs::readIopressureAt(dir));

  EXPECT_FLOAT_EQ(pressure.sec_10, 4.45);
  EXPECT_FLOAT_EQ(pressure.sec_60, 5.56);
  EXPECT_FLOAT_EQ(pressure.sec_300, 6.67);
}

TEST_F(FsTest, ReadIoPressureSome) {
  auto path = fixture_.cgroupDataDir();
  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto pressure =
      ASSERT_SYS_OK(Fs::readIopressureAt(dir, Fs::PressureType::SOME));

  EXPECT_FLOAT_EQ(pressure.sec_10, 1.12);
  EXPECT_FLOAT_EQ(pressure.sec_60, 2.23);
  EXPECT_FLOAT_EQ(pressure.sec_300, 3.34);
}

TEST_F(FsTest, ReadMemoryOomGroup) {
  auto path1 = fixture_.cgroupDataDir() + "/slice1.slice";
  auto dir1 = ASSERT_SYS_OK(Fs::DirFd::open(path1));
  auto oom_group = ASSERT_SYS_OK(Fs::readMemoryOomGroupAt(dir1));
  EXPECT_EQ(oom_group, true);

  auto path2 = fixture_.cgroupDataDir() + "/slice1.slice/service1.service";
  auto dir2 = ASSERT_SYS_OK(Fs::DirFd::open(path2));
  auto oom_group2 = ASSERT_SYS_OK(Fs::readMemoryOomGroupAt(dir2));
  EXPECT_EQ(oom_group2, false);
}

TEST_F(FsTest, IsUnderParentPath) {
  EXPECT_TRUE(Fs::isUnderParentPath("/sys/fs/cgroup/", "/sys/fs/cgroup/"));
  EXPECT_TRUE(Fs::isUnderParentPath("/sys/fs/cgroup/", "/sys/fs/cgroup/blkio"));
  EXPECT_FALSE(Fs::isUnderParentPath("/sys/fs/cgroup/", "/sys/fs/"));
  EXPECT_TRUE(Fs::isUnderParentPath("/", "/sys/"));
  EXPECT_FALSE(Fs::isUnderParentPath("/sys/", "/"));
  EXPECT_FALSE(Fs::isUnderParentPath("", "/sys/"));
  EXPECT_FALSE(Fs::isUnderParentPath("/sys/", ""));
  EXPECT_FALSE(Fs::isUnderParentPath("", ""));
}

TEST_F(FsTest, GetCgroup2MountPoint) {
  auto mountsfile = fixture_.fsMountsFile();
  auto cgrouppath = ASSERT_SYS_OK(Fs::getCgroup2MountPoint(mountsfile));

  EXPECT_EQ(cgrouppath, std::string("/sys/fs/cgroup/"));
}

TEST_F(FsTest, GetDeviceType) {
  auto fsDevDir = fixture_.fsDeviceDir();

  auto ssd_type = ASSERT_SYS_OK(Fs::getDeviceType("1:0", fsDevDir));
  EXPECT_EQ(ssd_type, DeviceType::SSD);
  auto hdd_type = ASSERT_SYS_OK(Fs::getDeviceType("1:1", fsDevDir));
  EXPECT_EQ(hdd_type, DeviceType::HDD);
  auto ret = Fs::getDeviceType("1:2", fsDevDir);
  ASSERT_FALSE(ret);
}

TEST_F(FsTest, ReadIostat) {
  auto path = fixture_.cgroupDataDir();

  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  auto io_stat = ASSERT_SYS_OK(Fs::readIostatAt(dir));
  EXPECT_EQ(io_stat.size(), 2);

  auto stat0 = io_stat[0];
  EXPECT_EQ(stat0.dev_id, "1:10");
  EXPECT_EQ(stat0.rbytes, 1111111);
  EXPECT_EQ(stat0.wbytes, 2222222);
  EXPECT_EQ(stat0.rios, 33);
  EXPECT_EQ(stat0.wios, 44);
  EXPECT_EQ(stat0.dbytes, 5555555555);
  EXPECT_EQ(stat0.dios, 6);

  auto stat1 = io_stat[1];
  EXPECT_EQ(stat1.dev_id, "1:11");
  EXPECT_EQ(stat1.rbytes, 2222222);
  EXPECT_EQ(stat1.wbytes, 3333333);
  EXPECT_EQ(stat1.rios, 44);
  EXPECT_EQ(stat1.wios, 55);
  EXPECT_EQ(stat1.dbytes, 6666666666);
  EXPECT_EQ(stat1.dios, 7);
}

TEST_F(FsTest, WriteMemoryHigh) {
  using F = Fixture;
  auto path = fixture_.cgroupDataDir() + "/write_test";
  F::materialize(F::makeDir(path, {F::makeFile("memory.high")}));

  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  ASSERT_SYS_OK(Fs::writeMemhighAt(dir, 54321));
  EXPECT_EQ(ASSERT_SYS_OK(Fs::readMemhighAt(dir)), 54321);
}

TEST_F(FsTest, WriteMemoryHighTmp) {
  using F = Fixture;
  auto path = fixture_.cgroupDataDir() + "/write_test";
  F::materialize(F::makeDir(path, {F::makeFile("memory.high.tmp")}));

  auto dir = ASSERT_SYS_OK(Fs::DirFd::open(path));
  ASSERT_SYS_OK(
      Fs::writeMemhightmpAt(dir, 54321, std::chrono::microseconds{400000}));
  EXPECT_EQ(ASSERT_SYS_OK(Fs::readMemhightmpAt(dir)), 54321);
}
