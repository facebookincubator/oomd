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
#include <utility>
#include <vector>

#include "oomd/fixtures/FsFixture.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

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
  auto de = Fs::readDir(dir, Fs::DE_DIR);

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
  auto de = Fs::readDir(dir, Fs::DE_FILE);

  ASSERT_EQ(de.files.size(), 4);
  EXPECT_THAT(de.files, Contains(std::string("file1")));
  EXPECT_THAT(de.files, Contains(std::string("file2")));
  EXPECT_THAT(de.files, Contains(std::string("file3")));
  EXPECT_THAT(de.files, Contains(std::string("file4")));
  EXPECT_THAT(de.files, Not(Contains(std::string("file5"))));
}

TEST_F(FsTest, ResolveWildcardedPathRelative) {
  auto dir = fixture_.fsDataDir();
  dir += "/wildcard";

  std::string wildcarded_path_some = "/this/path/is*/going/to/be/long/file";
  auto resolved = Fs::resolveWildcardPath(dir + wildcarded_path_some);
  ASSERT_EQ(resolved.size(), 2);
  EXPECT_THAT(resolved, Contains(dir + "/this/path/is/going/to/be/long/file"));
  EXPECT_THAT(
      resolved, Contains(dir + "/this/path/isNOT/going/to/be/long/file"));

  std::string wildcarded_path_all = "/this/path/*/going/to/be/long/file";
  resolved = Fs::resolveWildcardPath(dir + wildcarded_path_all);
  ASSERT_EQ(resolved.size(), 3);
  EXPECT_THAT(resolved, Contains(dir + "/this/path/is/going/to/be/long/file"));
  EXPECT_THAT(
      resolved, Contains(dir + "/this/path/isNOT/going/to/be/long/file"));
  EXPECT_THAT(resolved, Contains(dir + "/this/path/WAH/going/to/be/long/file"));

  resolved = Fs::resolveWildcardPath(dir + "/not/a/valid/dir");
  ASSERT_EQ(resolved.size(), 0);
}

TEST_F(FsTest, ResolveWildcardedPathAbsolute) {
  auto resolved = Fs::resolveWildcardPath("/proc/vm*");
  ASSERT_EQ(resolved.size(), 2);
  EXPECT_THAT(resolved, Contains("/proc/vmstat"));
  EXPECT_THAT(resolved, Contains("/proc/vmallocinfo"));
}

TEST_F(FsTest, ResolveCgroupWildcardPath) {
  auto root_fs = fixture_.fsDataDir();
  root_fs += "/wildcard";

  auto resolved = Fs::resolveCgroupWildcardPath(
      CgroupPath{root_fs, "/this/path/is*/going/to/be/long/file"});
  ASSERT_EQ(resolved.size(), 2);

  EXPECT_THAT(
      resolved,
      Contains(CgroupPath(root_fs, "/this/path/is/going/to/be/long/file")));
  EXPECT_THAT(
      resolved,
      Contains(CgroupPath(root_fs, "/this/path/isNOT/going/to/be/long/file")));

  // Verify root_fs is correctly separated from relative path
  auto it_one =
      resolved.find(CgroupPath(root_fs, "/this/path/is/going/to/be/long/file"));
  ASSERT_NE(it_one, resolved.end());
  EXPECT_EQ(it_one->cgroupFs(), root_fs);
  auto it_two = resolved.find(
      CgroupPath(root_fs, "/this/path/isNOT/going/to/be/long/file"));
  ASSERT_NE(it_two, resolved.end());
  EXPECT_EQ(it_two->cgroupFs(), root_fs);
}

TEST_F(FsTest, ReadFile) {
  auto file = fixture_.fsDataDir() + "/dir1/stuff";
  auto lines = Fs::readFileByLine(file);

  ASSERT_EQ(lines.size(), 4);
  EXPECT_EQ(lines[0], "hello world");
  EXPECT_EQ(lines[1], "my good man");
  EXPECT_EQ(lines[2], "");
  EXPECT_EQ(lines[3], "1");
}

TEST_F(FsTest, ReadFileBad) {
  auto file = fixture_.fsDataDir() + "/ksldjfksdlfdsjf";
  auto lines = Fs::readFileByLine(file);
  ASSERT_EQ(lines.size(), 0);
}

TEST_F(FsTest, GetPids) {
  auto dir = fixture_.cgroupDataDir();
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
  auto dir = fixture_.cgroupDataDir();
  EXPECT_EQ(Fs::readMemcurrent(dir), 987654321);
}

TEST_F(FsTest, ReadMemoryLow) {
  auto dir = fixture_.cgroupDataDir();
  EXPECT_EQ(Fs::readMemlow(dir), 333333);
}

TEST_F(FsTest, ReadMemoryMin) {
  auto dir = fixture_.cgroupDataDir();
  EXPECT_EQ(Fs::readMemmin(dir), 666);
}

TEST_F(FsTest, ReadMemoryHigh) {
  auto dir = fixture_.cgroupDataDir();
  EXPECT_EQ(Fs::readMemhigh(dir), 1000);
}

TEST_F(FsTest, ReadMemoryMax) {
  auto dir = fixture_.cgroupDataDir();
  EXPECT_EQ(Fs::readMemmax(dir), 654);
}

TEST_F(FsTest, ReadMemoryHighTmp) {
  auto dir = fixture_.cgroupDataDir();
  EXPECT_EQ(Fs::readMemhightmp(dir), 2000);
}

TEST_F(FsTest, ReadSwapCurrent) {
  auto dir = fixture_.cgroupDataDir();
  EXPECT_EQ(Fs::readSwapCurrent(dir), 321321);
}

TEST_F(FsTest, ReadControllers) {
  auto dir = fixture_.cgroupDataDir();
  auto controllers = Fs::readControllers(dir);

  ASSERT_EQ(controllers.size(), 4);
  EXPECT_THAT(controllers, Contains(std::string("cpu")));
  EXPECT_THAT(controllers, Contains(std::string("io")));
  EXPECT_THAT(controllers, Contains(std::string("memory")));
  EXPECT_THAT(controllers, Contains(std::string("pids")));
  EXPECT_THAT(controllers, Not(Contains(std::string("block"))));
}

TEST_F(FsTest, ReadMemoryPressure) {
  // v4.16+ upstream format
  auto dir = fixture_.cgroupDataDir();
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

  // old experimental format w/ debug info on
  auto dir3 = dir + "/service3.service";
  auto pressure3 = Fs::readMempressure(dir3);

  EXPECT_FLOAT_EQ(pressure3.sec_10, 4.44);
  EXPECT_FLOAT_EQ(pressure3.sec_60, 5.55);
  EXPECT_FLOAT_EQ(pressure3.sec_600, 6.66);
}

TEST_F(FsTest, ReadMemoryPressureSome) {
  // v4.16+ upstream format
  auto dir = fixture_.cgroupDataDir();
  auto pressure = Fs::readMempressure(dir, Fs::PressureType::SOME);

  EXPECT_FLOAT_EQ(pressure.sec_10, 1.11);
  EXPECT_FLOAT_EQ(pressure.sec_60, 2.22);
  EXPECT_FLOAT_EQ(pressure.sec_600, 3.33);

  // old experimental format
  auto dir2 = dir + "/service2.service";
  auto pressure2 = Fs::readMempressure(dir2, Fs::PressureType::SOME);

  EXPECT_FLOAT_EQ(pressure2.sec_10, 1.11);
  EXPECT_FLOAT_EQ(pressure2.sec_60, 2.22);
  EXPECT_FLOAT_EQ(pressure2.sec_600, 3.33);
}

TEST_F(FsTest, GetVmstat) {
  auto vmstatfile = fixture_.fsVmstatFile();
  auto vmstat = Fs::getVmstat(vmstatfile);

  EXPECT_EQ(vmstat["first_key"], 12345);
  EXPECT_EQ(vmstat["second_key"], 678910);
  EXPECT_EQ(vmstat["thirdkey"], 999999);

  // we expect the key is missing (ie default val = 0)
  EXPECT_EQ(vmstat["asdf"], 0);
}

TEST_F(FsTest, GetMeminfo) {
  auto meminfofile = fixture_.fsMeminfoFile();
  auto meminfo = Fs::getMeminfo(meminfofile);

  EXPECT_EQ(meminfo.size(), 49);
  EXPECT_EQ(meminfo["SwapTotal"], 2097148 * 1024);
  EXPECT_EQ(meminfo["SwapFree"], 1097041 * 1024);
  EXPECT_EQ(meminfo["HugePages_Total"], 0);

  // we expect the key is missing (ie default val = 0)
  EXPECT_EQ(meminfo["asdf"], 0);
}

TEST_F(FsTest, GetMemstat) {
  auto dir = fixture_.cgroupDataDir();
  auto meminfo = Fs::getMemstat(dir);

  EXPECT_EQ(meminfo.size(), 29);
  EXPECT_EQ(meminfo["anon"], 1294168064);
  EXPECT_EQ(meminfo["file"], 3870687232);
  EXPECT_EQ(meminfo["pglazyfree"], 0);

  // we expect the key is missing (ie default val = 0)
  EXPECT_EQ(meminfo["asdf"], 0);
}

TEST_F(FsTest, ReadIoPressure) {
  auto dir = fixture_.cgroupDataDir();
  auto pressure = Fs::readIopressure(dir);

  EXPECT_FLOAT_EQ(pressure.sec_10, 4.45);
  EXPECT_FLOAT_EQ(pressure.sec_60, 5.56);
  EXPECT_FLOAT_EQ(pressure.sec_600, 6.67);
}

TEST_F(FsTest, ReadIoPressureSome) {
  auto dir = fixture_.cgroupDataDir();
  auto pressure = Fs::readIopressure(dir, Fs::PressureType::SOME);

  EXPECT_FLOAT_EQ(pressure.sec_10, 1.12);
  EXPECT_FLOAT_EQ(pressure.sec_60, 2.23);
  EXPECT_FLOAT_EQ(pressure.sec_600, 3.34);
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
  auto cgrouppath = Fs::getCgroup2MountPoint(mountsfile);

  EXPECT_EQ(cgrouppath, std::string("/sys/fs/cgroup/"));
}

TEST_F(FsTest, GetDeviceType) {
  auto fsDevDir = fixture_.fsDeviceDir();

  try {
    auto ssd_type = Fs::getDeviceType("1:0", fsDevDir);
    EXPECT_EQ(ssd_type, DeviceType::SSD);
    auto hdd_type = Fs::getDeviceType("1:1", fsDevDir);
    EXPECT_EQ(hdd_type, DeviceType::HDD);
  } catch (const std::exception& e) {
    FAIL() << "Expect no exception but got: " << e.what();
  }
  try {
    Fs::getDeviceType("1:2", fsDevDir);
    FAIL() << "Expected Fs::bad_control_file";
  } catch (Fs::bad_control_file& e) {
    EXPECT_EQ(
        e.what(),
        fsDevDir + "/1:2/" + Fs::kDeviceTypeDir + "/" + Fs::kDeviceTypeFile +
            ": invalid format");
  } catch (const std::exception& e) {
    FAIL() << "Expected Fs::bad_control_file but got: " << e.what();
  }
}

TEST_F(FsTest, ReadIostat) {
  auto dir = fixture_.cgroupDataDir();

  auto io_stat = Fs::readIostat(dir);
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
