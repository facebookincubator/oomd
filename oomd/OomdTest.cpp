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

#include <gtest/gtest.h>

#include <string>
#include <unordered_set>

#include "oomd/Oomd.h"
#include "oomd/util/Fs.h"

using namespace Oomd;
using namespace testing;

constexpr auto kCgroupDataDir = "oomd/fixtures/cgroup";

class OomdTest : public ::testing::Test {
 public:
  OomdTest() {
    oomd = std::make_unique<::Oomd::Oomd>(
        nullptr,
        nullptr,
        5,
        kCgroupDataDir,
        "",
        io_devs,
        hdd_coeffs,
        ssd_coeffs);
  }

  std::string cgroup_path{kCgroupDataDir};
  OomdContext ctx;
  std::unique_ptr<::Oomd::Oomd> oomd{nullptr};
  CgroupPath service1{cgroup_path, "system.slice/service1.service"};
  CgroupPath service2{cgroup_path, "system.slice/service2.service"};
  CgroupPath service3{cgroup_path, "system.slice/service3.service"};
  CgroupPath service4{cgroup_path, "system.slice/service4.service"};
  CgroupPath slice1{cgroup_path, "system.slice/slice1.slice"};
  CgroupPath workload_service1{cgroup_path, "workload.slice/service1.service"};
  std::unordered_map<std::string, DeviceType> io_devs = {
      {"1:11", DeviceType::SSD}};
  IOCostCoeffs hdd_coeffs = {
      .read_iops = 6,
      .readbw = 5,
      .write_iops = 4,
      .writebw = 3,
      .trim_iops = 2,
      .trimbw = 1,
  };
  IOCostCoeffs ssd_coeffs = {
      .read_iops = 1,
      .readbw = 2,
      .write_iops = 3,
      .writebw = 4,
      .trim_iops = 5,
      .trimbw = 6,
  };
};

TEST_F(OomdTest, OomdContextUpdate) {
  EXPECT_EQ(ctx.cgroups().size(), 0);

  std::unordered_set<CgroupPath> cgroups;
  cgroups.emplace(CgroupPath(cgroup_path, "system.slice/*"));
  oomd->updateContext(cgroups, ctx);

  EXPECT_EQ(ctx.cgroups().size(), 5);

  EXPECT_TRUE(ctx.hasCgroupContext(service1));
  EXPECT_TRUE(ctx.hasCgroupContext(service2));
  EXPECT_TRUE(ctx.hasCgroupContext(service3));
  EXPECT_TRUE(ctx.hasCgroupContext(service4));
  EXPECT_TRUE(ctx.hasCgroupContext(slice1));
}

TEST_F(OomdTest, OomdContextMultipleUpdates) {
  std::unordered_set<CgroupPath> cgroups;
  cgroups.emplace(CgroupPath(cgroup_path, "system.slice/*"));
  oomd->updateContext(cgroups, ctx);

  for (int i = 0; i < 3; i++) {
    int64_t average = ctx.getCgroupContext(service1).average_usage;
    oomd->updateContext(cgroups, ctx);

    // We expect the avg usage to slowly converge from 0 -> true avg
    // b/c of AVERAGE_SIZE_DECAY
    EXPECT_GT(ctx.getCgroupContext(service1).average_usage, average);
  }
}

TEST_F(OomdTest, OomdContextUpdateMultiCgroup) {
  EXPECT_EQ(ctx.cgroups().size(), 0);

  std::unordered_set<CgroupPath> cgroups;
  cgroups.emplace(CgroupPath(cgroup_path, "system.slice/*"));
  cgroups.emplace(CgroupPath(cgroup_path, "workload.slice/*"));
  oomd->updateContext(cgroups, ctx);

  EXPECT_EQ(ctx.cgroups().size(), 6);

  EXPECT_TRUE(ctx.hasCgroupContext(service1));
  EXPECT_TRUE(ctx.hasCgroupContext(service2));
  EXPECT_TRUE(ctx.hasCgroupContext(service3));
  EXPECT_TRUE(ctx.hasCgroupContext(service4));
  EXPECT_TRUE(ctx.hasCgroupContext(slice1));
  EXPECT_TRUE(ctx.hasCgroupContext(workload_service1));
}

TEST_F(OomdTest, OomdContextUpdateMultiCgroupWildcard) {
  EXPECT_EQ(ctx.cgroups().size(), 0);

  std::unordered_set<CgroupPath> cgroups;
  cgroups.emplace(CgroupPath(cgroup_path, "*.slice/*"));
  cgroups.emplace(CgroupPath(cgroup_path, "workload.slice/*"));
  oomd->updateContext(cgroups, ctx);

  EXPECT_EQ(ctx.cgroups().size(), 6);

  EXPECT_TRUE(ctx.hasCgroupContext(service1));
  EXPECT_TRUE(ctx.hasCgroupContext(service2));
  EXPECT_TRUE(ctx.hasCgroupContext(service3));
  EXPECT_TRUE(ctx.hasCgroupContext(service4));
  EXPECT_TRUE(ctx.hasCgroupContext(slice1));
  EXPECT_TRUE(ctx.hasCgroupContext(workload_service1));
}

TEST_F(OomdTest, CalculateProtectionOverage) {
  std::unordered_set<CgroupPath> cgroups;
  cgroups.emplace(CgroupPath(cgroup_path, "system.slice/*"));
  oomd->updateContext(cgroups, ctx);

  auto s1_ctx = ctx.getCgroupContext(service1);
  auto s2_ctx = ctx.getCgroupContext(service2);
  auto s3_ctx = ctx.getCgroupContext(service3);
  auto s4_ctx = ctx.getCgroupContext(service4);
  auto sl1_ctx = ctx.getCgroupContext(slice1);

  EXPECT_LT(s1_ctx.effective_usage(), s2_ctx.effective_usage());
  EXPECT_LT(s1_ctx.effective_usage(), s3_ctx.effective_usage());
  EXPECT_LT(s1_ctx.effective_usage(), s4_ctx.effective_usage());
  EXPECT_LT(s1_ctx.effective_usage(), sl1_ctx.effective_usage());
  EXPECT_EQ(s2_ctx.effective_usage(), s3_ctx.effective_usage());
  EXPECT_EQ(s2_ctx.effective_usage(), s4_ctx.effective_usage());
  EXPECT_EQ(s2_ctx.effective_usage(), sl1_ctx.effective_usage());
}

TEST_F(OomdTest, MonitorRootHost) {
  std::string cgroup2fs_mntpt = Fs::getCgroup2MountPoint();
  if (cgroup2fs_mntpt.empty()) {
#ifdef MESON_BUILD
    // GTEST_SKIP() is in gtest 1.9.x (note: starting at 1.9.x,
    // gtest is living at master) and it's unlikely that it will
    // ever get packaged.
    return;
#else
    GTEST_SKIP() << "Host not running cgroup2";
#endif
  }

  std::unordered_set<CgroupPath> cgroups;
  CgroupPath root(cgroup2fs_mntpt, "/");
  cgroups.emplace(root);
  oomd->updateContext(cgroups, ctx);

  int64_t current = ctx.getCgroupContext(root).current_usage;
  // If we're running the test, I should hope the root host is using memory
  // otherwise we've really stumbled onto a competitive advantage.
  EXPECT_GT(current, 0);
}

TEST_F(OomdTest, CalculateProtectionOverageContrived) {
  const std::string contrived_cgroup_path =
      cgroup_path + "/protection_overage.fakeroot";

  std::unordered_set<CgroupPath> cgroups;
  cgroups.emplace(CgroupPath(contrived_cgroup_path, "*/*"));
  // We're manually adding in ancestors here b/c Oomd::Oomd usually does
  // this for us and we're not using the real constructor code path
  cgroups.emplace(CgroupPath(contrived_cgroup_path, "*"));
  oomd->updateContext(cgroups, ctx);

  ctx.dump();

  int64_t A = ctx.getCgroupContext(CgroupPath(contrived_cgroup_path, "A"))
                  .effective_usage();
  int64_t A1 = ctx.getCgroupContext(CgroupPath(contrived_cgroup_path, "A/A1"))
                   .effective_usage();
  int64_t A2 = ctx.getCgroupContext(CgroupPath(contrived_cgroup_path, "A/A2"))
                   .effective_usage();
  int64_t B = ctx.getCgroupContext(CgroupPath(contrived_cgroup_path, "B"))
                  .effective_usage();
  int64_t B1 = ctx.getCgroupContext(CgroupPath(contrived_cgroup_path, "B/B1"))
                   .effective_usage();
  int64_t B2 = ctx.getCgroupContext(CgroupPath(contrived_cgroup_path, "B/B2"))
                   .effective_usage();

  EXPECT_EQ(A, 2l << 30);
  EXPECT_EQ(B, 3l << 30);

  // Hierarchy is B1 > B2 >= A1 > A2
  EXPECT_GT(A1, A2);
  EXPECT_EQ(B2, A1);
  EXPECT_GT(B1, B2);
}

TEST_F(OomdTest, CalculateIOCostCumulative) {
  /*
  system.slice/io.stat content:
  1:10 rbytes=1111111 wbytes=2222222 rios=33 wios=44 dbytes=5555555555 dios=6
  1:11 rbytes=2222222 wbytes=3333333 rios=44 wios=55 dbytes=6666666666 dios=7
  */
  std::unordered_set<CgroupPath> cgroups;
  cgroups.emplace(CgroupPath(cgroup_path, "system.slice"));
  oomd->updateContext(cgroups, ctx);

  auto expected_io_cost_cum = 2222222 /*rbytes*/ * ssd_coeffs.readbw +
      3333333 /*wbytes*/ * ssd_coeffs.writebw +
      44 /*rios*/ * ssd_coeffs.read_iops + 55 /*wios*/ * ssd_coeffs.write_iops +
      6666666666 /*dbytes*/ * ssd_coeffs.trimbw +
      7 /*dios*/ * ssd_coeffs.trim_iops;

  auto root_ctx = ctx.getCgroupContext(CgroupPath(cgroup_path, "system.slice"));
  EXPECT_EQ(root_ctx.io_cost_cumulative, expected_io_cost_cum);
  EXPECT_EQ(root_ctx.io_cost_rate, 0); // initial rate should be zero

  oomd->updateContext(cgroups, ctx);
  // if file is not change, nothing should change
  EXPECT_EQ(root_ctx.io_cost_cumulative, expected_io_cost_cum);
  EXPECT_EQ(root_ctx.io_cost_rate, 0);
}

TEST_F(OomdTest, MissingControlFiles) {
  std::unordered_set<CgroupPath> cgroups;
  cgroups.emplace(CgroupPath(cgroup_path, "missing_control_files.slice"));

  ASSERT_NO_THROW(oomd->updateContext(cgroups, ctx));
  EXPECT_EQ(ctx.cgroups().size(), 0);
}
