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

#include "oomd/CgroupContext.h"
#include "oomd/Log.h"
#include "oomd/OomdContext.h"
#include "oomd/util/Fixture.h"
#include "oomd/util/TestHelper.h"

using namespace Oomd;
using namespace testing;
using F = Fixture;

class CgroupContextTest : public Test {
 public:
  CgroupContextTest() {
    params_.io_devs = {{"1:10", DeviceType::HDD}, {"1:11", DeviceType::SSD}};
    params_.hdd_coeffs = {.read_iops = 6,
                          .readbw = 5,
                          .write_iops = 4,
                          .writebw = 3,
                          .trim_iops = 2,
                          .trimbw = 1};
    params_.ssd_coeffs = {.read_iops = 1,
                          .readbw = 2,
                          .write_iops = 3,
                          .writebw = 4,
                          .trim_iops = 5,
                          .trimbw = 6};
    ctx_ = OomdContext(params_);
  }

 protected:
  void SetUp() override {
    tempDir_ = Fixture::mkdtempChecked();
    ctx_ = OomdContext(params_);
  }

  void TearDown() override {
    F::rmrChecked(tempDir_);
  }

  OomdContext ctx_;
  ContextParams params_;
  std::string tempDir_;
};

TEST_F(CgroupContextTest, MonitorRootHost) {
  std::string cgroup2fs_mntpt = ASSERT_SYS_OK(Fs::getCgroup2MountPoint());
  if (cgroup2fs_mntpt.empty()) {
#ifdef GTEST_SKIP
    GTEST_SKIP() << "Host not running cgroup2";
#else
    // GTEST_SKIP() is in gtest 1.9.x (note: starting at 1.9.x,
    // gtest is living at master) and it's unlikely that it will
    // ever get packaged.
    OLOG << "Host not running cgroup2";
    return;
#endif
  }

  if (cgroup2fs_mntpt == "/sys/fs/cgroup/unified/") {
    // If we have the cgroup tree in "hybrid" mode, this will fail when we test
    // to get the cgroup context, since we will not find any memory stat files
    // in that tree. Let's also skip this test in that case.
    // TODO: A better approach would be to find which controllers are mounted in
    // the cgroup2 mountpoint, but for now this should be enough to get us to
    // pass our tests on Travis-CI.
#ifdef GTEST_SKIP
    GTEST_SKIP() << "This test does not support hybrid hierarchy";
#else
    OLOG << "This test does not support hybrid hierarchy";
    return;
#endif
  }

  OomdContext ctx;
  auto cgroup_ctx = ctx.addToCacheAndGet(CgroupPath(cgroup2fs_mntpt, "/"));
  ASSERT_TRUE(cgroup_ctx);
  // If we're running the test, I should hope the root host is using memory
  // otherwise we've really stumbled onto a competitive advantage.
  EXPECT_GT(cgroup_ctx->get().current_usage(), 0);
}

TEST_F(CgroupContextTest, UniqueId) {
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir("A"), F::makeDir("B"), F::makeDir("C"), F::makeDir("D")}));
  std::unordered_set<CgroupContext::Id> ids;

  for (const CgroupContext& cgroup_ctx : ctx_.addToCacheAndGet(
           std::unordered_set<CgroupPath>{CgroupPath(tempDir_, "*")})) {
    if (auto id = cgroup_ctx.id()) {
      ids.emplace(*id);
    }
  }
  EXPECT_EQ(ids.size(), 4);
}

TEST_F(CgroupContextTest, MemoryProtection) {
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
          "A",
          {F::makeFile("memory.current", "10000\n"),
           F::makeFile("memory.low", "2000\n"),
           F::makeFile("memory.min", "2500\n"),
           F::makeDir(
               "B",
               {F::makeFile("memory.current", "1000\n"),
                F::makeFile("memory.low", "250\n"),
                F::makeFile("memory.min", "200\n"),
                F::makeDir(
                    "C",
                    {F::makeFile("memory.current", "0\n"),
                     F::makeFile("memory.low", "250\n"),
                     F::makeFile("memory.min", "200\n")}),
                F::makeDir(
                    "D",
                    {F::makeFile("memory.current", "1000\n"),
                     F::makeFile("memory.low", "0\n"),
                     F::makeFile("memory.min", "0\n")})}),
           F::makeDir(
               "E",
               {F::makeFile("memory.current", "250\n"),
                F::makeFile("memory.low", "1000\n"),
                F::makeFile("memory.min", "200\n"),
                F::makeDir(
                    "F",
                    {F::makeFile("memory.current", "2000\n"),
                     F::makeFile("memory.low", "2000\n"),
                     F::makeFile("memory.min", "2000\n")}),
                F::makeDir(
                    "G",
                    {F::makeFile("memory.current", "3000\n"),
                     F::makeFile("memory.low", "3000\n"),
                     F::makeFile("memory.min", "3000\n")})})})}));

  // Top level slice gets whatever protection it claims
  auto cgroup_ctx = ctx_.addToCacheAndGet(CgroupPath(tempDir_, "A"));
  ASSERT_TRUE(cgroup_ctx);
  EXPECT_EQ(cgroup_ctx->get().memory_protection(), 2500);

  // If sum of sibling protection is less than parent protection, all get
  // whatever they claim
  cgroup_ctx = ctx_.addToCacheAndGet(CgroupPath(tempDir_, "A/B"));
  ASSERT_TRUE(cgroup_ctx);
  EXPECT_EQ(cgroup_ctx->get().memory_protection(), 250);
  cgroup_ctx = ctx_.addToCacheAndGet(CgroupPath(tempDir_, "A/E"));
  ASSERT_TRUE(cgroup_ctx);
  EXPECT_EQ(cgroup_ctx->get().memory_protection(), 250);

  // If sum of sibling protection is zero, all get zero
  cgroup_ctx = ctx_.addToCacheAndGet(CgroupPath(tempDir_, "A/B/C"));
  ASSERT_TRUE(cgroup_ctx);
  EXPECT_EQ(cgroup_ctx->get().memory_protection(), 0);
  cgroup_ctx = ctx_.addToCacheAndGet(CgroupPath(tempDir_, "A/B/D"));
  ASSERT_TRUE(cgroup_ctx);
  EXPECT_EQ(cgroup_ctx->get().memory_protection(), 0);

  // If sum of sibling protection exceeds parent protection, split in proportion
  cgroup_ctx = ctx_.addToCacheAndGet(CgroupPath(tempDir_, "A/E/F"));
  ASSERT_TRUE(cgroup_ctx);
  EXPECT_EQ(cgroup_ctx->get().memory_protection(), 100);
  cgroup_ctx = ctx_.addToCacheAndGet(CgroupPath(tempDir_, "A/E/G"));
  ASSERT_TRUE(cgroup_ctx);
  EXPECT_EQ(cgroup_ctx->get().memory_protection(), 150);
}

/*
 * Verify that CgroupContext won't read from a recreated cgroup.
 */
TEST_F(CgroupContextTest, DistinguishRecreate) {
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
          "system.slice",
          // Dummy file to make cgroup valid
          {F::makeFile("cgroup.controllers"),
           F::makeFile("memory.current", "123\n")})}));

  auto cgroup_ctx = ASSERT_EXISTS(
      CgroupContext::make(ctx_, CgroupPath(tempDir_, "system.slice")));
  ASSERT_EQ(cgroup_ctx.current_usage(), 123);

  // Remove cgroup and recreate one with the exact same name
  F::rmrChecked(tempDir_ + "/system.slice");
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
          "system.slice",
          {F::makeFile("cgroup.controllers"),
           F::makeFile("memory.current", "234\n")})}));

  // This CgroupContext should no longer be valid
  EXPECT_FALSE(cgroup_ctx.refresh());
  // Files under removed-and-recreated cgroup should no longer be accessible
  auto err = CgroupContext::Error::NO_ERROR;
  EXPECT_EQ(cgroup_ctx.current_usage(&err), std::nullopt);
  EXPECT_EQ(err, CgroupContext::Error::INVALID_CGROUP);
}

/*
 * Verify expected values are read from fs.
 * Verify data are cached and not affected by fs changes.
 * Verify fs changes are reflected once refresh() is called.
 */
TEST_F(CgroupContextTest, DataLifeCycle) {
  // Set up cgroup control files
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
          "system.slice",
          // Dummy file to make cgroup valid
          {F::makeFile("cgroup.controllers"),
           F::makeFile(
               "cgroup.stat",
               {"nr_descendants 2\n"
                "nr_dying_descendants 1\n"}),
           F::makeFile(
               "io.pressure",
               {"some avg10=0.04 avg60=0.03 avg300=0.02 total=12345\n"
                "full avg10=0.03 avg60=0.02 avg300=0.01 total=23456\n"}),
           F::makeFile(
               "io.stat",
               {"1:10"
                " rbytes=1111111 wbytes=2222222 rios=33 wios=44"
                " dbytes=5555555555 dios=6\n"
                "1:11"
                " rbytes=2222222 wbytes=3333333 rios=44 wios=55"
                " dbytes=6666666666 dios=7\n"}),
           F::makeFile("memory.current", {"1122334455\n"}),
           F::makeFile("memory.high", {"2233445566\n"}),
           F::makeFile("memory.high.tmp", {"max 0\n"}),
           F::makeFile("memory.low", {"11223344\n"}),
           F::makeFile("memory.max", {"3344556677\n"}),
           F::makeFile("memory.min", {"112233\n"}),
           F::makeFile(
               "memory.pressure",
               {"some avg10=0.31 avg60=0.21 avg300=0.11 total=1234567\n"
                "full avg10=0.30 avg60=0.20 avg300=0.10 total=2345678\n"}),
           F::makeFile(
               "memory.stat",
               {"anon 123456789\n"
                "file 12345678\n"
                "pgscan 4567890123\n"}),
           F::makeFile("memory.swap.current", {"1234\n"}),
           F::makeFile("memory.swap.max", {"1024\n"}),
           F::makeDir("service1.service", {}),
           F::makeDir("service2.service", {}),
           F::makeDir("service3.service", {})})}));

  auto cgroup_ctx = ASSERT_EXISTS(
      CgroupContext::make(ctx_, CgroupPath(tempDir_, "system.slice")));

  std::decay_t<decltype(cgroup_ctx.children())> children;
  std::decay_t<decltype(cgroup_ctx.mem_pressure())> mem_pressure;
  std::decay_t<decltype(cgroup_ctx.io_pressure())> io_pressure;
  std::decay_t<decltype(cgroup_ctx.memory_stat())> memory_stat;
  std::decay_t<decltype(cgroup_ctx.io_stat())> io_stat;
  decltype(cgroup_ctx.current_usage()) current_usage;
  decltype(cgroup_ctx.swap_usage()) swap_usage;
  decltype(cgroup_ctx.swap_max()) swap_max;
  decltype(cgroup_ctx.effective_swap_max()) effective_swap_max;
  decltype(cgroup_ctx.memory_low()) memory_low;
  decltype(cgroup_ctx.memory_min()) memory_min;
  decltype(cgroup_ctx.memory_high()) memory_high;
  decltype(cgroup_ctx.memory_high_tmp()) memory_high_tmp;
  decltype(cgroup_ctx.memory_max()) memory_max;
  decltype(cgroup_ctx.nr_dying_descendants()) nr_dying_descendants;
  decltype(cgroup_ctx.memory_protection()) memory_protection;
  decltype(cgroup_ctx.io_cost_cumulative()) io_cost_cumulative;
  decltype(cgroup_ctx.pg_scan_cumulative()) pg_scan_cumulative;
  decltype(cgroup_ctx.average_usage()) average_usage;
  decltype(cgroup_ctx.io_cost_rate()) io_cost_rate;
  decltype(cgroup_ctx.pg_scan_rate()) pg_scan_rate;

  auto set_and_check_fields = [&]() {
    children = cgroup_ctx.children();
    mem_pressure = cgroup_ctx.mem_pressure();
    io_pressure = cgroup_ctx.io_pressure();
    memory_stat = cgroup_ctx.memory_stat();
    io_stat = cgroup_ctx.io_stat();
    current_usage = cgroup_ctx.current_usage();
    swap_usage = cgroup_ctx.swap_usage();
    swap_max = cgroup_ctx.swap_max();
    effective_swap_max = cgroup_ctx.effective_swap_max();
    memory_low = cgroup_ctx.memory_low();
    memory_min = cgroup_ctx.memory_min();
    memory_high = cgroup_ctx.memory_high();
    memory_high_tmp = cgroup_ctx.memory_high_tmp();
    memory_max = cgroup_ctx.memory_max();
    nr_dying_descendants = cgroup_ctx.nr_dying_descendants();
    memory_protection = cgroup_ctx.memory_protection();
    io_cost_cumulative = cgroup_ctx.io_cost_cumulative();
    pg_scan_cumulative = cgroup_ctx.pg_scan_cumulative();
    average_usage = cgroup_ctx.average_usage();
    io_cost_rate = cgroup_ctx.io_cost_rate();
    pg_scan_rate = cgroup_ctx.pg_scan_rate();

    ASSERT_TRUE(children);
    ASSERT_TRUE(mem_pressure);
    ASSERT_TRUE(io_pressure);
    ASSERT_TRUE(memory_stat);
    ASSERT_TRUE(io_stat);
    ASSERT_TRUE(current_usage);
    ASSERT_TRUE(swap_usage);
    ASSERT_TRUE(swap_max);
    ASSERT_TRUE(effective_swap_max);
    ASSERT_TRUE(memory_low);
    ASSERT_TRUE(memory_min);
    ASSERT_TRUE(memory_high);
    ASSERT_TRUE(memory_high_tmp);
    ASSERT_TRUE(memory_max);
    ASSERT_TRUE(nr_dying_descendants);
    ASSERT_TRUE(memory_protection);
    ASSERT_TRUE(io_cost_cumulative);
    ASSERT_TRUE(pg_scan_cumulative);
    ASSERT_TRUE(average_usage);
    ASSERT_TRUE(io_cost_rate);
  };

  set_and_check_fields();

  using memory_stat_t = decltype(memory_stat);

  // Check basic readings
  EXPECT_THAT(
      *children,
      UnorderedElementsAre(
          "service1.service", "service2.service", "service3.service"));
  EXPECT_FLOAT_EQ(mem_pressure->sec_10, 0.3);
  EXPECT_FLOAT_EQ(mem_pressure->sec_60, 0.2);
  EXPECT_FLOAT_EQ(mem_pressure->sec_300, 0.1);
  EXPECT_EQ(
      mem_pressure->total, std::optional<std::chrono::microseconds>(2345678));
  EXPECT_FLOAT_EQ(io_pressure->sec_10, 0.03);
  EXPECT_FLOAT_EQ(io_pressure->sec_60, 0.02);
  EXPECT_FLOAT_EQ(io_pressure->sec_300, 0.01);
  EXPECT_EQ(
      io_pressure->total, std::optional<std::chrono::microseconds>(23456));
  EXPECT_EQ(
      memory_stat,
      memory_stat_t(
          {{"anon", 123456789}, {"file", 12345678}, {"pgscan", 4567890123}}));
  EXPECT_EQ(
      io_stat,
      IOStat({{"1:10", 1111111, 2222222, 33, 44, 5555555555, 6},
              {"1:11", 2222222, 3333333, 44, 55, 6666666666, 7}}));
  EXPECT_EQ(current_usage, 1122334455);
  EXPECT_EQ(swap_usage, 1234);
  EXPECT_EQ(swap_max, 1024);
  EXPECT_EQ(effective_swap_max, 0);
  EXPECT_EQ(memory_low, 11223344);
  EXPECT_EQ(memory_min, 112233);
  EXPECT_EQ(memory_high, 2233445566);
  EXPECT_EQ(memory_high_tmp, std::numeric_limits<int64_t>::max());
  EXPECT_EQ(memory_max, 3344556677);
  EXPECT_EQ(nr_dying_descendants, 1);
  EXPECT_EQ(memory_protection, 11223344);
  // int64_t(1122334455 / 4.0)
  EXPECT_EQ(average_usage, 280583613);
  // 1111111*5 + 2222222*3 + 33*6 + 44*4 + 5555555555*1 + 6*2 +
  // 2222222*2 + 3333333*4 + 44*1 + 55*3 + 6666666666*6 + 7*5
  EXPECT_EQ(io_cost_cumulative, 45585556178);
  EXPECT_EQ(io_cost_rate, 0);
  EXPECT_EQ(pg_scan_cumulative, 4567890123);
  EXPECT_EQ(pg_scan_rate, std::nullopt);

  // Update most of control files (by adding 1, 0.1 or 0.01)
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
          "system.slice",
          {F::makeFile(
               "cgroup.stat",
               {"nr_descendants 3\n"
                "nr_dying_descendants 2\n"}),
           F::makeFile(
               "io.pressure",
               {"some avg10=0.04 avg60=0.03 avg300=0.02 total=12346\n"
                "full avg10=0.04 avg60=0.03 avg300=0.02 total=23457\n"}),
           F::makeFile(
               "io.stat",
               {"1:10"
                " rbytes=1111112 wbytes=2222223 rios=34 wios=45"
                " dbytes=5555555556 dios=7\n"
                "1:11"
                " rbytes=2222223 wbytes=3333334 rios=45 wios=56"
                " dbytes=6666666667 dios=8\n"}),
           F::makeFile("memory.current", {"1122334456\n"}),
           F::makeFile("memory.high", {"2233445567\n"}),
           F::makeFile("memory.high.tmp", {"max 0\n"}),
           F::makeFile("memory.low", {"11223345\n"}),
           F::makeFile("memory.min", {"112234\n"}),
           F::makeFile("memory.max", {"3344556678\n"}),
           F::makeFile(
               "memory.pressure",
               {"some avg10=0.31 avg60=0.21 avg300=0.11 total=1234568\n"
                "full avg10=0.31 avg60=0.21 avg300=0.11 total=2345679\n"}),
           F::makeFile(
               "memory.stat",
               {"anon 123456790\n"
                "file 12345679\n"
                "pgscan 5678901234\n"}),
           F::makeFile("memory.swap.current", {"1235\n"}),
           F::makeFile("memory.swap.max", {"2048\n"}),
           F::makeDir("service1.service", {}),
           F::makeDir("service2.service", {}),
           F::makeDir("service3.service", {}),
           F::makeDir("service4.service", {})})}));
  F::rmrChecked(tempDir_ + "/system.slice/service2.service");

  // All values should stay the same
  EXPECT_EQ(cgroup_ctx.children(), children);
  EXPECT_EQ(cgroup_ctx.mem_pressure(), mem_pressure);
  EXPECT_EQ(cgroup_ctx.io_pressure(), io_pressure);
  EXPECT_EQ(cgroup_ctx.memory_stat(), memory_stat);
  EXPECT_EQ(cgroup_ctx.io_stat(), io_stat);
  EXPECT_EQ(cgroup_ctx.current_usage(), current_usage);
  EXPECT_EQ(cgroup_ctx.swap_usage(), swap_usage);
  EXPECT_EQ(cgroup_ctx.swap_max(), swap_max);
  EXPECT_EQ(cgroup_ctx.effective_swap_max(), effective_swap_max);
  EXPECT_EQ(cgroup_ctx.memory_low(), memory_low);
  EXPECT_EQ(cgroup_ctx.memory_min(), memory_min);
  EXPECT_EQ(cgroup_ctx.memory_high(), memory_high);
  EXPECT_EQ(cgroup_ctx.memory_high_tmp(), memory_high_tmp);
  EXPECT_EQ(cgroup_ctx.memory_max(), memory_max);
  EXPECT_EQ(cgroup_ctx.nr_dying_descendants(), nr_dying_descendants);
  EXPECT_EQ(cgroup_ctx.memory_protection(), memory_protection);
  EXPECT_EQ(cgroup_ctx.average_usage(), average_usage);
  EXPECT_EQ(cgroup_ctx.io_cost_cumulative(), io_cost_cumulative);
  EXPECT_EQ(cgroup_ctx.io_cost_rate(), io_cost_rate);
  EXPECT_EQ(cgroup_ctx.pg_scan_cumulative(), pg_scan_cumulative);
  EXPECT_EQ(cgroup_ctx.pg_scan_rate(), pg_scan_rate);

  // Call refresh() to clear cache and retrieve values again
  ASSERT_TRUE(cgroup_ctx.refresh());
  set_and_check_fields();

  // Data are now updated
  EXPECT_THAT(
      *children,
      UnorderedElementsAre(
          "service1.service", "service3.service", "service4.service"));
  EXPECT_FLOAT_EQ(mem_pressure->sec_10, 0.31);
  EXPECT_FLOAT_EQ(mem_pressure->sec_60, 0.21);
  EXPECT_FLOAT_EQ(mem_pressure->sec_300, 0.11);
  EXPECT_EQ(
      mem_pressure->total, std::optional<std::chrono::microseconds>(2345679));
  EXPECT_FLOAT_EQ(io_pressure->sec_10, 0.04);
  EXPECT_FLOAT_EQ(io_pressure->sec_60, 0.03);
  EXPECT_FLOAT_EQ(io_pressure->sec_300, 0.02);
  EXPECT_EQ(
      io_pressure->total, std::optional<std::chrono::microseconds>(23457));
  EXPECT_EQ(
      memory_stat,
      memory_stat_t(
          {{"anon", 123456790}, {"file", 12345679}, {"pgscan", 5678901234}}));
  EXPECT_EQ(
      io_stat,
      IOStat({{"1:10", 1111112, 2222223, 34, 45, 5555555556, 7},
              {"1:11", 2222223, 3333334, 45, 56, 6666666667, 8}}));
  EXPECT_EQ(current_usage, 1122334456);
  EXPECT_EQ(swap_usage, 1235);
  EXPECT_EQ(swap_max, 2048);
  EXPECT_EQ(effective_swap_max, 0);
  EXPECT_EQ(memory_low, 11223345);
  EXPECT_EQ(memory_min, 112234);
  EXPECT_EQ(memory_high, 2233445567);
  EXPECT_EQ(memory_high_tmp, std::numeric_limits<int64_t>::max());
  EXPECT_EQ(memory_max, 3344556678);
  EXPECT_EQ(nr_dying_descendants, 2);
  EXPECT_EQ(memory_protection, 11223345);
  // itn64_t(280583613 * (3.0 / 4.0) + 1122334456 / 4.0)
  EXPECT_EQ(average_usage, 491021323);
  // 1111112*5 + 2222223*3 + 34*6 + 45*4 + 5555555556*1 + 7*2 +
  // 2222223*2 + 3333334*4 + 45*1 + 56*3 + 6666666667*6 + 8*5
  EXPECT_EQ(io_cost_cumulative, 45585556220);
  EXPECT_EQ(io_cost_rate, 45585556220 - 45585556178);
  EXPECT_EQ(pg_scan_cumulative, 5678901234);
  EXPECT_EQ(pg_scan_rate, 5678901234 - 4567890123);
}

TEST_F(CgroupContextTest, EffectiveSwapMax) {
  ctx_.setSystemContext(SystemContext{.swaptotal = 3000});
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
          "system.slice",
          {F::makeFile("cgroup.controllers"),
           F::makeFile("memory.swap.max", "1000\n"),
           F::makeDir(
               "oomd.service",
               {F::makeFile("cgroup.controllers"),
                F::makeFile("memory.swap.max", "2000\n")})})}));

  auto cgroup_ctx = ASSERT_EXISTS(CgroupContext::make(
      ctx_, CgroupPath(tempDir_, "system.slice/oomd.service")));

  ASSERT_TRUE(cgroup_ctx.effective_swap_max());
  ASSERT_EQ(*cgroup_ctx.effective_swap_max(), 1000);
}

TEST_F(CgroupContextTest, EffectiveSwapFree) {
  ctx_.setSystemContext(SystemContext{.swaptotal = 400, .swapused = 100});
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
           "system.slice",
           {F::makeFile("cgroup.controllers"),
            F::makeFile("memory.swap.max", "max\n"),
            F::makeFile("memory.swap.current", "100\n"),
            F::makeDir(
                "oomd.service",
                {
                    F::makeFile("cgroup.controllers"),
                    F::makeFile("memory.swap.max", "200\n"),
                    F::makeFile("memory.swap.current", "100\n"),
                })}),
       F::makeDir(
           "foo.slice",
           {F::makeFile("cgroup.controllers"),
            F::makeFile("memory.swap.max", "0\n"),
            F::makeFile("memory.swap.current", "0\n")})}));

  {
    // system.slice is limited by the total amount of the swap on the
    // machine: 400 - 100 = 300
    auto cgroup_ctx = ASSERT_EXISTS(
        CgroupContext::make(ctx_, CgroupPath(tempDir_, "system.slice")));
    ASSERT_TRUE(cgroup_ctx.effective_swap_free());
    ASSERT_EQ(*cgroup_ctx.effective_swap_free(), 300);
  }
  {
    // oomd.service is limited by memory.swap.max
    auto cgroup_ctx = ASSERT_EXISTS(CgroupContext::make(
        ctx_, CgroupPath(tempDir_, "system.slice/oomd.service")));
    ASSERT_TRUE(cgroup_ctx.effective_swap_free());
    ASSERT_EQ(*cgroup_ctx.effective_swap_free(), 100);
  }
  {
    // swap.max of 0 on foo.slice results in 0% util
    auto cgroup_ctx = ASSERT_EXISTS(
        CgroupContext::make(ctx_, CgroupPath(tempDir_, "foo.slice")));
    ASSERT_TRUE(cgroup_ctx.effective_swap_free());
    ASSERT_EQ(*cgroup_ctx.effective_swap_free(), 0);
  }
}

TEST_F(CgroupContextTest, EffectiveSwapUtilPct) {
  ctx_.setSystemContext(SystemContext{.swaptotal = 400, .swapused = 100});
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
           "system.slice",
           {F::makeFile("cgroup.controllers"),
            F::makeFile("memory.swap.max", "max\n"),
            F::makeFile("memory.swap.current", "100\n"),
            F::makeDir(
                "oomd.service",
                {
                    F::makeFile("cgroup.controllers"),
                    F::makeFile("memory.swap.max", "200\n"),
                    F::makeFile("memory.swap.current", "100\n"),
                })}),
       F::makeDir(
           "foo.slice",
           {F::makeFile("cgroup.controllers"),
            F::makeFile("memory.swap.max", "0\n"),
            F::makeFile("memory.swap.current", "0\n")})}));

  {
    // system.slice is limited by the total amount of the swap on the
    // machine - 100 / 400 = 0.25
    auto cgroup_ctx = ASSERT_EXISTS(
        CgroupContext::make(ctx_, CgroupPath(tempDir_, "system.slice")));
    ASSERT_TRUE(cgroup_ctx.effective_swap_util_pct());
    ASSERT_EQ(*cgroup_ctx.effective_swap_util_pct(), 0.25);
  }
  {
    // oomd.service is limited by memory.swap.max
    auto cgroup_ctx = ASSERT_EXISTS(CgroupContext::make(
        ctx_, CgroupPath(tempDir_, "system.slice/oomd.service")));
    ASSERT_TRUE(cgroup_ctx.effective_swap_util_pct());
    ASSERT_EQ(*cgroup_ctx.effective_swap_util_pct(), 0.5);
  }
  {
    // swap.max of 0 on foo.slice results in 0% util
    auto cgroup_ctx = ASSERT_EXISTS(
        CgroupContext::make(ctx_, CgroupPath(tempDir_, "foo.slice")));
    ASSERT_TRUE(cgroup_ctx.effective_swap_util_pct());
    ASSERT_EQ(*cgroup_ctx.effective_swap_util_pct(), 0);
  }
}

TEST_F(CgroupContextTest, EffectiveSwapUtilPctNoSwap) {
  // Check that with no swap available on the root slice, this still works
  ctx_.setSystemContext(SystemContext{.swaptotal = 0, .swapused = 0});
  {
    auto cgroup_ctx =
        ASSERT_EXISTS(CgroupContext::make(ctx_, CgroupPath(tempDir_, "")));
    ASSERT_TRUE(cgroup_ctx.effective_swap_util_pct());
    ASSERT_EQ(*cgroup_ctx.effective_swap_util_pct(), 0);
  }
}

/*
 * Verify that CgroupContext won't read from a recreated cgroup.
 */
TEST_F(CgroupContextTest, MemoryGrowthDoesntDivideByZero) {
  F::materialize(F::makeDir(
      tempDir_,
      {F::makeDir(
          "system.slice",
          // Dummy file to make cgroup valid
          {F::makeFile("cgroup.controllers"),
           F::makeFile("memory.current", "0\n")})}));

  auto cgroup_ctx = ASSERT_EXISTS(
      CgroupContext::make(ctx_, CgroupPath(tempDir_, "system.slice")));

  ASSERT_TRUE(cgroup_ctx.average_usage());
  ASSERT_EQ(*cgroup_ctx.average_usage(), 0);

  ASSERT_NO_THROW(cgroup_ctx.memory_growth());
  ASSERT_TRUE(cgroup_ctx.memory_growth());
  ASSERT_EQ(*cgroup_ctx.memory_growth(), 0);
}
