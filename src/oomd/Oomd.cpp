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

#include "oomd/Oomd.h"

#include <signal.h>
#include <cmath>
#include <functional>
#include <thread>

#include <strings.h>
#include "oomd/CgroupContext.h"
#include "oomd/Log.h"
#include "oomd/dropin/FsDropInService.h"
#include "oomd/include/Assert.h"
#include "oomd/include/Defines.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"
namespace Oomd {

Oomd::Oomd(
    std::unique_ptr<Config2::IR::Root> ir_root,
    std::unique_ptr<Engine::Engine> engine,
    int interval,
    const std::string& cgroup_fs,
    const std::string& drop_in_dir,
    const std::unordered_map<std::string, DeviceType>& io_devs,
    const IOCostCoeffs& hdd_coeffs,
    const IOCostCoeffs& ssd_coeffs)
    : interval_(interval),
      ir_root_(std::move(ir_root)),
      engine_(std::move(engine)) {
  ContextParams params{
      .io_devs = io_devs,
      .hdd_coeffs = hdd_coeffs,
      .ssd_coeffs = ssd_coeffs,
  };
  ctx_ = OomdContext(params);
  if (drop_in_dir.size()) {
    fs_drop_in_service_ =
        FsDropInService::create(cgroup_fs, *ir_root_, *engine_, drop_in_dir);
  }
}

Oomd::~Oomd() = default;

void Oomd::updateContext() {
  // Update information about swapfree
  SystemContext system_ctx;
  auto swaps = Fs::readFileByLine("/proc/swaps");

  // TODO(dschatzberg): Handle error here
  if (swaps) {
    // For each swap, tally up used and total
    for (size_t i = 1; i < swaps->size(); ++i) {
      auto parts = Util::split((*swaps)[i], '\t');
      // The /proc/swaps format is pretty bad. The first field is padded by
      // spaces but the rest of the fields are padded by '\t'. Since we don't
      // really care about the first field, we'll just split by '\t'.
      OCHECK_EXCEPT(
          parts.size() == 4, std::runtime_error("/proc/swaps malformed"));
      system_ctx.swaptotal += std::stoll(parts[1]) * 1024; // Values are in KB
      system_ctx.swapused += std::stoll(parts[2]) * 1024; // Values are in KB
    }
  }

  auto swappiness = Fs::getSwappiness();
  if (swappiness) {
    system_ctx.swappiness = *swappiness;
  }

  if (auto vmstat_opt = Fs::getVmstat()) {
    system_ctx.vmstat = std::move(*vmstat_opt);

    // Factor for calculating moving average
    const static double factor60 = std::exp(-interval_.count() / 60.0);
    const static double factor300 = std::exp(-interval_.count() / 300.0);

    auto& prev_system_ctx = ctx_.getSystemContext();
    if (prev_system_ctx.vmstat.size() > 0) {
      auto swapout_bps = (system_ctx.vmstat.at("pswpout") -
                          prev_system_ctx.vmstat.at("pswpout")) *
          4096.0 / interval_.count();
      system_ctx.swapout_bps = swapout_bps;
      system_ctx.swapout_bps_60 = swapout_bps +
          factor60 * (prev_system_ctx.swapout_bps_60 - swapout_bps);
      system_ctx.swapout_bps_300 = swapout_bps +
          factor300 * (prev_system_ctx.swapout_bps_300 - swapout_bps);
    }
  }

  ctx_.setSystemContext(system_ctx);
  ctx_.setPrekillHooksHandler([&](const CgroupContext& cgroup_ctx) {
    return engine_->firePrekillHook(cgroup_ctx, ctx_);
  });
  ctx_.refresh();
  ctx_.bumpCurrentTick();
}

int Oomd::run(const sigset_t* mask) {
  if (!engine_) {
    OLOG << "Could not run engine. Your config file is probably invalid\n";
    return EXIT_CANT_RECOVER;
  }

  struct timespec ts {
    .tv_sec = interval_.count(), .tv_nsec = 0,
  };

  OLOG << "Running oomd";

  while (true) {
    // sigtimedwait so if we get a SIGINT or SIGTERM we wake up and exit right
    // away.
    int rc;
    do {
      rc = sigtimedwait(mask, nullptr, &ts);
    } while (rc == -1 && errno == EINTR);

    if (rc == -1 && errno != EAGAIN) {
      perror("sigtimedwait");
      exit(EXIT_FAILURE);
    } else if (rc != -1) {
      // Synchronously log to stderr so there's no buffering
      std::cerr << "Received signal " << rc << ". Exiting!";
      pthread_sigmask(SIG_UNBLOCK, mask, nullptr);
      pthread_kill(pthread_self(), rc);
      return -1;
    }

    if (fs_drop_in_service_) {
      fs_drop_in_service_->updateDropIns();
    }

    updateContext();

    // Prerun all the plugins
    engine_->prerun(ctx_);

    // Run all the plugins
    engine_->runOnce(ctx_);
  }

  return 0;
}

} // namespace Oomd
