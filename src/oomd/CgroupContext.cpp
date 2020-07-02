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

#include "oomd/CgroupContext.h"

#include "oomd/OomdContext.h"

namespace Oomd {

std::optional<CgroupContext> CgroupContext::make(
    OomdContext& ctx,
    const CgroupPath& cgroup) {
  if (auto fd = Fs::DirFd::open(cgroup.absolutePath())) {
    return CgroupContext(ctx, cgroup, std::move(*fd));
  }
  return std::nullopt;
}

CgroupContext::CgroupContext(
    OomdContext& ctx,
    const CgroupPath& path,
    Fs::DirFd&& dirFd)
    : ctx_(ctx),
      cgroup_(path),
      cgroup_dir_(std::move(dirFd)),
      data_(std::make_unique<CgroupData>()) {}

bool CgroupContext::refresh() {
  archive_ = {.average_usage = data_->average_usage,
              .io_cost_cumulative = data_->io_cost_cumulative,
              .pg_scan_cumulative = data_->pg_scan_cumulative};
  *data_ = {};
  return Fs::isCgroupValid(cgroup_dir_);
}

/*
 * Use macro to define proxy functions to access the underlying data object.
 * If a field is not set, set it with the result of a given expression. If the
 * expression throws Fs::bad_control_file (most likely cgroup is gone), we set
 * the err to INVALID_CGROUP if it's not nullptr. The given expression can also
 * use the err pointer as a mechanism to return error without throwing.
 *
 * Because data_ is a pointer, we can do this lazy set and get with const
 * function signature.
 */
#define __PROXY(field, expr, rettype)              \
  rettype CgroupContext::field(Error* err) const { \
    if (!data_->field) {                           \
      try {                                        \
        data_->field = (expr);                     \
        if (!data_->field && err) {                \
          *err = Error::INVALID_CGROUP;            \
        }                                          \
      } catch (const Fs::bad_control_file&) {      \
        if (err) {                                 \
          *err = Error::INVALID_CGROUP;            \
        }                                          \
      }                                            \
    }                                              \
    return data_->field;                           \
  }

#define FIELD_TYPE(field) decltype(CgroupContext::CgroupData::field)
#define PROXY(field, expr) __PROXY(field, expr, FIELD_TYPE(field))
#define PROXY_CONST_REF(field, expr) \
  __PROXY(field, expr, const FIELD_TYPE(field)&)

PROXY_CONST_REF(children, getChildren())
PROXY_CONST_REF(mem_pressure, getMemPressure())
PROXY_CONST_REF(io_pressure, getIoPressure())
PROXY_CONST_REF(memory_stat, Fs::getMemstatAt(cgroup_dir_))
PROXY_CONST_REF(io_stat, Fs::readIostatAt(cgroup_dir_))
PROXY(id, cgroup_dir_.inode())
PROXY(current_usage, getMemcurrent())
PROXY(swap_usage, Fs::readSwapCurrentAt(cgroup_dir_))
PROXY(memory_low, Fs::readMemlowAt(cgroup_dir_))
PROXY(memory_min, Fs::readMemminAt(cgroup_dir_))
PROXY(memory_high, Fs::readMemhighAt(cgroup_dir_))
PROXY(memory_high_tmp, Fs::readMemhightmpAt(cgroup_dir_))
PROXY(memory_max, Fs::readMemmaxAt(cgroup_dir_))
PROXY(nr_dying_descendants, Fs::getNrDyingDescendantsAt(cgroup_dir_))
PROXY(is_populated, Fs::readIsPopulatedAt(cgroup_dir_))
PROXY(kill_preference, Fs::readKillPreferenceAt(cgroup_dir_))
PROXY(oom_group, Fs::readMemoryOomGroupAt(cgroup_dir_))
PROXY(io_cost_cumulative, getIoCostCumulative(err))
PROXY(pg_scan_cumulative, getPgScanCumulative(err))
PROXY(memory_protection, getMemoryProtection(err))
PROXY(io_cost_rate, getIoCostRate(err))
PROXY(average_usage, getAverageUsage(err))
PROXY(pg_scan_rate, getPgScanRate(err))

std::optional<int64_t> CgroupContext::anon_usage(Error* err) const {
  if (const auto& stat = memory_stat(err)) {
    if (auto anon = stat->find("anon"); anon != stat->end()) {
      return anon->second;
    } else if (err) {
      *err = Error::INVALID_CGROUP;
    }
  }
  return 0;
}

std::optional<int64_t> CgroupContext::effective_usage(
    Error* err,
    int64_t memory_scale,
    int64_t memory_adj) const {
  if (!current_usage(err) || !memory_protection(err)) {
    return std::nullopt;
  }
  return *current_usage() * memory_scale - *memory_protection() + memory_adj;
}

std::optional<double> CgroupContext::memory_growth(Error* err) const {
  if (!current_usage(err) || !average_usage(err)) {
    return std::nullopt;
  }
  return static_cast<double>(*current_usage()) / *average_usage();
}

std::vector<std::string> CgroupContext::getChildren() const {
  return Fs::readDir(cgroup_.absolutePath(), Fs::DE_DIR).dirs;
}

std::optional<ResourcePressure> CgroupContext::getMemPressure() const {
  return cgroup_.isRoot() ? Fs::readRootMempressure()
                          : Fs::readMempressureAt(cgroup_dir_);
}

std::optional<ResourcePressure> CgroupContext::getIoPressure() const {
  return cgroup_.isRoot() ? Fs::readRootIopressure()
                          : Fs::readIopressureAt(cgroup_dir_);
}

std::optional<int64_t> CgroupContext::getMemcurrent() const {
  return cgroup_.isRoot() ? Fs::readRootMemcurrent()
                          : Fs::readMemcurrentAt(cgroup_dir_);
}

namespace {
std::optional<int64_t> rawProtection(
    const CgroupContext& ctx,
    CgroupContext::Error* err = nullptr) {
  if (!ctx.current_usage(err) || !ctx.memory_min(err) || !ctx.memory_low(err)) {
    return std::nullopt;
  }
  return std::min(
      ctx.current_usage(), std::max(ctx.memory_min(), ctx.memory_low()));
}

std::optional<int64_t> normalizedProtection(
    const CgroupContext& ctx,
    const CgroupContext& parent_ctx,
    int64_t protection_sum,
    CgroupContext::Error* err = nullptr) {
  if (protection_sum == 0) {
    // If the cgroup isn't using any memory then it's trivially true it's
    // not receiving any protection
    return 0;
  } else {
    auto raw_prot = rawProtection(ctx, err);
    if (!raw_prot) {
      return std::nullopt;
    }
    auto parent_prot = parent_ctx.memory_protection(err);
    if (!parent_prot) {
      return std::nullopt;
    }
    return *raw_prot * std::min(1.0, 1.0 * *parent_prot / protection_sum);
  }
}
} // namespace

/*
 * Calculate memory protection, taking into account actual distribution of
 * memory protection.
 *
 * Let's say R(cgrp) is the raw protection amount a cgroup has according to its
 * own config, P(cgrp) is the amount of actual protection it gets. This function
 * returns P(cgrp).
 *
 * Let R(cgrp) = min(cgrp.memory.current, max(cgrp.memory.min,
 * cgrp.memory.low))
 *
 * Then, P(cgrp) = R(cgrp) * min(1.0, P(parent) / (Sum of L(child) for each
 * children of parent))
 */
std::optional<int64_t> CgroupContext::getMemoryProtection(Error* err) const {
  if (cgroup_.isRoot()) {
    return current_usage(err);
  }

  auto parent_cgroup = cgroup_.getParent();
  if (parent_cgroup.isRoot()) {
    // We're at a top level cgroup where P(cgrp) == R(cgrp)
    return rawProtection(*this, err);
  }
  auto parent_ctx = ctx_.addToCacheAndGet(parent_cgroup);
  if (!parent_ctx) {
    if (err) {
      *err = Error::INVALID_CGROUP;
    }
    return std::nullopt;
  }

  std::unordered_set<CgroupPath> sibling_cgroups;
  std::vector<OomdContext::ConstCgroupContextRef> siblings;
  if (auto children = parent_ctx->get().children(err)) {
    for (const auto& name : *children) {
      sibling_cgroups.insert(parent_cgroup.getChild(name));
    }
    siblings = ctx_.addToCacheAndGet(sibling_cgroups);
  } else {
    return std::nullopt;
  }

  int64_t protection_sum = 0;
  for (auto sibling_ctx : siblings) {
    protection_sum += rawProtection(sibling_ctx).value_or(0);
  }
  return normalizedProtection(*this, *parent_ctx, protection_sum, err);
}

std::optional<double> CgroupContext::getIoCostCumulative(Error* err) const {
  if (!io_stat(err)) {
    return std::nullopt;
  }
  const auto& params = ctx_.getParams();
  double cost = 0.0;
  // calculate the sum of cumulative io cost on all devices.
  for (const auto& stat : *io_stat()) {
    // only keep stats from devices we care
    auto dev = params.io_devs.find(stat.dev_id);
    if (dev == params.io_devs.end()) {
      continue;
    }
    IOCostCoeffs coeffs;
    switch (dev->second) {
      case DeviceType::SSD:
        coeffs = params.ssd_coeffs;
        break;
      case DeviceType::HDD:
        coeffs = params.hdd_coeffs;
        break;
    }
    // Dot product between dev io stat and io cost coeffs. A more sensible way
    // is to do dot product between rate of change (bandwidth, iops) with coeffs
    // but since the coeffs are constant, we can calculate rate of change later.
    cost += stat.rios * coeffs.read_iops + stat.rbytes * coeffs.readbw +
        stat.wios * coeffs.write_iops + stat.wbytes * coeffs.writebw +
        stat.dios * coeffs.trim_iops + stat.dbytes * coeffs.trimbw;
  }
  return cost;
}

std::optional<int64_t> CgroupContext::getPgScanCumulative(
    Error* err = nullptr) const {
  static constexpr auto kPgScan = "pgscan";
  if (const auto& memstat = memory_stat(err)) {
    if (auto pos = memstat->find(kPgScan); pos != memstat->end()) {
      return std::make_optional(pos->second);
    }
  }
  return std::nullopt;
}

std::optional<int64_t> CgroupContext::getAverageUsage(Error* err) const {
  if (!current_usage(err)) {
    return std::nullopt;
  }
  auto prev_avg = archive_.average_usage.value_or(0);
  auto decay = ctx_.getParams().average_size_decay;
  return prev_avg * ((decay - 1) / decay) + (*current_usage() / decay);
}

std::optional<double> CgroupContext::getIoCostRate(Error* err) const {
  if (!io_cost_cumulative(err)) {
    return std::nullopt;
  }
  return !archive_.io_cost_cumulative
      ? 0.0
      : *io_cost_cumulative() - *archive_.io_cost_cumulative;
}

std::optional<int64_t> CgroupContext::getPgScanRate(Error* err) const {
  if (!pg_scan_cumulative(err)) {
    return std::nullopt;
  }
  return !archive_.pg_scan_cumulative
      ? 0.0
      : *pg_scan_cumulative() - *archive_.pg_scan_cumulative;
}

} // namespace Oomd
