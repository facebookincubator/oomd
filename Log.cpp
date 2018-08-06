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

#include "oomd/Log.h"

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <iomanip>
#include <iostream>
#include <system_error>

#include <folly/logging/xlog.h>

#include "oomd/shared/OomdContext.h"
#include "oomd/util/Fs.h"

namespace Oomd {

Log::Log(int kmsg_fd) : kmsg_fd_(kmsg_fd) {}

Log::~Log() {
  if (kmsg_fd_ >= 0) {
    ::close(kmsg_fd_);
  }
}

void Log::init_or_die() {
  int kmsg_fd = ::open("/dev/kmsg", O_WRONLY);
  if (kmsg_fd < 0) {
    const int errcode = errno; // prevent errno clobbering
    perror("open");
    XLOG(ERR) << "Unable to open outfile (default=/dev/kmsg), not logging";
    throw std::system_error(errcode, std::generic_category());
  }

  Log::get(kmsg_fd);
}

Log& Log::get(int kmsg_fd) {
  static Log singleton(kmsg_fd);
  return singleton;
}

std::unique_ptr<Log> Log::get_for_unittest(int kmsg_fd) {
  return std::unique_ptr<Log>(new Log(kmsg_fd));
}

void Log::kmsgLog(const std::string& buf, const std::string& prefix) const {
  if (kmsg_fd_ >= 0) {
    std::string message(buf);
    if (prefix.size() > 0) {
      message.insert(0, prefix + ": ");
    }
    auto ret = Fs::writeFull(kmsg_fd_, message.data(), message.size());
    if (ret == -1) {
      perror("error writing");
      XLOG(ERR) << "Unable to write log to output file";
    }
  }

  XLOG(INFO) << buf;
}

void Log::kmsgLog(
    const std::string& to_kill,
    const std::string& kill_type,
    const CgroupContext& context,
    const OomContext& oom_context,
    bool dry) const {
  // Parse out OOM detection context
  std::ostringstream octx_oss;
  switch (oom_context.type) {
    case OomType::SWAP:
      octx_oss << "swap," << oom_context.stat.swap_free << "MB";
      break;
    case OomType::PRESSURE_10:
      octx_oss << "pressure10," << oom_context.stat.pressure_10_duration << "s";
      break;
    case OomType::PRESSURE_60:
      octx_oss << "pressure60";
      break;
    case OomType::KILL_LIST:
      octx_oss << "killlist";
      break;
    default:
      octx_oss << "none";
      break;
  }

  std::ostringstream oss;
  oss << std::setprecision(2) << std::fixed;
  oss << context.pressure.sec_10 << " " << context.pressure.sec_60 << " "
      << context.pressure.sec_600 << " " << to_kill << " "
      << context.current_usage << " "
      << "detector:" << octx_oss.str() << " "
      << "killer:" << (dry ? "(dry)" : "") << kill_type;
  kmsgLog(oss.str(), "oomd kill");
}

} // namespace Oomd
