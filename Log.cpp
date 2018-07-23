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

Log::Log(const char* outfile) {
  kmsg_fd_ = ::open(outfile, O_WRONLY);
  if (kmsg_fd_ < 0) {
    const int errcode = errno; // prevent errno clobbering
    perror("open");
    XLOG(ERR) << "Unable to open outfile (default=/dev/kmsg), not logging";
    throw std::system_error(errcode, std::generic_category());
  }
}

Log::~Log() {
  if (kmsg_fd_ >= 0) {
    ::close(kmsg_fd_);
  }
}

void Log::init_or_die() {
  try {
    Log::get();
  } catch (const std::system_error& e) {
    XLOG(FATAL) << "Log failed to init. Aborting.";
  }
}

Log& Log::get() {
  static Log singleton;
  return singleton;
}

std::unique_ptr<Log> Log::get_for_unittest(const char* outfile) {
  return std::unique_ptr<Log>(new Log(outfile));
}

void Log::log(const std::string& buf, const std::string& prefix) const {
  if (kmsg_ && kmsg_fd_ >= 0) {
    std::string message(buf);
    if (prefix.size() > 0) {
      message.insert(0, prefix + ": ");
    }
    auto ret = Fs::writeFull(kmsg_fd_, message.data(), message.size());
    if (ret == -1) {
      perror("error writing");
      XLOG(ERR) << "Unable to write log to output file";

      // Prevents double printing to stderr
      if (!stderr_) {
        XLOG(ERR) << buf;
      }
    }
  }

  if (stderr_) {
    // glog, by default, logs to stderr (unless turned off via cmdline)
    XLOG(INFO) << buf;
  }
}

void Log::log(
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
  log(oss.str(), "oomd kill");
}

} // namespace Oomd
