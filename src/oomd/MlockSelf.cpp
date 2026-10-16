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

#include "oomd/MlockSelf.h"

#include <sys/mman.h>
#include <unistd.h>

#include <cerrno>
#include <climits>
#include <cstdint>
#include <cstdio>
#include <exception>
#include <memory>

#include "oomd/Log.h"

namespace Oomd {

// Based on Meta-internal common/memory/MlockProcess.cpp:mlockSelf
// (https://fburl.com/code/2o1vos2l), itself derived from HHVM's pagein_self.
size_t mlockSelf() {
  size_t locked = 0;
  size_t failed = 0;
  int last_errno = 0;
  uintptr_t begin, end, inode, pgoff;
  char mapname[PATH_MAX];
  static_assert(sizeof(mapname) == 4096, "update the %4095s width below");
  char perm[5];
  char dev[6];

  int bufsz = sizeof(uintptr_t) * 4 + sizeof(mapname) + sizeof(char) * 11;
  // pad due to the spaces between the inode number and the mapname
  bufsz += 100;

  std::unique_ptr<char[]> buf(new char[bufsz]);
  FILE* fp = fopen("/proc/self/maps", "r");
  if (fp == nullptr) {
    const int open_errno = errno;
    OLOG << "mlock-self: failed to open /proc/self/maps errno=" << open_errno;
    return 0;
  }

  while (!feof(fp)) {
    if (fgets(buf.get(), bufsz, fp) == nullptr) {
      break;
    }

    int r = sscanf(
        buf.get(),
        // Unlike the original, bound the pathname: PATH_MAX limits syscall path
        // arguments, but /proc/self/maps prints a mapping's full path, which
        // can be longer (e.g. a deep tree created with relative paths).
        "%lx-%lx %4s %lx %5s %lu %4095s",
        &begin,
        &end,
        perm,
        &pgoff,
        dev,
        &inode,
        mapname);

    // page in read-only segments that correspond to a file on disk
    if (r != 7 || perm[0] != 'r' || perm[1] != '-' ||
        access(mapname, F_OK) != 0) {
      continue;
    }

    // NOLINTNEXTLINE(performance-no-int-to-ptr)
    void* addr = reinterpret_cast<void*>(begin);
    size_t len = end - begin;
    // advise kernel to load them up in memory
    madvise(addr, len, MADV_WILLNEED);
    if (mlock(addr, len) == 0) {
      locked += len;
    } else {
      failed += len;
      last_errno = errno;
    }
  }

  fclose(fp);
  OLOG << "mlock-self: locked=" << locked << " failed=" << failed
       << " errno=" << last_errno;
  return locked;
}

std::thread mlockSelfAsync() {
  try {
    return std::thread([] {
      try {
        mlockSelf();
      } catch (...) {
        OLOG << "mlock-self failed with an exception";
      }
    });
  } catch (const std::exception& e) {
    OLOG << "mlock-self: failed to start thread: " << e.what();
  }
  return {};
}

} // namespace Oomd
