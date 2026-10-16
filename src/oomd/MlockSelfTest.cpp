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

#include <fcntl.h>
#include <gtest/gtest.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#include <fstream>
#include <functional>
#include <string>
#include <thread>
#include <vector>

#include "oomd/MlockSelf.h"

using namespace Oomd;

namespace {

// GTEST_SKIP() is missing from gtest older than 1.10.
#ifdef GTEST_SKIP
#define SKIP_TEST(msg) GTEST_SKIP() << msg
#else
#define SKIP_TEST(msg) return
#endif

size_t vmLckBytes() {
  std::ifstream status("/proc/self/status");
  std::string key;
  size_t kb = 0;
  while (status >> key) {
    if (key == "VmLck:" && status >> kb) {
      return kb * 1024;
    }
  }
  return 0;
}

// Sanitizers intercept mlock() and return 0 without making the syscall, so a
// page only counts as locked if VmLck actually grew.
bool canMlockPage() {
  const long page = ::sysconf(_SC_PAGESIZE);
  void* addr = ::mmap(
      nullptr,
      page,
      PROT_READ | PROT_WRITE,
      MAP_PRIVATE | MAP_ANONYMOUS,
      -1,
      0);
  if (addr == MAP_FAILED) {
    return false;
  }
  const size_t before = vmLckBytes();
  const bool locked = ::mlock(addr, page) == 0 && vmLckBytes() >= before + page;
  ::munmap(addr, page);
  return locked;
}

constexpr int kSkip = 77;

// Runs fn in a forked child; returns its exit code, or -1 if it didn't exit
// normally (e.g. it was killed by a stack-protector abort).
int runInChild(const std::function<int()>& fn) {
  const pid_t pid = ::fork();
  if (pid == -1) {
    return -2;
  }
  if (pid == 0) {
    ::_exit(fn());
  }
  int status = 0;
  if (::waitpid(pid, &status, 0) != pid || !WIFEXITED(status)) {
    return -1;
  }
  return WEXITSTATUS(status);
}

} // namespace

TEST(MlockSelfTest, LocksOwnMappings) {
  if (!canMlockPage()) {
    SKIP_TEST("mlock not permitted or intercepted by a sanitizer");
  }
  const size_t before = vmLckBytes();
  const size_t locked = mlockSelf();
  EXPECT_GT(locked, 0);
  EXPECT_GE(vmLckBytes() - before, locked);
  ::munlockall();
}

TEST(MlockSelfTest, LocksNothingWithoutMemlockLimit) {
  const int code = runInChild([] {
    struct rlimit limit{};
    ::getrlimit(RLIMIT_MEMLOCK, &limit);
    limit.rlim_cur = 0;
    // Skip if mlock() still succeeds: CAP_IPC_LOCK or a sanitizer interceptor.
    if (::setrlimit(RLIMIT_MEMLOCK, &limit) != 0 ||
        ::mlock(&limit, sizeof(limit)) == 0) {
      return kSkip;
    }
    return mlockSelf() == 0 ? 0 : 1;
  });
  if (code == kSkip) {
    SKIP_TEST("mlock ignores RLIMIT_MEMLOCK here");
  }
  EXPECT_EQ(code, 0);
}

TEST(MlockSelfTest, ContinuesWithoutThread) {
  const int code = runInChild([] {
    struct rlimit limit{};
    ::getrlimit(RLIMIT_NPROC, &limit);
    limit.rlim_cur = 0;
    ::setrlimit(RLIMIT_NPROC, &limit);
    try {
      std::thread([] {}).join();
      return kSkip;
    } catch (const std::exception&) {
    }
    if (mlockSelfAsync().joinable()) {
      return 1;
    }
    return vmLckBytes() == 0 ? 0 : 2;
  });
  if (code == kSkip) {
    SKIP_TEST("RLIMIT_NPROC doesn't stop thread creation here");
  }
  EXPECT_EQ(code, 0);
}

// The kernel can report a mapping path longer than PATH_MAX when the file was
// reached through relative paths.
TEST(MlockSelfTest, HandlesPathLongerThanPathMax) {
  char dir[] = "/tmp/mlock_self_test.XXXXXX";
  ASSERT_NE(::mkdtemp(dir), nullptr);
  const int code = runInChild([&dir] {
    int fd = ::open(dir, O_RDONLY | O_DIRECTORY);
    for (int i = 0; fd >= 0 && i < 22; ++i) {
      const std::string name = std::to_string(i) + std::string(198, 'x');
      if (::mkdirat(fd, name.c_str(), 0700) != 0) {
        return 3;
      }
      const int next = ::openat(fd, name.c_str(), O_RDONLY | O_DIRECTORY);
      ::close(fd);
      fd = next;
    }
    const int file = ::openat(fd, "file", O_CREAT | O_RDWR, 0600);
    if (file < 0 || ::ftruncate(file, 4096) != 0 ||
        ::mmap(nullptr, 4096, PROT_READ, MAP_PRIVATE, file, 0) == MAP_FAILED) {
      return 4;
    }
    // Unlink now: the mapping keeps the file alive for mlockSelf().
    ::unlinkat(fd, "file", 0);
    mlockSelf();
    return 0;
  });
  EXPECT_EQ(code, 0);
  // The child unlinked the file; remove the directory chain from the top.
  int fd = ::open(dir, O_RDONLY | O_DIRECTORY);
  std::vector<int> fds{fd};
  for (int i = 0; fd >= 0 && i < 21; ++i) {
    fd = ::openat(
        fd,
        (std::to_string(i) + std::string(198, 'x')).c_str(),
        O_RDONLY | O_DIRECTORY);
    fds.push_back(fd);
  }
  for (int i = 21; i >= 0; --i) {
    if (fds[i] >= 0) {
      ::unlinkat(
          fds[i],
          (std::to_string(i) + std::string(198, 'x')).c_str(),
          AT_REMOVEDIR);
      ::close(fds[i]);
    }
  }
  ::rmdir(dir);
}
