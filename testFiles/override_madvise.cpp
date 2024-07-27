#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h> // for open
#include <stdio.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <unistd.h>
#include <cerrno> // for errno
#include <cstring> // for strerror
#include <fstream> // for std::ifstream and std::ofstream
#include <iostream> // for std::cerr and std::cout
#include <syslog.h>

typedef int (*orig_madvise_t)(void* addr, size_t length, int advice);

extern "C" int madvise(void* addr, size_t length, int advice) {
  static orig_madvise_t orig_madvise = nullptr;
    openlog("madviseLog", LOG_PID | LOG_CONS, LOG_USER);

  // Ensure thread-safe initialization of orig_madvise
  if (orig_madvise == nullptr) {
    orig_madvise = (orig_madvise_t)dlsym(RTLD_NEXT, "madvise");
    // if (!orig_madvise) {
    //   if (logFile) logFile << "Error locating original madvise: " <<
    //   dlerror() << std::endl; return -1;
    // }
  }

  int shm_fd = shm_open("/indicator_shm", O_RDONLY, 0666);
  if (shm_fd == -1) {
    return orig_madvise(addr, length, advice);
  }

  volatile int* indicator =
      static_cast<int*>(mmap(0, sizeof(int), PROT_READ, MAP_SHARED, shm_fd, 0));
  if (indicator == MAP_FAILED) {
    close(shm_fd);
    return orig_madvise(addr, length, advice);
  }

  if (msync((void*)indicator, sizeof(int), MS_SYNC) == -1) {
    syslog(LOG_INFO, "msync error");
  }
  if (*indicator == 1 && advice == MADV_FREE) {
      syslog(LOG_INFO, "changed advice %d", *indicator);
      advice = MADV_DONTNEED;
  }

  munmap((void*)indicator, sizeof(int));
  close(shm_fd);
  closelog();

  return orig_madvise(addr, length, advice);
}