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

typedef int (*orig_madvise_t)(void* addr, size_t length, int advice);
extern "C" int madvise(void* addr, size_t length, int advice) {
  static orig_madvise_t orig_madvise = nullptr;

  if (orig_madvise == nullptr) {
    orig_madvise = (orig_madvise_t)dlsym(RTLD_NEXT, "madvise");
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

  if (*indicator == 1 && advice == MADV_FREE) {
      advice = MADV_DONTNEED;
  }

  munmap((void*)indicator, sizeof(int));
  close(shm_fd);

  return orig_madvise(addr, length, advice);
}