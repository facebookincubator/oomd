#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>
#include <sys/mman.h>
#include <fstream> // for std::ifstream
#include <iostream> // for std::cerr

typedef int (*orig_madvise_t)(void* addr, size_t length, int advice);

int madvise(void* addr, size_t length, int advice) {
  static orig_madvise_t orig_madvise = NULL;
  if (orig_madvise == NULL) {
    orig_madvise = (orig_madvise_t)dlsym(RTLD_NEXT, "madvise");
  }
  std::ifstream file("/indicator");
  if (!file) {
    // If file doesn't exist or can't be opened, fall back to original behavior
    return orig_madvise(addr, length, advice);
  }

  int indicator;
  file >> indicator;
  file.close();

  if (indicator == 1 && advice == MADV_DONTNEED) {
    advice = MADV_FREE;
  }

  return orig_madvise(addr, length, advice);
}