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

  // Ensure thread-safe initialization of orig_madvise
  if (orig_madvise == nullptr) {
    orig_madvise = (orig_madvise_t)dlsym(RTLD_NEXT, "madvise");
    if (!orig_madvise) {
      std::cerr << "Error locating original madvise: " << dlerror()
                << std::endl;
      return -1;
    }
  }

  const char* indicatorFilePath = "/home/guyy/oomd/testFiles/indicator";
  int fd = open(indicatorFilePath, O_RDONLY);
  if (fd == -1) {
    std::cerr
        << "Could not open indicator file, falling back to original madvise"
        << ": " << strerror(errno) << std::endl;
    return orig_madvise(addr, length, advice);
  }

  // Busy wait until the lock can be acquired
  while (flock(fd, LOCK_SH) == -1) {
    usleep(1000); // Sleep for 1ms before trying again
  }

  // Use std::ifstream to read the file's contents
  std::ifstream file(indicatorFilePath);
  int indicator;
  file >> indicator;
  file.close();

  // Release the lock
  while (flock(fd, LOCK_UN) == -1) {
    std::cerr << "Could not unlock indicator file, retrying..." << std::endl;
    usleep(1000);
  }

  close(fd);

  if (indicator == 1 && advice == MADV_FREE) {
    const char* filePath = "/home/guyy/oomd/testFiles/output_madvise.txt";
    std::ofstream outputFile(filePath, std::ios::out | std::ios::app);
    if (outputFile) {
      outputFile << "Changing madvise advice from MADV_FREE to MADV_DONTNEED"
                 << std::endl;
      outputFile.close();
    } else {
      std::cerr << "Could not open log file for writing" << std::endl;
    }
    advice = MADV_DONTNEED;
  }

  return orig_madvise(addr, length, advice);
}