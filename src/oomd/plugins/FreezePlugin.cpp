#include "oomd/plugins/FreezePlugin.h"
#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/include/Types.h"
#include "oomd/util/FreezeUtills.h"
#include "oomd/util/Util.h"

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>
#include <cerrno>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include "../../../usr/include/x86_64-linux-gnu/sys/stat.h"

#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"
#define FREEZER_PATH "/sys/fs/cgroup/freezer"
#define SYSCALL_FAILED -1

namespace Oomd {

REGISTER_PLUGIN(freeze, FreezePlugin::create);

// Wrapper for pidfd_open syscall
int pidfd_open(pid_t pid, unsigned int flags) {
  return syscall(SYS_pidfd_open, pid, flags);
}

// Wrapper function for process_madvise
int process_madvise(
    int pidfd,
    const struct iovec* vec,
    size_t vlen,
    int advice,
    unsigned long flags) {
  if (pidfd == SYSCALL_FAILED) {
    Util::logError("pidfd_open");
    return SYSCALL_FAILED;
  }

  if (syscall(SYS_process_madvise, pidfd, vec, vlen, advice, flags) == SYSCALL_FAILED) {
    close(pidfd);
    return SYSCALL_FAILED;
  }

  return pidfd;
}

int FreezePlugin::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  createFreezer();
  return BaseKillPlugin::init(args, context);
}


int FreezePlugin::tryToKillPids(const std::vector<int>& procs) {
  for (auto pid : procs) {
    handleProcess(pid);
  }
  return 0; // TODO: change to count of frozen processes?
}


std::vector<OomdContext::ConstCgroupContextRef>
FreezePlugin::rankForKilling(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {
  return OomdContext::sortDescWithKillPrefs(
      cgroups, [this](const CgroupContext& cgroup_ctx) {
        return cgroup_ctx.current_usage().value_or(0);
      });
}

void FreezePlugin::ologKillTarget(
    OomdContext& ctx,
    const CgroupContext& target,
    const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */) {
  OLOG << "Nitzan and Guy freezed \"" << target.cgroup().relativePath()
       << "\" (" << target.current_usage().value_or(0) / 1024 / 1024
       << "MB) based on swap usage at "
       << target.swap_usage().value_or(0) / 1024 / 1024 << "MB;";
}


void FreezePlugin::handleProcess(int pid) {
  if (!createFreezeCgroup()) {
    OLOG << "Failed to create freeze cgroup";
    return;
  }
  freezeProcess(pid);

  if (!pageOutMemory(pid)) {
    OLOG << "Failed to page out memory";
    return;
  }
}

bool FreezePlugin::createFreezeCgroup(void) {
  // Create the cgroup directory
  if (mkdir(CGROUP_PATH, 0755) && errno != EEXIST) {
    Util::logError("create cgroup directory");
    return false;
  }
  OLOG << "Successfully created or found existing cgroup directory: "
       << CGROUP_PATH;
  return true;
}

void FreezePlugin::freezeProcess(int pid) {
  // Add the process to the cgroup
  if (pid <= 0) {
    OLOG << "Invalid PID: " << pid;
    return;
  }
  char tasks_path[256];
  snprintf(tasks_path, sizeof(tasks_path), "%s/tasks", CGROUP_PATH);
  char pid_str[16];
  snprintf(pid_str, sizeof(pid_str), "%d", pid);
  writeToFile(tasks_path, pid_str);

  // Freeze the process
  char state_path[256];
  snprintf(state_path, sizeof(state_path), "%s/freezer.state", CGROUP_PATH);
  writeToFile(state_path, "FROZEN");
  OLOG << "process: " << pid << "is now frozen!";
}

bool FreezePlugin::swapHasFreeMB(int megabyte) {
  FILE* meminfo_file = fopen("/proc/meminfo", "r");
  if (!meminfo_file) {
    Util::logError("fopen");
    return false;
  }

  char line[256];
  unsigned long swapFree = 0;

  while (fgets(line, sizeof(line), meminfo_file)) {
    if (sscanf(line, "SwapFree: %lu kB", &swapFree) == 1) {
      break;
    }
  }

  fclose(meminfo_file);

  if (swapFree < megabyte * 1024) {
    OLOG << "Not enough free swap space: " << swapFree << " kB";
    return false;
  }

  return true;
}

bool FreezePlugin::pageOutMemory(int pid) {
  // Open and read /proc/[pid]/maps
  char maps_path[256];
  snprintf(maps_path, sizeof(maps_path), "/proc/%d/maps", pid);
  FILE* maps_file = fopen(maps_path, "r");
  if (!maps_file) {
    Util::logError("fopen");
    return false;
  }

  // Open the pidfd
  int pidfd = Oomd::pidfd_open(pid, 0);
  if (pidfd == SYSCALL_FAILED) {
    Util::logError("pidfd_open");
    fclose(maps_file);
    return false;
  }

  // Parse memory regions and batch the operations
  char line[256];
  unsigned long start, end;
  std::vector<struct iovec> iovecs;

  while (fgets(line, sizeof(line), maps_file) && swapHasFreeMB(1000)) {
    if (sscanf(line, "%lx-%lx", &start, &end) == 2) {
      struct iovec iov = {
          .iov_base = (void*)start,
          .iov_len = end - start,
      };
      iovecs.push_back(iov);
    }
    // Batch process if the vector size reaches a certain limit (e.g., 100
    // regions)
    if (iovecs.size() >= 100) {
      if (Oomd::process_madvise(
              pidfd, iovecs.data(), iovecs.size(), MADV_PAGEOUT, 0) == SYSCALL_FAILED) {
        Util::logError("process_madvise");
      } else {
        for (const auto& iov : iovecs) {
          OLOG << "Memory at " << iov.iov_base << " was paged out";
        }
      }
      iovecs.clear();
    }
  }

  // Process any remaining regions
  if (swapHasFreeMB(1000)) {
    if (!iovecs.empty()) {
      if (Oomd::process_madvise(
              pidfd, iovecs.data(), iovecs.size(), MADV_PAGEOUT, 0) == SYSCALL_FAILED) {
        Util::logError("process_madvise");
      } else {
        for (const auto& iov : iovecs) {
          OLOG << "Memory at " << iov.iov_base << " was paged out";
        }
      }
    }
  }

  close(pidfd);
  fclose(maps_file);
  return true;
}

void FreezePlugin::createFreezer(void) {
  // Create the cgroup directory
  if (mkdir(FREEZER_PATH, 0755) == SYSCALL_FAILED) {
    if (errno == EEXIST) {
      OLOG << "Freezer already exists!";
    } else {
      Util::logError("create freezer");
      exit(EXIT_FAILURE);
    }
  } else {
    OLOG << "Created freezer: " << FREEZER_PATH;
  }

  // Mount the cgroup filesystem
  if (mount("freezer", FREEZER_PATH, "cgroup", 0, "freezer") == SYSCALL_FAILED) {
    if (errno == EBUSY) {
      OLOG << "Freezer already mounted";
    } else {
      Util::logError("mount freezer");
      exit(EXIT_FAILURE);
    }
  } else {
    OLOG << "Mounted freezer";
  }
}
} // namespace Oomd