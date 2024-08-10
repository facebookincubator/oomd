#include "oomd/plugins/FreezePlugin.h"
#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/FreezeUtills.h"
#include "oomd/util/Util.h"

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/sysinfo.h>
#include <sys/types.h>
#include <unistd.h>
#include <cerrno>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stdexcept>

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
    logError("pidfd_open");
    return SYSCALL_FAILED;
  }

  if (syscall(SYS_process_madvise, pidfd, vec, vlen, advice, flags) ==
      SYSCALL_FAILED) {
    close(pidfd);
    return SYSCALL_FAILED;
  }

  return pidfd;
}

int FreezePlugin::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  createFreezer();
  createFreezeCgroup();
  return BaseKillPlugin::init(args, context);
}

int FreezePlugin::tryToKillPids(const std::vector<int>& procs) {
  for (auto pid : procs) {
    handleProcess(pid);
  }
  std::string pathToCgroup = "/sys/fs/cgroup/test";
  
  memoryReclaimCgroup(pathToCgroup);
  return 0;
}

std::vector<OomdContext::ConstCgroupContextRef> FreezePlugin::rankForKilling(
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
  OLOG << "Freezed \"" << target.cgroup().relativePath() << "\" ("
       << target.current_usage().value_or(0) / 1024 / 1024
       << "MB) based on swap usage at "
       << target.swap_usage().value_or(0) / 1024 / 1024 << "MB;";
}

void FreezePlugin::handleProcess(int pid) {
  freezeProcess(pid);

  // if (!pageOutMemory(pid)) {
  //   OLOG << "Failed to page out memory";
  //   return;
  // }
}

bool FreezePlugin::createFreezeCgroup(void) {
  // Create the cgroup directory
  if (mkdir(MY_FREEZER_PATH, 0755) && errno != EEXIST) {
    logError("create cgroup directory");
    return false;
  }
  OLOG << "Successfully created or found existing cgroup directory: "
       << MY_FREEZER_PATH;
  return true;
}

void FreezePlugin::memoryReclaimCgroup(std::string& pathToCgroup) {
  // writeToFile(pathToCgroup + "/memory.reclaim", RECLAIM);
  auto cgroupDirFd = Fs::DirFd::open(pathToCgroup);
  if(!cgroupDirFd) {
    logError("open " + pathToCgroup);
  }

  auto toReclaim = Fs::readMemcurrentAt(cgroupDirFd.value());

  if(!cgroupDirFd) {
    logError("read " + pathToCgroup + "/memory.current");
  }

  Fs::writeMemReclaimAt(cgroupDirFd.value() ,toReclaim.value(), std::nullopt);
}

void FreezePlugin::freezeProcess(int pid) {
  // Add the process to the cgroup
  if (pid <= 0) {
    OLOG << "Invalid PID: " << pid;
    return;
  }
  char tasks_path[256];
  snprintf(tasks_path, sizeof(tasks_path), "%s/tasks", MY_FREEZER_PATH);
  char pid_str[16];
  snprintf(pid_str, sizeof(pid_str), "%d", pid);
  writeToFile(tasks_path, pid_str);

  // Freeze the process
  char state_path[256];
  snprintf(state_path, sizeof(state_path), "%s/freezer.state", MY_FREEZER_PATH);
  writeToFile(state_path, "FROZEN");
  OLOG << "process: " << pid << "is now frozen!";
}

bool FreezePlugin::pageOutMemory(int pid) {
  try {
    std::ostringstream maps_path;
    maps_path << "/proc/" << pid << "/maps";

    std::ifstream maps_file(maps_path.str());

    if (!maps_file.is_open()) {
      logError("Failed to open file: " + maps_path.str());
      return false;
    }

    int pidfd = pidfd_open(pid, 0);
    if (pidfd == SYSCALL_FAILED) {
      logError("pidfd_open");
      return false;
    }

    std::vector<struct iovec> iovecs;
    std::string line;
    unsigned long start, end;

    while (std::getline(maps_file, line) && swapHasFreeMB(10000)) {
      if (std::sscanf(line.c_str(), "%lx-%lx", &start, &end) == 2) {
        if (start >= VSYSCALL_START && end <= VSYSCALL_END) {
          continue;
        }
        struct iovec iov = {
            .iov_base = reinterpret_cast<void*>(start),
            .iov_len = end - start,
        };
        iovecs.push_back(iov);
      }

      if (iovecs.size() >= 100) {
        if (Oomd::process_madvise(
                pidfd, iovecs.data(), iovecs.size(), MADV_PAGEOUT, 0) ==
            SYSCALL_FAILED) {
          logError("process_madvise");
        } else {
          // for (const auto& iov : iovecs) {
          //    OLOG << "Memory at " << iov.iov_base << " was paged out";
          // }
          iovecs.clear();
        }
      }
    }

    if (swapHasFreeMB(10000) && !iovecs.empty()) {
      if (Oomd::process_madvise(
              pidfd, iovecs.data(), iovecs.size(), MADV_PAGEOUT, 0) ==
          SYSCALL_FAILED) {
        logError("process_madvise");
      } 
      // else {
      //   for (const auto& iov : iovecs) {
      //     OLOG << "Memory at " << iov.iov_base << " was paged out";
      //   }
      // }
    }

    close(pidfd);
    return true;

  } catch (const std::system_error& e) {
    logError(e.what());
    return false;
  }
}

bool FreezePlugin::swapHasFreeMB(int megabyte) {
  struct sysinfo info;

  // Get system information
  if (sysinfo(&info) != 0) {
    logError("sysinfo");
    return false;
  }

  // Calculate free swap in kilobytes
  unsigned long swapFreeByte = (info.freeswap * info.mem_unit);

  // Check if the free swap space is less than the required amount
  if (swapFreeByte < static_cast<unsigned long>(megabyte) * 1024 * 1024) {
    logError("Not enough free swap space");
    return false;
  }

  return true;
}

void FreezePlugin::createFreezer(void) {
  // Create the cgroup directory
  if (mkdir(FREEZER_PATH, 0755) == SYSCALL_FAILED) {
    if (errno == EEXIST) {
      OLOG << "Freezer already exists!";
    } else {
      logError("create freezer");
      exit(EXIT_FAILURE);
    }
  } else {
    OLOG << "Created freezer: " << FREEZER_PATH;
  }

  // Mount the cgroup filesystem
  if (mount("freezer", FREEZER_PATH, "cgroup", 0, "freezer") ==
      SYSCALL_FAILED) {
    if (errno == EBUSY) {
      OLOG << "Freezer already mounted";
    } else {
      logError("mount freezer");
      exit(EXIT_FAILURE);
    }
  } else {
    OLOG << "Mounted freezer";
  }
}
} // namespace Oomd