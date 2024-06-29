#include "oomd/plugins/FreezePlugin.h"
#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/include/Types.h"
#include "oomd/util/Util.h"

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <cerrno>
#include "../../../usr/include/x86_64-linux-gnu/sys/stat.h"

#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"

void writeToFile(const std::string& path, const std::string& value);
void handleProcess(int pid);
bool createFreezeCgroup();
void freezeProcess(int pid);
bool pageOutMemory(int pid);
int getFirstPidInCgroup(const std::string& path);

namespace Oomd {

REGISTER_PLUGIN(freeze, FreezePlugin<>::create);

// Wrapper for pidfd_open syscall
int pidfd_open(pid_t pid, unsigned int flags) {
  return syscall(SYS_pidfd_open, pid, flags);
}

// Wrapper function for process_madvise
long process_madvise(
    pid_t pid,
    const struct iovec* vec,
    size_t vlen,
    int advice,
    unsigned long flags) {
  int pidfd = pidfd_open(pid, 0);
  if (pidfd == -1) {
    perror("pidfd_open");
    return EXIT_FAILURE;
  }

  int result = syscall(SYS_process_madvise, pidfd, vec, vlen, advice, flags);

  if(result == -1) {
    close(pidfd);
    return -1;
  }

  return pidfd;
}
template <typename Base>
int FreezePlugin<Base>::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  return BaseKillPlugin::init(args, context);
}

template <typename Base>
int FreezePlugin<Base>::tryToKillPids(const std::vector<int>& procs) {
for(auto pid : procs) {
      handleProcess(pid);
}
  return 0;
}

template <typename Base>
std::vector<OomdContext::ConstCgroupContextRef> FreezePlugin<Base>::rankForKilling(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {
  return OomdContext::sortDescWithKillPrefs(
      cgroups,
      [this](const CgroupContext& cgroup_ctx) {
          return cgroup_ctx.current_usage().value_or(0);
      });
}

template <typename Base>
void FreezePlugin<Base>::ologKillTarget(
    OomdContext& ctx,
    const CgroupContext& target,
    const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */) {
    OLOG << "Nitzan and Guy freezed \"" << target.cgroup().relativePath() << "\" ("
         << target.current_usage().value_or(0) / 1024 / 1024
         << "MB) based on swap usage at "
         << target.swap_usage().value_or(0) / 1024 / 1024 << "MB;";
}

} // namespace Oomd

void writeToFile(const std::string& path, const std::string& value) {
  std::ofstream file(path);
  if (!file.is_open()) {
    OLOG << "Error opening file: " << path << " - " << strerror(errno);
    throw std::runtime_error("Failed to open file: " + path);
  }
  file << value;
  if (file.fail()) {
    OLOG << "Error writing to file: " << path << " - " << strerror(errno);
    throw std::runtime_error("Failed to write to file: " + path);
  }
  file.close();
  OLOG << "Successfully wrote to file: " << path;
}

void handleProcess(int pid) {
  if (!createFreezeCgroup()) {
    OLOG << "Failed to create freeze cgroup";
    return;
  }
  freezeProcess(pid);
  
  if(!pageOutMemory(pid)) {
    OLOG << "Failed to page out memory";
    return;
  }
}

bool createFreezeCgroup() {
  // Create the cgroup directory
  if (mkdir(CGROUP_PATH, 0755) && errno != EEXIST) {
    OLOG << "Error creating cgroup directory: " << std::strerror(errno);
    return false;
  }
  OLOG << "Successfully created or found existing cgroup directory: "
       << CGROUP_PATH;
  return true;
}

void freezeProcess(int pid) {
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

int getFirstPidInCgroup(const std::string& cgroupPath) {
  std::string tasksFile = cgroupPath + "/cgroup.procs";
  std::ifstream file(tasksFile);

  if (!file.is_open()) {
    OLOG << "Error: Could not open file " << tasksFile << " - "
         << strerror(errno);
    return -1;
  }

  int pid;
  file >> pid;

  if (file.fail()) {
    OLOG << "Error: Could not read PID from file " << tasksFile;
    return -1;
  }
  OLOG << "Read PID: " << pid << " from file: " << tasksFile;
  return pid;
}

bool pageOutMemory(int pid) {
    // Open and read /proc/[pid]/maps
    char maps_path[256];
    snprintf(maps_path, sizeof(maps_path), "/proc/%d/maps", pid);
    FILE *maps_file = fopen(maps_path, "r");
    if (!maps_file) {
        perror("fopen");
        return false;
    }

    // Parse memory regions
    char line[256];
    unsigned long start, end;
    while (fgets(line, sizeof(line), maps_file)) {
        if (sscanf(line, "%lx-%lx", &start, &end) == 2) {
            struct iovec iov = {
                .iov_base = (void *)start,
                .iov_len = end - start,
            };
            int pidfd = Oomd::process_madvise(pid, &iov, 1, MADV_PAGEOUT, 0);
            if ( pidfd == -1) {
                perror("process_madvise");
            }
            else {
                OLOG << "Memory at " << iov.iov_base << " was paged out\n";
            }
                close(pidfd);
        }
    }
    fclose(maps_file);
    return true;
}