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
void FreezeProcess(int pid);
bool pageOutMemory(int pid);
int getFirstPidInCgroup(const std::string& path);

namespace Oomd {

REGISTER_PLUGIN(freeze, FreezePlugin::create);

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

  return syscall(SYS_process_madvise, pidfd, vec, vlen, advice, flags);
}

int FreezePlugin::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  argParser_.addArgumentCustom(
      "cgroup",
      cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      },
      true);
  OLOG << "got here 1";
  return 0;
}

Engine::PluginRet FreezePlugin::run(OomdContext& ctx) {
  // auto it = std::find_if(
  //     initialCgroups.begin(),
  //     initialCgroups.end(),
  //     [](const OomdContext::ConstCgroupContextRef& cgroupCtx) {
  //       return cgroupCtx.get().cgroup().relativePath() == "testgroup";
  //     });

  // if (it != initialCgroups.end()) {
  //   const OomdContext::ConstCgroupContextRef& testGroupCgroup = *it;
  //   const int selectedPid =
  //   getFirstPidInCgroup(testGroupCgroup.get().cgroup().absolutePath());
  //   FreezeProcess(selectedPid);
  // } else {
  //   std::cerr << "Cgroup with name 'testgroup' not found." << std::endl;
  // }
  try {
    const std::vector<OomdContext::ConstCgroupContextRef>& initialCgroups =
        ctx.addToCacheAndGet(cgroups_);
    OLOG << "got here 2";
    OLOG << cgroups_.size();
    // if (initialCgroups.empty()) {
    //   OLOG << "No cgroups found";
    //   return Engine::PluginRet::CONTINUE;
    // }
    // const OomdContext::ConstCgroupContextRef& testGroupCgroup =
    // initialCgroups[0];

      const std::string cgroupPath = "/sys/fs/cgroup/test";
      const int selectedPid = getFirstPidInCgroup(cgroupPath);

      if (selectedPid == -1) {
        OLOG << "No PID found in cgroup: " << cgroupPath;
        return Engine::PluginRet::CONTINUE;
      }

      OLOG << "got here 3, PID: " << selectedPid;
      handleProcess(selectedPid);
      OLOG << "got here 4";
  } catch (const std::exception& e) {
    OLOG << "Exception in FreezePlugin::run: " << e.what();
    return Engine::PluginRet::STOP;
  }

  return Engine::PluginRet::CONTINUE;
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
  FreezeProcess(pid);
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

void FreezeProcess(int pid) {
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

// bool pageOutMemory(int pid) {
//     // Open and read /proc/[pid]/maps
//     char maps_path[256];
//     snprintf(maps_path, sizeof(maps_path), "/proc/%d/maps", pid);
//     FILE *maps_file = fopen(maps_path, "r");
//     if (!maps_file) {
//         perror("fopen");
//         return false;
//     }

//     // Parse memory regions
//     char line[256];
//     unsigned long start, end;
//     while (fgets(line, sizeof(line), maps_file)) {
//         if (sscanf(line, "%lx-%lx", &start, &end) == 2) {
//             struct iovec iov = {
//                 .iov_base = (void *)start,
//                 .iov_len = end - start,
//             };
//             if (process_madvise(pid, &iov, 1, MADV_PAGEOUT, 0) == -1) {
//                 perror("process_madvise");
//             }
//             else {
//                 std::cout << "Memory at " << iov.iov_base << " was paged
//                 out\n";
//             }
//         }
//     }
//     fclose(maps_file);
//     return true;
// }