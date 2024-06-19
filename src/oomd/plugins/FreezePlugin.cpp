#include "oomd/plugins/FreezePlugin.h"
#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/include/Types.h"
#include "oomd/util/Util.h"

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <unistd.h>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "../../../usr/include/x86_64-linux-gnu/sys/stat.h"
#include <cerrno>


#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"

namespace Oomd {

REGISTER_PLUGIN(freeze, FreezePlugin::create);

// Wrapper for pidfd_open syscall
int pidfd_open(pid_t pid, unsigned int flags) {
    return syscall(SYS_pidfd_open, pid, flags);
}

// Wrapper function for process_madvise
long process_madvise(pid_t pid, const struct iovec *vec, size_t vlen, int advice, unsigned long flags) {
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
  return 0;
}

Engine::PluginRet FreezePlugin::run(OomdContext& ctx) {
  const std::vector<OomdContext::ConstCgroupContextRef>& initialCgroups =
      ctx.addToCacheAndGet(cgroups_);

  auto it = std::find_if(
      initialCgroups.begin(),
      initialCgroups.end(),
      [](const OomdContext::ConstCgroupContextRef& cgroupCtx) {
        return cgroupCtx.get().cgroup().relativePath() == "testgroup";
      });

  if (it != initialCgroups.end()) {
    const OomdContext::ConstCgroupContextRef& testGroupCgroup = *it;
    
  } else {
    std::cerr << "Cgroup with name 'testgroup' not found." << std::endl;
  }

  return Engine::PluginRet::CONTINUE;
}

} // namespace Oomd

void write_to_file(const std::string& path, const std::string& value) {
  std::ofstream file(path);
  if (!file.is_open()) {
    std::cerr << "Error opening file: " << strerror(errno) << std::endl;
    throw std::runtime_error("Failed to open file");
  }
  file << value;
  if (file.fail()) {
    std::cerr << "Error writing to file: " << strerror(errno) << std::endl;
    throw std::runtime_error("Failed to write to file");
  }
  file.close();
}

void handle_process(int pid) {
  create_freeze_cgroup();
  freeze_process(pid);
}

bool create_freeze_cgroup() {
  // Create the cgroup directory
  if (mkdir(CGROUP_PATH, 0755) && errno != EEXIST) {
    std::cerr << "Error creating cgroup directory: " << std::strerror(errno)
              << std::endl;
    return false;
  }
  return true;
}

void freeze_process(int pid) {
  // Add the process to the cgroup
  char tasks_path[256];
  snprintf(tasks_path, sizeof(tasks_path), "%s/tasks", CGROUP_PATH);
  char pid_str[16];
  snprintf(pid_str, sizeof(pid_str), "%d", pid);
  write_to_file(tasks_path, pid_str);

  // Freeze the process
  char state_path[256];
  snprintf(state_path, sizeof(state_path), "%s/freezer.state", CGROUP_PATH);
  write_to_file(state_path, "FROZEN");
}

// bool page_out_memory(int pid) {
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
//                 std::cout << "Memory at " << iov.iov_base << " was paged out\n";
//             }
//         }
//     }
//     fclose(maps_file);
//     return true;
// }