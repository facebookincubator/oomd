#include "oomd/plugins/ChangeMadviseToFree.h"

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/sysinfo.h>
#include <unistd.h>
#include <fstream>
#include <iostream>
#include <unordered_map>
#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/FreezeUtills.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(change_madvise_to_free, ChangeMadviseToFree::create);

static const char* SHM_NAME = "/indicator_shm";
static const size_t SHM_SIZE = sizeof(int);

int ChangeMadviseToFree::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  return 0;
}

Engine::PluginRet ChangeMadviseToFree::run(OomdContext& ctx) {
  struct sysinfo memInfo;
  sysinfo(&memInfo);

  long long totalMemory = memInfo.totalram;
  totalMemory *= memInfo.mem_unit;

  long long freeMemory = memInfo.freeram;
  freeMemory *= memInfo.mem_unit;

  double freeMemoryPercentage = (double)freeMemory / totalMemory * 100.0;

  if (freeMemoryPercentage < 30.0) {
    OLOG << "More than 70% memory is used, pausing...";
    return Engine::PluginRet::ASYNC_PAUSED;
  }

  // Open the shared memory object
  int shm_fd = shm_open(SHM_NAME, O_RDWR, 0666);
  if (shm_fd == -1) {
    logError("Error opening shared memory");
    return Engine::PluginRet::STOP;
  }

  // Map the shared memory object in memory
  int* indicator = static_cast<int*>(
      mmap(0, SHM_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0));
  if (indicator == MAP_FAILED) {
    logError("Error mapping shared memory");
    close(shm_fd);
    return Engine::PluginRet::STOP;
  }

  // Write the indicator value
  *indicator = 0;
  OLOG << "Successfully changed the indicator to 0.";

  if (msync(indicator, SHM_SIZE, MS_SYNC) == -1) {
    munmap(indicator, SHM_SIZE) logError("Error syncing shared memory");
    close(shm_fd)
  }
  // Unmap and close the shared memory object
  munmap(indicator, SHM_SIZE);
  close(shm_fd);

  return Engine::PluginRet::CONTINUE;
}
} // namespace Oomd