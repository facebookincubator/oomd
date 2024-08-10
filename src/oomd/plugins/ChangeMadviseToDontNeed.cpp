#include "oomd/plugins/ChangeMadviseToDontNeed.h"

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

static const char* SHM_NAME = "/indicator_shm";
static const size_t SHM_SIZE = sizeof(int);

REGISTER_PLUGIN(change_madvise_to_dont_need, ChangeMadviseToDontNeed::create);

int ChangeMadviseToDontNeed::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  // Create shared memory object
  shm_fd_ = shm_open(SHM_NAME, O_CREAT | O_RDWR, 0666);
  if (shm_fd_ == -1) {
    logError("Error creating shared memory");
    return 1;
  }

  // Configure the size of the shared memory object
  if (ftruncate(shm_fd_, SHM_SIZE) == -1) {
    logError("Error configuring the size of shared memory");
    return 1;
  }

  // Map the shared memory object in memory
  indicator_ = static_cast<int*>(
      mmap(0, SHM_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd_, 0));
  if (indicator_ == MAP_FAILED) {
    logError("Error mapping shared memory");
    return 1;
  }

  // Initialize the indicator value
  *indicator_ = 0;

  // Success
  return 0;
}

Engine::PluginRet ChangeMadviseToDontNeed::run(OomdContext& ctx) {
  // Write the indicator value
  *indicator_ = 1;
  OLOG << "Successfully changed the indicator to 1.";
  return Engine::PluginRet::CONTINUE;
}

ChangeMadviseToDontNeed::~ChangeMadviseToDontNeed() {
  // Unmap and close the shared memory object
  if (indicator_ != MAP_FAILED) {
    munmap((void*)indicator_, SHM_SIZE);
  }
  if (shm_fd_ != -1) {
    close(shm_fd_);
  }
}
} // namespace Oomd