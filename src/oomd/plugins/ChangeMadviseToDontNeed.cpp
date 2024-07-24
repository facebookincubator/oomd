#include "oomd/plugins/ChangeMadviseToDontNeed.h"

#include <fstream>
#include <iostream>
#include <unordered_map>
#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/FreezeUtills.h"
#include "oomd/util/Util.h"
#include <fcntl.h>  // for open
#include <sys/file.h>  // for flock, LOCK_UN
#include <unistd.h>  // for close, usleep

namespace Oomd {

REGISTER_PLUGIN(change_madvise_to_dont_need, ChangeMadviseToDontNeed::create);

int ChangeMadviseToDontNeed::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  // argParser_.addArgument("message", message_, true);

  // if (!argParser_.parse(args)) {
  //   return 1;
  // }

  // Success
  return 0;
}

Engine::PluginRet ChangeMadviseToDontNeed::run(OomdContext& ctx) {
    const std::string indicatorFilePath = "/home/guyy/oomd/testFiles/indicator";
    int fd = open(indicatorFilePath.c_str(), O_WRONLY);
    if (fd == -1) {
        logError("Error opening indicator file");
        return Engine::PluginRet::STOP;
    }

    // Busy wait until the exclusive lock can be acquired
    while (flock(fd, LOCK_EX) == -1) {
        usleep(1000); // Sleep for 1ms before trying again
    }

    // Use std::ofstream to write to the file
    std::ofstream indicatorFile(indicatorFilePath);
    if (!indicatorFile) {
        logError("Error opening indicator file for writing: ");
        close(fd);
        return Engine::PluginRet::STOP;
    }

    indicatorFile << "1" << std::endl;
    OLOG << "Wrote 1 to indicator file";
    if (indicatorFile.fail()) {
        logError("Error writing to indicator file: " + indicatorFilePath);
    } else {
        OLOG << "Successfully changed the indicator to 1.";
    }

    indicatorFile.close();

    // Release the lock and close the file descriptor
    while (flock(fd, LOCK_UN) == -1) {
        std::cerr << "Could not unlock indicator file, retrying..." << std::endl;
        usleep(1000); // Sleep for 1ms before trying again
    }

    close(fd);
  return Engine::PluginRet::CONTINUE;
}
} // namespace Oomd