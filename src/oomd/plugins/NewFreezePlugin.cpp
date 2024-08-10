#include "oomd/plugins/NewFreezePlugin.h"
#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

#include "oomd/include/Types.h"
#include "oomd/util/FreezeUtills.h"

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/sysinfo.h>
#include <unistd.h>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <algorithm>
#include <cstring>
#include <cerrno>

namespace Oomd {

REGISTER_PLUGIN(new_freeze, NewFreezePlugin::create);

int NewFreezePlugin::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  argParser_.addArgumentCustom(
      "cgroup",
      cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      },
      true);
        if (!argParser_.parse(args)) {
    return 1;
  }
  return 0;
}

void NewFreezePlugin::freezeCgroup(const std::string& cgroupPath) {
  std::string freezeFilePath = cgroupPath + "/cgroup.freeze";
  std::ofstream freezeFile(freezeFilePath);
  
  if (!freezeFile.is_open()) {
    logError("Error opening freeze file: " + freezeFilePath);
    return;
  }

  freezeFile << "1";
  freezeFile.close();

  OLOG << "Frozen cgroup: " << cgroupPath;
}


void NewFreezePlugin::memoryReclaimCgroup(const std::string& pathToCgroup) {
  auto cgroupDirFd = Fs::DirFd::open(pathToCgroup);
  if (!cgroupDirFd) {
    logError("open " + pathToCgroup);
    return;
  }

  auto toReclaim = Fs::readMemcurrentAt(cgroupDirFd.value());
  if (!toReclaim) {
    logError("read " + pathToCgroup + "/memory.current");
    return;
  }

  auto result = Fs::writeMemReclaimAt(cgroupDirFd.value(), toReclaim.value(), std::nullopt);
  if (!result) {
    logError("Failed to write memory reclaim at " + pathToCgroup);
  }
  OLOG << "Reclaimed memory at: " << pathToCgroup;
}


void NewFreezePlugin::processCgroup(const std::string& cgroupPath) {
  freezeCgroup(cgroupPath);
  memoryReclaimCgroup(cgroupPath);
}

Engine::PluginRet NewFreezePlugin::run(OomdContext& ctx) {
  for (const auto& cgroupPath : cgroups_) {
    std::string path = cgroupPath.absolutePath();

    // Create a thread for each cgroup path and detach it
    std::thread([this, path]() {
      processCgroup(path);
    }).detach();  // Detach the thread so it runs independently
  }

  return Engine::PluginRet::CONTINUE;
}

} // namespace Oomd
