#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/include/Types.h"
#include "oomd/plugins/ThrottleCgroup.h"
#include "oomd/util/FreezeUtills.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/sysinfo.h>
#include <sys/types.h>
#include <unistd.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <algorithm>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <cerrno>
#include "../../../usr/include/x86_64-linux-gnu/sys/stat.h"

#define THROTTLE_FILE_PATH "/cpu.max"
#define THROTTLE_CGROUP "/sys/fs/cgroup/bench.slice/bench-nas.slice"
#define MONITOR_CGROUP "/sys/fs/cgroup/bench.slice"
#define MAX_LEVEL 4
#define PERIOD 100000
namespace Oomd {

REGISTER_PLUGIN(throttle, ThrottleCgroup::create);

int ThrottleCgroup::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  return BaseKillPlugin::init(args, context);
}

void ThrottleCgroup::increaseLevel() {
  changeLevel(+1);
}
void ThrottleCgroup::decreaseLevel() {
  changeLevel(-1);
}

void ThrottleCgroup::changeLevel(int change) {
  level = std::min(MAX_LEVEL, std::max(level + change, 0));
  const std::string quotaAndPeriod = getCurrentQuotaAndPeriod();
  throttle(THROTTLE_CGROUP, quotaAndPeriod);
}

std::string ThrottleCgroup::getCurrentQuotaAndPeriod() {
  int relativeQuota;

  switch (level) {
    case 1:
      relativeQuota = 0.75;
      break;
    case 2:
      relativeQuota = 0.5;
      break;
    case 3:
      relativeQuota = 0.25;
      break;
    case MAX_LEVEL:
      relativeQuota = 0.1;
      break;
    default:
      relativeQuota = 1;
      break;
  }

  std::ostringstream valuesAsString;
  valuesAsString << relativeQuota * PERIOD << " " << PERIOD;
  return valuesAsString.str();
}

Engine::PluginRet ThrottleCgroup::run(OomdContext& ctx) {
  struct sysinfo memInfo;
  sysinfo(&memInfo);
  for (const CgroupContext& cgroup_ctx : ctx.addToCacheAndGet(cgroups_)) {
    OLOG << "cgroup \"" << cgroup_ctx.cgroup().relativePath() << "\" "
         << "memory.current=" << cgroup_ctx.current_usage().value_or(0)
         << "memory.stat (anon)=" << cgroup_ctx.anon_usage().value_or(0);
  }

  auto monitoredCgroupDirFd = Fs::DirFd::open(MONITOR_CGROUP);

  auto totalMemory = Fs::readMemmaxAt(monitoredCgroupDirFd.value());

  auto usedMemory = Fs::readMemcurrentAt(monitoredCgroupDirFd.value());

  double usedMemoryPercentage = (double)usedMemory.value() / totalMemory.value() * 100.0;

  // if usage is high (low on memory)
  if (usedMemoryPercentage > 70.0) {
    increaseLevel();
    OLOG << "low on memory, new throttle level: " << level << ", pausing...";
    return Engine::PluginRet::ASYNC_PAUSED;
  }

  if (usedMemoryPercentage < 30) {
    decreaseLevel();
    OLOG << "memory is fine, new throttle level: " << level << ", pausing...";

    if (level == 0) {
      return Engine::PluginRet::CONTINUE;
    }
    return Engine::PluginRet::ASYNC_PAUSED;
  }
  return Engine::PluginRet::CONTINUE;
};

int ThrottleCgroup::tryToKillPids(const std::vector<int>& procs) {
  return 1;
}

void ThrottleCgroup::throttle(
    const std::string& cgroupPath,
    const std::string& quotaAndPeriod) {

  const std::string path = cgroupPath + THROTTLE_FILE_PATH;

  writeToFile(path, quotaAndPeriod);
}

std::vector<OomdContext::ConstCgroupContextRef> ThrottleCgroup::rankForKilling(
    OomdContext& ctx,
    const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {
  return OomdContext::sortDescWithKillPrefs(
      cgroups, [this](const CgroupContext& cgroup_ctx) {
        return cgroup_ctx.current_usage().value_or(0);
      });
}

void ThrottleCgroup::ologKillTarget(
    OomdContext& ctx,
    const CgroupContext& target,
    const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */) {
  OLOG << "Prefeteched memory and unfreezed \""
       << target.cgroup().relativePath();
}

} // namespace Oomd
