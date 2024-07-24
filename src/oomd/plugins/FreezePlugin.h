#pragma once

#include <string>
#include <vector>
#include "oomd/plugins/BaseKillPlugin.h"

#define VSYSCALL_START 0xffffffffff600000
#define VSYSCALL_END 0xffffffffff601000
#define RECLAIM "1"

#include <chrono>

namespace Oomd {


class FreezePlugin : public BaseKillPlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static FreezePlugin* create() {
    return new FreezePlugin();
  }

  int tryToKillPids(const std::vector<int>& procs) override;

  ~FreezePlugin() = default;

 protected:
  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) override;

  void ologKillTarget(
      OomdContext& ctx,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& peers) override;

 private:
  void handleProcess(int pid);
  bool createFreezeCgroup(void);
  void freezeProcess(int pid);
  bool pageOutMemory(int pid);
  void createFreezer(void);
  bool swapHasFreeMB(int megabyte);
  void memoryReclaimCgroup(std::string& pathToCgroup);
};

} // namespace Oomd