#include <string>
#include <vector>
#include "oomd/plugins/BaseKillPlugin.h"

#include <chrono>

namespace Oomd {

struct MemoryRegion {
  unsigned long start;
  unsigned long end;
  size_t swapSize;
};

class UnfreezePlugin : public BaseKillPlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static UnfreezePlugin* create() {
    return new UnfreezePlugin();
  }

  Engine::PluginRet run(OomdContext& ctx) override;

  int tryToKillPids(const std::vector<int>& procs) override;

  ~UnfreezePlugin() = default;

 protected:
  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) override;

  void ologKillTarget(
      OomdContext& ctx,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& peers) override;

  std::vector<MemoryRegion> getSwappedRegions(pid_t pid);

  void unfreezeProcess(int pid);

  void pageInMemory(int pid);
};

} // namespace Oomd