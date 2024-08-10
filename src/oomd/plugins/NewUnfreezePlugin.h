#include <string>
#include <vector>
#include <unordered_set>
#include "oomd/plugins/BasePlugin.h"

#include <chrono>

namespace Oomd {

struct MemoryRegion {
  unsigned long start;
  unsigned long end;
  size_t swapSize;
};

class NewUnfreezePlugin : public BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static NewUnfreezePlugin* create() {
    return new NewUnfreezePlugin();
  }

  Engine::PluginRet run(OomdContext& ctx) override;

  ~NewUnfreezePlugin() = default;

private:
  std::unordered_set<CgroupPath> cgroups_;
  std::vector<MemoryRegion> getSwappedRegions(pid_t pid);
  void processCgroup(const std::string& cgroupPath);
  void unfreezeCgroup(int pid);
  void pageInMemory(int pid);
};

} // namespace Oomd