#include <string>
#include <vector>
#include "oomd/plugins/BaseKillPlugin.h"

#include <chrono>

namespace Oomd {

class ThrottleCgroup : public BaseKillPlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static ThrottleCgroup* create() {
    return new ThrottleCgroup();
  }

  void decreaseLevel();

  void increaseLevel();

  void changeLevel(int change);

  void throttle(
    const std::string& cgroupPath,
    const std::string& quotaAndPeriod);

  std::string getCurrentQuotaAndPeriod();

  Engine::PluginRet run(OomdContext& ctx) override;

  int tryToKillPids(const std::vector<int>& procs) override;

  ~ThrottleCgroup() = default;

 protected:
 int level = 0;
 
  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) override;

  void ologKillTarget(
      OomdContext& ctx,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& peers) override;
};

} // namespace Oomd