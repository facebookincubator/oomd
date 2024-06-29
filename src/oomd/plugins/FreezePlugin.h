#pragma once

#include <string>
#include <vector>
#include "oomd/plugins/BaseKillPlugin.h"

#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"

#include <chrono>

namespace Oomd {

template <typename Base = BaseKillPlugin>
class FreezePlugin : public Base {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static FreezePlugin* create() {
    return new FreezePlugin();
  }

  // Engine::PluginRet run(OomdContext& ctx) override;

  int tryToKillPids(const std::vector<int>& procs) override;

  ~FreezePlugin() = default;

 protected:
  std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
      OomdContext& ctx,
      const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) override;

  int64_t getSwapExcess(const CgroupContext& cgroup_ctx);

  void ologKillTarget(
      OomdContext& ctx,
      const CgroupContext& target,
      const std::vector<OomdContext::ConstCgroupContextRef>& peers) override;
};

} // namespace Oomd