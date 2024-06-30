#pragma once

#include <string>
#include <vector>
#include "oomd/plugins/BaseKillPlugin.h"

#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"

#include <chrono>

namespace Oomd {

template <typename Base = BaseKillPlugin>
class UnfreezePlugin : public Base {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static UnfreezePlugin* create() {
    return new UnfreezePlugin();
  }

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
};

} // namespace Oomd