#pragma once

#include <string>
#include <vector>
#include "oomd/engine/BasePlugin.h"

#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"

using std::string;

namespace Oomd {

class FreezePlugin : public Engine::BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static FreezePlugin* create() {
    return new FreezePlugin();
  }

  ~FreezePlugin() = default;

 private:
  std::unordered_set<CgroupPath> cgroups_;
};

} // namespace Oomd