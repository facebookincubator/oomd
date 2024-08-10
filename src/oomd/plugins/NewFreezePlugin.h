#pragma once

#include <string>
#include <unordered_set>
#include "oomd/plugins/BaseKillPlugin.h"
#include "oomd/include/CgroupPath.h"
#define RECLAIM "1"
#define FREEZE "1"

namespace Oomd {

class NewFreezePlugin : public Engine::BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static NewFreezePlugin* create() {
    return new NewFreezePlugin();
  }
Engine::PluginRet run(OomdContext& /* unused */) override;
  ~NewFreezePlugin() = default;

 private:
  std::unordered_set<CgroupPath> cgroups_;
  void freezeCgroup(const std::string& cgroupPath);
  void memoryReclaimCgroup(const std::string& cgroupPath);
  void processCgroup(const std::string& cgroupPath);
};

} // namespace Oomd
