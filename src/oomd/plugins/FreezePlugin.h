#pragma once

#include <string>
#include <vector>
#include "oomd/engine/BasePlugin.h"

#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"

using std::string;

void write_to_file(const std::string& path, const std::string& value);
void handle_process(int pid);
bool create_freeze_cgroup();
void freeze_process(int pid);
bool page_out_memory(int pid);

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