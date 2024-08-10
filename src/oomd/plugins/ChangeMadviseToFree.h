#pragma once

#include <string>

#include "oomd/engine/BasePlugin.h"

using std::string;

namespace Oomd {

class ChangeMadviseToFree : public Engine::BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static ChangeMadviseToFree* create() {
    return new ChangeMadviseToFree();
  }

  ~ChangeMadviseToFree();

 private:
  int* indicator_;
  int shm_fd_;
};

} // namespace Oomd