#pragma once

#include <string>

#include "oomd/engine/BasePlugin.h"

using std::string;

namespace Oomd {

class ChangeMadviseToDontNeed : public Engine::BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static ChangeMadviseToDontNeed* create() {
    return new ChangeMadviseToDontNeed();
  }

  ~ChangeMadviseToDontNeed();

 private:
  volatile int* indicator_;
  int shm_fd_;
};

} // namespace Oomd