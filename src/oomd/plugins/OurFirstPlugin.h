#pragma once

#include <string>

#include "oomd/engine/BasePlugin.h"

using std::string;

namespace Oomd {

class OurFirstPlugin : public Engine::BasePlugin {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  Engine::PluginRet run(OomdContext& /* unused */) override;

  static OurFirstPlugin* create() {
    return new OurFirstPlugin();
  }

  ~OurFirstPlugin() = default;

 private:
  string message_;
};

} // namespace Oomd