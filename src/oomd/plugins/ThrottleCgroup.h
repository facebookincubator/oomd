#include <string>
#include <vector>
#include "oomd/engine/BasePlugin.h"

#include <chrono>

namespace Oomd {

class ThrottleCgroup : public Engine::BasePlugin {
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

  ~ThrottleCgroup() = default;

 protected:
 int level = 0;

};

} // namespace Oomd