#include "oomd/PluginConstructionContext.h"

namespace Oomd {

PluginConstructionContext::PluginConstructionContext(
    const std::string& cgroup_fs)
    : cgroup_fs_(cgroup_fs) {}

const std::string& PluginConstructionContext::cgroupFs() const {
  return cgroup_fs_;
}

} // namespace Oomd
