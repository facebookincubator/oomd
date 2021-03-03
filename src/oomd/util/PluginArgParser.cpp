#include <string>

#include "oomd/include/Types.h"
#include "oomd/util/PluginArgParser.h"

namespace Oomd {

std::unordered_set<CgroupPath> PluginArgParser::parseCgroup(
    const PluginConstructionContext& context,
    const std::string& cgroupStr) {
  std::unordered_set<CgroupPath> res;
  const auto& cgroup_fs = context.cgroupFs();
  auto cgroups = Util::split(cgroupStr, ',');
  for (const auto& c : cgroups) {
    res.emplace(cgroup_fs, c);
  }

  return res;
}

int PluginArgParser::parseUnsignedInt(const std::string& intStr) {
  int res = std::stoi(intStr);
  if (res < 0) {
    throw std::invalid_argument("must be non-negative");
  }
  return res;
}

void PluginArgParser::setName(const std::string& pluginName) {
  pluginName_ = pluginName;
}

std::string PluginArgParser::getName() {
  return pluginName_;
}

SystemMaybe<Unit> PluginArgParser::parse(const Engine::PluginArgs& args) {
  for (const auto& argName : requiredArgs_) {
    if (args.find(argName) == args.end()) {
      OLOG << "Required arg \"" << argName << "\" missing in plugin \""
           << pluginName_ << "\"";
      return SYSTEM_ERROR(
          EINVAL,
          "Required arg \"",
          argName,
          "\" missing in plugin \"",
          pluginName_,
          "\"");
    }
  }
  for (const auto& entry : args) {
    if (argValueFillingFuncs_.find(entry.first) !=
        argValueFillingFuncs_.end()) {
      auto funcRes = argValueFillingFuncs_.at(entry.first)(entry.second);
      if (!funcRes) {
        return SYSTEM_ERROR(
            EINVAL,
            "Failed parsing arg for plugin \"",
            pluginName_,
            "\", error: ",
            funcRes.error().what());
      }
    } else {
      OLOG << "Unknown arg \"" << entry.first << "\" in plugin \""
           << pluginName_ << "\"";
      return SYSTEM_ERROR(
          EINVAL,
          "Unknown arg \"",
          entry.first,
          "\" in plugin \"",
          pluginName_,
          "\"");
    }
  }

  return noSystemError();
}

std::unordered_set<std::string> PluginArgParser::validArgNames() {
  std::unordered_set<std::string> res;
  for (const auto& entry : argValueFillingFuncs_) {
    res.emplace(entry.first);
  }
  return res;
};

template <>
int64_t PluginArgParser::parseValue(const std::string& valueString) {
  return std::stoull(valueString);
}

template <>
int PluginArgParser::parseValue(const std::string& valueString) {
  return std::stoi(valueString);
}

template <>
double PluginArgParser::parseValue(const std::string& valueString) {
  return std::stod(valueString);
}

template <>
float PluginArgParser::parseValue(const std::string& valueString) {
  return std::stof(valueString);
}

template <>
bool PluginArgParser::parseValue(const std::string& valueString) {
  if (valueString == "true" || valueString == "True" || valueString == "1") {
    return true;
  }

  if (valueString == "false" || valueString == "False" || valueString == "0") {
    return false;
  }

  throw std::invalid_argument(
      "Invalid resource value, must be true/false, True/False, 1/0.");
}

template <>
std::chrono::milliseconds PluginArgParser::parseValue(
    const std::string& valueString) {
  return std::chrono::milliseconds(std::stoll(valueString));
}

template <>
ResourceType PluginArgParser::parseValue(const std::string& valueString) {
  if (valueString == "io") {
    return ResourceType::IO;
  } else if (valueString == "memory") {
    return ResourceType::MEMORY;
  }
  throw std::invalid_argument(
      "Invalid resource value, must be either 'io' or 'memory'.");
}

} // namespace Oomd
