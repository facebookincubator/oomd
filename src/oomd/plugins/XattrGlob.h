/*
 * Copyright (C) 2026-present, Meta Platforms, Inc. and affiliates
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 */

#pragma once

#include <memory>
#include <string>
#include <unordered_set>

#include "oomd/engine/BasePlugin.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/plugins/XattrValueMatcher.h"

namespace Oomd {

class XattrGlob final : public Engine::BasePlugin {
 public:
  static XattrGlob* create();

  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;
  Engine::PluginRet run(OomdContext& context) override;

 private:
  std::unordered_set<CgroupPath> cgroups_;
  std::unordered_set<CgroupPath> ruleset_cgroups_;
  std::string xattr_;
  bool debug_{false};
  std::shared_ptr<const XattrValueMatcher> matcher_;
};

} // namespace Oomd
