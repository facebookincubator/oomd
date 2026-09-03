/*
 * Copyright (C) 2026-present, Meta Platforms, Inc. and affiliates
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 */

#include "oomd/plugins/XattrGlob.h"

#include <exception>
#include <utility>

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/PluginArgParser.h"

namespace Oomd {

XattrGlob* XattrGlob::create() {
  return new XattrGlob();
}

REGISTER_PLUGIN(xattr_glob, XattrGlob::create);

int XattrGlob::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  cgroups_.clear();
  ruleset_cgroups_.clear();
  xattr_.clear();
  debug_ = false;
  matcher_.reset();

  std::string allowlist;
  std::string denylist;
  PluginArgParser argParser(getName());
  argParser.addArgumentCustom(
      "cgroup", cgroups_, [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      });
  argParser.addArgumentCustom(
      "ruleset_cgroup",
      ruleset_cgroups_,
      [context](const std::string& cgroupStr) {
        return PluginArgParser::parseCgroup(context, cgroupStr);
      });
  argParser.addArgument("xattr", xattr_, true);
  argParser.addArgument("allowlist", allowlist, true);
  argParser.addArgument("denylist", denylist);
  argParser.addArgument("debug", debug_);

  if (!argParser.parse(args)) {
    return 1;
  }

  try {
    matcher_ =
        XattrValueMatcherProvider::compileCommaSeparated(allowlist, denylist);
  } catch (const std::exception& error) {
    OLOG << "Failed to compile xattr_glob patterns: " << error.what();
    return 1;
  }

  return 0;
}

Engine::PluginRet XattrGlob::run(OomdContext& context) {
  if (!matcher_) {
    return Engine::PluginRet::STOP;
  }

  bool allowed = false;
  auto cgroups = context.addToCacheAndGet(cgroups_, ruleset_cgroups_);
  if (cgroups.empty()) {
    if (debug_) {
      OLOG << "xattr_glob stopped because no target cgroups were selected";
    }
    return Engine::PluginRet::STOP;
  }

  for (const CgroupContext& cgroup : cgroups) {
    const auto decision = matcher_->evaluateCgroup(cgroup.fd(), xattr_);
    if (debug_) {
      OLOG << "xattr_glob checked " << cgroup.cgroup().relativePath()
           << " with reason " << xattrValueMatchReasonName(decision.reason);
    }
    if (decision.reason == XattrValueMatchReason::DENYLIST_MATCH ||
        decision.reason == XattrValueMatchReason::XATTR_MISSING ||
        decision.reason == XattrValueMatchReason::XATTR_READ_ERROR ||
        decision.reason == XattrValueMatchReason::XATTR_VALUE_INVALID) {
      return Engine::PluginRet::STOP;
    }
    if (decision.allowed()) {
      allowed = true;
    }
  }

  return allowed ? Engine::PluginRet::CONTINUE : Engine::PluginRet::STOP;
}

} // namespace Oomd
