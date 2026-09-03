/*
 * Copyright (C) 2026-present, Meta Platforms, Inc. and affiliates
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 */

#pragma once

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "oomd/util/Fs.h"
#include "oomd/util/SystemMaybe.h"

namespace Oomd {

enum class XattrValueMatchReason {
  ALLOW,
  DENYLIST_MATCH,
  NOT_ALLOWLISTED,
  XATTR_MISSING,
  XATTR_READ_ERROR,
  XATTR_VALUE_INVALID,
};

struct XattrValueMatchDecision {
  XattrValueMatchReason reason{XattrValueMatchReason::XATTR_READ_ERROR};
  std::optional<size_t> pattern_index;

  bool allowed() const {
    return reason == XattrValueMatchReason::ALLOW;
  }
};

std::string_view xattrValueMatchReasonName(XattrValueMatchReason reason);

/*
 * Immutable policy over one xattr value. Patterns are bounded globs. The
 * matcher uses fnmatch over the whole xattr value. Deny wins over allow, and an
 * empty allowlist denies every value.
 */
class XattrValueMatcher final {
 public:
  static SystemMaybe<std::optional<std::string>> readValueAt(
      const Fs::DirFd& cgroup,
      std::string_view xattr);

  XattrValueMatchDecision evaluateValue(const std::string& value) const;
  XattrValueMatchDecision evaluateCgroup(
      const Fs::DirFd& cgroup,
      std::string_view xattr) const;

 private:
  struct Impl;
  explicit XattrValueMatcher(std::shared_ptr<const Impl> impl);

  std::shared_ptr<const Impl> impl_;

  friend class XattrValueMatcherProvider;
};

// Pure factory for immutable compiled policies. A live provider can atomically
// publish and share one returned snapshot without coupling this layer to its
// configuration source.
class XattrValueMatcherProvider final {
 public:
  static std::shared_ptr<const XattrValueMatcher> compile(
      const std::vector<std::string>& allowlist,
      const std::vector<std::string>& denylist);
  static std::shared_ptr<const XattrValueMatcher> compileCommaSeparated(
      const std::string& allowlist,
      const std::string& denylist);
};

} // namespace Oomd
