/*
 * Copyright (C) 2026-present, Meta Platforms, Inc. and affiliates
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 */

#include "oomd/plugins/XattrValueMatcher.h"

#include <fnmatch.h>
#include <cerrno>
#include <stdexcept>
#include <utility>

namespace Oomd {
namespace {

constexpr size_t kMaxPatternCount = 256;
constexpr size_t kMaxPatternLength = 512;
constexpr size_t kMaxTotalPatternLength = 32 * 1024;
constexpr size_t kMaxPatternListLength = 64 * 1024;
constexpr size_t kMaxXattrNameLength = 255;

bool containsNul(std::string_view value) {
  return value.find('\0') != std::string_view::npos;
}

std::vector<std::string> splitPatternList(const std::string& patterns) {
  if (patterns.size() > kMaxPatternListLength) {
    throw std::invalid_argument("xattr value match pattern list is too large");
  }
  if (patterns.empty()) {
    return {};
  }

  std::vector<std::string> result;
  size_t begin = 0;
  while (begin <= patterns.size()) {
    const size_t separator = patterns.find(',', begin);
    const size_t end =
        separator == std::string::npos ? patterns.size() : separator;
    result.emplace_back(patterns.substr(begin, end - begin));
    if (separator == std::string::npos) {
      break;
    }
    begin = separator + 1;
  }
  return result;
}

} // namespace

struct XattrValueMatcher::Impl {
  explicit Impl(
      const std::vector<std::string>& allowPatterns,
      const std::vector<std::string>& denyPatterns) {
    if (allowPatterns.size() + denyPatterns.size() > kMaxPatternCount) {
      throw std::invalid_argument("too many xattr value match patterns");
    }

    size_t totalLength = 0;
    auto validate = [&](const std::vector<std::string>& patterns) {
      for (const auto& pattern : patterns) {
        totalLength += pattern.size();
        if (pattern.empty() || pattern.size() > kMaxPatternLength ||
            containsNul(pattern)) {
          throw std::invalid_argument("invalid xattr value match pattern");
        }
      }
    };
    validate(allowPatterns);
    validate(denyPatterns);
    if (totalLength > kMaxTotalPatternLength) {
      throw std::invalid_argument("xattr value match patterns are too large");
    }

    allowlist = allowPatterns;
    denylist = denyPatterns;
  }

  std::vector<std::string> allowlist;
  std::vector<std::string> denylist;
};

XattrValueMatcher::XattrValueMatcher(std::shared_ptr<const Impl> impl)
    : impl_(std::move(impl)) {}

std::string_view xattrValueMatchReasonName(XattrValueMatchReason reason) {
  switch (reason) {
    case XattrValueMatchReason::ALLOW:
      return "allow";
    case XattrValueMatchReason::DENYLIST_MATCH:
      return "denylist_match";
    case XattrValueMatchReason::NOT_ALLOWLISTED:
      return "not_allowlisted";
    case XattrValueMatchReason::XATTR_MISSING:
      return "xattr_missing";
    case XattrValueMatchReason::XATTR_READ_ERROR:
      return "xattr_read_error";
    case XattrValueMatchReason::XATTR_VALUE_INVALID:
      return "xattr_value_invalid";
  }
  return "unknown";
}

SystemMaybe<std::optional<std::string>> XattrValueMatcher::readValueAt(
    const Fs::DirFd& cgroup,
    std::string_view xattr) {
  if (xattr.empty() || xattr.size() > kMaxXattrNameLength ||
      containsNul(xattr)) {
    return SYSTEM_ERROR(EINVAL, "Invalid xattr name");
  }

  auto value = Fs::getxattrAt(cgroup, std::string(xattr));
  if (!value) {
    return SYSTEM_ERROR(value.error());
  }
  return std::move(*value);
}

XattrValueMatchDecision XattrValueMatcher::evaluateValue(
    const std::string& value) const {
  if (containsNul(value)) {
    return {XattrValueMatchReason::XATTR_VALUE_INVALID, std::nullopt};
  }

  for (size_t index = 0; index < impl_->denylist.size(); ++index) {
    if (::fnmatch(impl_->denylist[index].c_str(), value.c_str(), 0) == 0) {
      return {XattrValueMatchReason::DENYLIST_MATCH, index};
    }
  }
  for (size_t index = 0; index < impl_->allowlist.size(); ++index) {
    if (::fnmatch(impl_->allowlist[index].c_str(), value.c_str(), 0) == 0) {
      return {XattrValueMatchReason::ALLOW, index};
    }
  }
  return {XattrValueMatchReason::NOT_ALLOWLISTED, std::nullopt};
}

XattrValueMatchDecision XattrValueMatcher::evaluateCgroup(
    const Fs::DirFd& cgroup,
    std::string_view xattr) const {
  auto value = readValueAt(cgroup, xattr);
  if (!value) {
    return {XattrValueMatchReason::XATTR_READ_ERROR, std::nullopt};
  }
  if (!value->has_value()) {
    return {XattrValueMatchReason::XATTR_MISSING, std::nullopt};
  }
  return evaluateValue(**value);
}

std::shared_ptr<const XattrValueMatcher> XattrValueMatcherProvider::compile(
    const std::vector<std::string>& allowlist,
    const std::vector<std::string>& denylist) {
  return std::shared_ptr<const XattrValueMatcher>(new XattrValueMatcher(
      std::make_shared<const XattrValueMatcher::Impl>(allowlist, denylist)));
}

std::shared_ptr<const XattrValueMatcher>
XattrValueMatcherProvider::compileCommaSeparated(
    const std::string& allowlist,
    const std::string& denylist) {
  return compile(splitPatternList(allowlist), splitPatternList(denylist));
}

} // namespace Oomd
