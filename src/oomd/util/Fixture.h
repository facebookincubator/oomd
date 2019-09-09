/*
 * Copyright (C) 2018-present, Facebook, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#pragma once

#include <functional>
#include <initializer_list>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace Oomd {

// Fixture is a utility class for unit tests to encode their fixture files and
// materialize them in run time in some temporary directory.
class Fixture {
 public:
  typedef std::function<void(const std::string& path, const std::string& name)>
      materializeFunc;

  // Dirent to be materialized into a file or directory
  class DirEntry {
   public:
    explicit DirEntry(materializeFunc materialize)
        : materialize_(materialize) {}
    void materialize(const std::string& path, const std::string& name) const {
      materialize_(path, name);
    }

   private:
    const materializeFunc materialize_;
  };

  // Helper functions to create different named DirEntries so that we can
  // represent a directory tree with initializer_list in a single statement.
  typedef std::pair<const std::string, DirEntry> DirEntryPair;
  static DirEntryPair makeFile(
      const std::string& name,
      const std::string& content = "");
  static DirEntryPair makeDir(
      const std::string& name,
      std::unordered_map<std::string, DirEntry> entries = {});

  // utility functions that interact with file system
  static std::string mkdtempChecked();
  static void mkdirsChecked(
      const std::string& path,
      const std::string& prefix = "");
  static void writeChecked(const std::string& path, const std::string& content);
  static void rmrChecked(const std::string& path);
};
} // namespace Oomd
