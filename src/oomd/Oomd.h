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

#include <signal.h>
#include <chrono>
#include <memory>
#include <string>
#include <unordered_map>

#include "oomd/OomdContext.h"

namespace Oomd {

namespace Config2::IR {
struct Root;
}
namespace Engine {
class Engine;
}
class DropInServiceAdaptor;

class Oomd {
 public:
  Oomd(
      std::unique_ptr<Config2::IR::Root> ir_root,
      std::unique_ptr<Engine::Engine> engine,
      int interval,
      const std::string& cgroup_fs,
      const std::string& drop_in_dir,
      const std::unordered_map<std::string, DeviceType>& io_devs = {},
      const IOCostCoeffs& hdd_coeffs = {},
      const IOCostCoeffs& ssd_coeffs = {});
  ~Oomd();

  void updateContext();
  int run(const sigset_t* mask);

 private:
  // runtime settings
  std::chrono::seconds interval_{0};
  std::unique_ptr<Config2::IR::Root> ir_root_;
  std::unique_ptr<Engine::Engine> engine_;
  std::unique_ptr<DropInServiceAdaptor> fs_drop_in_service_;

  OomdContext ctx_;
};

} // namespace Oomd
