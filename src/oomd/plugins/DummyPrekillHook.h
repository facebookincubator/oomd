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

#include "oomd/engine/PrekillHook.h"

namespace Oomd {

/*
 * A simple prekill hook useful for e2e testing and as a prekill hook
 * implementation template.
 */
class DummyPrekillHook : public Engine::PrekillHook {
 public:
  int init(
      const Engine::PluginArgs& args,
      const PluginConstructionContext& context) override;

  static DummyPrekillHook* create() {
    return new DummyPrekillHook();
  }

  virtual ~DummyPrekillHook() override = default;

  virtual std::unique_ptr<Engine::PrekillHookInvocation> fire(
      const CgroupContext& cgroup_ctx,
      const ActionContext& action_ctx) override;
};

class DummyPrekillHookInvocation : public Engine::PrekillHookInvocation {
 public:
  virtual bool didFinish() override;
  virtual ~DummyPrekillHookInvocation() override = default;
};

} // namespace Oomd
