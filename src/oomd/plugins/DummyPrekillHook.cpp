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

#include "oomd/plugins/DummyPrekillHook.h"

#include "oomd/Log.h"
#include "oomd/OomdContext.h"
#include "oomd/PluginRegistry.h"

namespace Oomd {

REGISTER_PREKILL_HOOK(dummy_prekill_hook, DummyPrekillHook::create);

int DummyPrekillHook::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {
  // parse any args you want to be configurable here
  return PrekillHook::init(args, context);
}

std::unique_ptr<Engine::PrekillHookInvocation> DummyPrekillHook::fire(
    const CgroupContext& cgroup_ctx,
    const ActionContext& /* unused */) {
  OLOG << "Prekill hook fired on " << cgroup_ctx.cgroup().relativePath();

  // this allocation is a waste, but it simplifies the prekill hook mechanism
  // and all real prekill hooks should need it.
  return std::unique_ptr<Engine::PrekillHookInvocation>(
      new DummyPrekillHookInvocation());
}

bool DummyPrekillHookInvocation::didFinish() {
  // always finish immediately
  return true;
}

} // namespace Oomd
