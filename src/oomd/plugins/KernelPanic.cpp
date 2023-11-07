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

#include "oomd/plugins/KernelPanic.h"

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

namespace Oomd {

REGISTER_PLUGIN(kernel_panic, KernelPanic::create);

int KernelPanic::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& /* unused */) {
  return argParser_.parse(args) ? 0 : 1;
}

Engine::PluginRet KernelPanic::run(OomdContext& /* unused */) {
  const auto fd = Fs::Fd::open("/proc/sysrq-trigger", false);
  if (!fd) {
    OLOG << "Failed to open /proc/sysrq-trigger";
    return Engine::PluginRet::CONTINUE;
  }
  if (Util::writeFull(fd->fd(), "c", 1) != 1) {
    OLOG << "Failed to trigger kernel panic";
    return Engine::PluginRet::CONTINUE;
  }
  return Engine::PluginRet::STOP;
}

} // namespace Oomd
