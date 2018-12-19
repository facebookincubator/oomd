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

#include "oomd/plugins/BaseSystemdPlugin.h"

#include <systemd/sd-bus.h>

#include "oomd/Log.h"
#include "oomd/util/ScopeGuard.h"

namespace Oomd {

/*
Communicate with systemd over DBUS interface.
Supports only methods with "ss" signature.
see https://www.freedesktop.org/wiki/Software/systemd/dbus/ for details.

returns false in case of any errors.
*/
bool BaseSystemdPlugin::talkToSystemdManager(
    const std::string& method,
    const std::string& service,
    const std::string& mode) {
  sd_bus_error error = SD_BUS_ERROR_NULL;
  sd_bus_message* m = nullptr;
  sd_bus* bus = nullptr;
  const char* path;
  int r;

  OOMD_SCOPE_EXIT {
    sd_bus_error_free(&error);
    sd_bus_message_unref(m);
    sd_bus_unref(bus);
  };

  /* Connect to the system bus */
  r = sd_bus_open_system(&bus);
  if (r < 0) {
    OLOG << "Failed to connect to system bus: " << strerror(-r);
    return false;
  }

  if (bus == nullptr) {
    OLOG << "Failed to connect to system bus: bus is null";
    return false;
  };

  /* Issue the method call and store the respons message in m */
  r = sd_bus_call_method(
      bus,
      "org.freedesktop.systemd1", /* service to contact */
      "/org/freedesktop/systemd1", /* object path */
      "org.freedesktop.systemd1.Manager", /* interface name */
      method.c_str(), /* method name */
      &error, /* object to return error in */
      &m, /* return message on success */
      "ss", /* input signature */
      service.c_str(), /* first argument */
      mode.c_str()); /* second argument */
  if (r < 0) {
    OLOG << "Failed to issue method call: " << error.message;
    return false;
  }

  if (m == nullptr) {
    OLOG << "Failed to issue method call: return message is null";
    return false;
  }

  /* Parse the response message */
  r = sd_bus_message_read(m, "o", &path);
  if (r < 0) {
    OLOG << "Failed to parse response message: " << strerror(-r);
    return false;
  }

  OLOG << "Queued service job as " << path;
  return true;
}

bool BaseSystemdPlugin::restartService(const std::string& service) {
  OLOG << "Restarting systemd service \"" << service << "\"";
  return talkToSystemdManager("RestartUnit", service);
}

bool BaseSystemdPlugin::stopService(const std::string& service) {
  OLOG << "Stopping systemd service \"" << service << "\"";
  return talkToSystemdManager("StopUnit", service);
}

} // namespace Oomd
