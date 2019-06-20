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

#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include <getopt.h>

#include "oomd/Log.h"
#include "oomd/Oomd.h"
#include "oomd/PluginRegistry.h"
#include "oomd/config/CompactConfigParser.h"
#include "oomd/config/ConfigCompiler.h"
#include "oomd/config/JsonConfigParser.h"
#include "oomd/include/Assert.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/Defines.h"
#include "oomd/util/Fs.h"
#include "oomd/util/Util.h"

static constexpr auto kConfigFilePath = "/etc/oomd.json";
static constexpr auto kCgroupFsRoot = "/sys/fs/cgroup";

enum class ConfigFormat {
  JSON = 0,
  COMPACT,
};

static void printUsage() {
  std::cerr
      << "usage: oomd [OPTION]...\n\n"
         "optional arguments:\n"
         "  --help, -h                 Show this help message and exit\n"
         "  --config, -C CONFIG        Config file (default: /etc/oomd.json)\n"
         "  --interval, -i INTERVAL    Event loop polling interval (default: 5)\n"
         "  --cgroup-fs, -f FS         Cgroup2 filesystem mount point (default: /sys/fs/cgroup)\n"
         "  --check-config, -c CONFIG  Check config file (default: /etc/oomd.json)\n"
         "  --list-plugins, -l         List all available plugins\n"
         "  --drop-in-dir, -w DIR      Directory to watch for drop in configs\n"
         "  -Xoption=value             Additional options\n"
         "\n"
         "additional options:\n"
         "  -Xconfig=[json|compact]\n"
      << std::endl;
}

static bool system_reqs_met() {
  // 4.20 mempressure file
  auto psi = Oomd::Fs::readFileByLine("/proc/pressure/memory");
  if (psi.size()) {
    return true;
  }

  // Experimental mempressure file
  psi = Oomd::Fs::readFileByLine("/proc/mempressure");
  if (psi.size()) {
    return true;
  }

  std::cerr
      << "PSI not detected. Is your system running a new enough kernel?\n";
  return false;
}

static bool cgroup_fs_valid(std::string& path) {
  std::string cgroup2ParentPath = Oomd::Fs::getCgroup2MountPoint();
  return Oomd::Fs::isUnderParentPath(cgroup2ParentPath, path);
}

static std::unique_ptr<Oomd::Config2::IR::Root> parseConfig(
    ConfigFormat format,
    const std::string& flag_conf_file) {
  std::unique_ptr<Oomd::Config2::IR::Root> ir;

  std::ifstream conf_file(flag_conf_file, std::ios::in);
  if (!conf_file.is_open()) {
    std::cerr << "Could not open config_file=" << flag_conf_file << std::endl;
    return nullptr;
  }

  std::stringstream buf;
  buf << conf_file.rdbuf();

  if (format == ConfigFormat::JSON) {
    Oomd::Config2::JsonConfigParser json_parser;
    ir = json_parser.parse(buf.str());
  } else if (format == ConfigFormat::COMPACT) {
    Oomd::Config2::CompactConfigParser compact_parser;
    ir = compact_parser.parse(buf.str());
  } else {
    std::cerr << "Unhandled config parser format" << std::endl;
  }

  if (!ir) {
    std::cerr << "Could not parse conf_file=" << flag_conf_file << std::endl;
    return nullptr;
  }

  return ir;
}

static int processAdditionalOptions(
    const std::string& opt_pair,
    ConfigFormat& config_format) {
  auto parts = Oomd::Util::split(opt_pair, '=');
  if (parts.size() != 2) {
    std::cerr << "Invalid option=\"" << opt_pair << "\"" << std::endl;
    return 1;
  }

  for (auto& p : parts) {
    Oomd::Util::strip(p);
  }

  if (parts[0] == "config") {
    if (parts[1] == "json") {
      config_format = ConfigFormat::JSON;
    } else if (parts[1] == "compact") {
      config_format = ConfigFormat::COMPACT;
    } else {
      std::cerr << "Unrecognized config format=" << parts[1] << std::endl;
      return 1;
    }
  }

  return 0;
}

int main(int argc, char** argv) {
  std::string flag_conf_file = kConfigFilePath;
  std::string cgroup_fs = kCgroupFsRoot;
  std::string drop_in_dir;
  int interval = 5;
  bool should_check_config = false;
  ConfigFormat config_format = ConfigFormat::JSON;

  int option_index = 0;
  int c = 0;

  const char* const short_options = "hC:w:i:f:c:lX:";
  option long_options[] = {
      option{"help", no_argument, nullptr, 'h'},
      option{"config", required_argument, nullptr, 'C'},
      option{"interval", required_argument, nullptr, 'i'},
      option{"cgroup-fs", required_argument, nullptr, 'f'},
      option{"check-config", required_argument, nullptr, 'c'},
      option{"list-plugins", no_argument, nullptr, 'l'},
      option{"drop-in-dir", required_argument, nullptr, 'w'},
      option{"", required_argument, nullptr, 'X'},
      option{nullptr, 0, nullptr, 0}};

  while ((c = getopt_long(
              argc, argv, short_options, long_options, &option_index)) != -1) {
    switch (c) {
      case 'h':
        printUsage();
        return 0;
      case 'C':
        flag_conf_file = std::string(optarg);
        break;
      case 'c':
        should_check_config = true;
        flag_conf_file = std::string(optarg);
        break;
      case 'w':
        drop_in_dir = std::string(optarg);
        break;
      case 'l':
        std::cerr << "List of plugins oomd was compiled with:\n";
        for (const auto& plugin_name :
             Oomd::getPluginRegistry().getRegistered()) {
          std::cerr << " " << plugin_name << "\n";
        }
        return 0;
      case 'i':
        interval = std::stoi(optarg);
        break;
      case 'f':
        cgroup_fs = std::string(optarg);
        break;
      case 'X':
        if (processAdditionalOptions(std::string(optarg), config_format)) {
          return 1;
        }
        break;
      case 0:
        break;
      case '?':
        std::cerr << "Unknown option or missing argument\n";
        printUsage();
        return 1;
      default:
        return 1;
    }
  }

  if (optind < argc) {
    std::cerr << "Non-option argument is not supported: ";
    while (optind < argc) {
      std::cerr << "\"" << argv[optind++] << "\" ";
    }
    std::cerr << std::endl;
    printUsage();
    return 1;
  }

  // Init oomd logging code
  //
  // NB: do not use OLOG before initializing logging. Doing so will disable
  // kmsg logging (due to some weird setup code required to get unit testing
  // correct). Be careful not to make any oomd library calls before initing
  // logging.
  if (!Oomd::Log::init()) {
    std::cerr << "Logging failed to initialize. Try running with sudo\n";
    return 1;
  }

  if (should_check_config) {
    auto ir = parseConfig(config_format, flag_conf_file);
    if (!ir) {
      return 1;
    }

    auto engine = Oomd::Config2::compile(*ir);
    if (!engine) {
      OLOG << "Config is not valid";
      return 1;
    }

    return 0;
  }

  if (!system_reqs_met()) {
    std::cerr << "System requirements not met\n";
    return EXIT_CANT_RECOVER;
  }

  if (!cgroup_fs_valid(cgroup_fs)) {
    std::cerr << "cgroup path not valid\n";
    return EXIT_CANT_RECOVER;
  }

  std::cerr << "oomd running with conf_file=" << flag_conf_file
            << " interval=" << interval << std::endl;

  auto ir = parseConfig(config_format, flag_conf_file);
  if (!ir) {
    return EXIT_CANT_RECOVER;
  }

  auto engine = Oomd::Config2::compile(*ir);
  if (!engine) {
    OLOG << "Config failed to compile";
    return EXIT_CANT_RECOVER;
  }

  Oomd::Oomd oomd(
      std::move(ir), std::move(engine), interval, cgroup_fs, drop_in_dir);
  return oomd.run();
}
