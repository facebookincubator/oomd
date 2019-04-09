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
#include "oomd/config/ConfigCompiler.h"
#include "oomd/config/JsonConfigParser.h"
#include "oomd/include/Assert.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/Defines.h"
#include "oomd/util/Fs.h"

static constexpr auto kConfigFilePath = "/etc/oomd.json";
static constexpr auto kCgroupFsRoot = "/sys/fs/cgroup";

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

static std::unique_ptr<Oomd::Engine::Engine> parseAndCompile(
    const std::string& flag_conf_file) {
  std::ifstream conf_file(flag_conf_file, std::ios::in);
  if (!conf_file.is_open()) {
    std::cerr << "Could not open confg_file=" << flag_conf_file << std::endl;
    return nullptr;
  }
  std::stringstream buf;
  buf << conf_file.rdbuf();
  Oomd::Config2::JsonConfigParser json_parser;
  auto ir = json_parser.parse(buf.str());
  if (!ir) {
    std::cerr << "Could not parse conf_file=" << flag_conf_file << std::endl;
    return nullptr;
  }
  return Oomd::Config2::compile(*ir);
}

int main(int argc, char** argv) {
  std::string flag_conf_file = kConfigFilePath;
  std::string cgroup_fs = kCgroupFsRoot;
  int interval = 5;
  bool should_check_config = false;

  int option_index = 0;
  int c = 0;

  const char* const short_options = "hC:drvi:f:c:l";
  option long_options[] = {
      option{"sandcastle_mode", no_argument, nullptr, 0},
      option{"xattr_reporting", no_argument, nullptr, 0},
      option{"help", no_argument, nullptr, 'h'},
      option{"config", required_argument, nullptr, 'C'},
      option{"dry", no_argument, nullptr, 'd'},
      option{"report", no_argument, nullptr, 'r'},
      option{"verbose", no_argument, nullptr, 'v'},
      option{"interval", required_argument, nullptr, 'i'},
      option{"cgroup-fs", required_argument, nullptr, 'f'},
      option{"check-config", required_argument, nullptr, 'c'},
      option{"list-plugins", no_argument, nullptr, 'l'},
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
      case 'd':
        std::cerr << "Noop for backwards compatible dry\n";
        break;
      case 'l':
        std::cerr << "List of plugins oomd was compiled with:\n";
        for (const auto& plugin_name :
             Oomd::getPluginRegistry().getRegistered()) {
          std::cerr << " " << plugin_name << "\n";
        }
        return 0;
      case 'r':
        std::cerr << "Noop for backwards compatible report\n";
        break;
      case 'v':
        std::cerr << "Noop for backwards compatible verbose\n";
        break;
      case 'i':
        interval = std::stoi(optarg);
        break;
      case 'f':
        cgroup_fs = std::string(optarg);
        break;
      case 0:
        if (long_options[option_index].flag != nullptr) {
          break;
        }
        if (strcmp(long_options[option_index].name, "sandcastle_mode") == 0) {
          std::cerr << "Noop for backwards compatible sandcastle_mode\n";
        } else if (
            strcmp(long_options[option_index].name, "xattr_reporting") == 0) {
          std::cerr << "Noop for backwards compatible xattr_reporting\n";
        }
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
    auto ret = parseAndCompile(flag_conf_file);
    if (!ret) {
      OLOG << "Config is not valid";
    }
    return ret ? 0 : 1;
  }

  if (!system_reqs_met()) {
    std::cerr << "System requirements not met\n";
    return EXIT_CANT_RECOVER;
  }

  std::cerr << "oomd running with conf_file=" << flag_conf_file
            << " interval=" << interval << std::endl;

  auto engine = parseAndCompile(flag_conf_file);
  if (!engine) {
    OLOG << "Config failed to compile";
    return EXIT_CANT_RECOVER;
  }

  Oomd::Oomd oomd(std::move(engine), interval, cgroup_fs);
  return oomd.run();
}
