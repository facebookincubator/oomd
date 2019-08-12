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

#include <json/value.h>
#include <sys/types.h>
#include <sys/unistd.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include <getopt.h>

#include "oomd/Log.h"
#include "oomd/Oomd.h"
#include "oomd/PluginRegistry.h"
#include "oomd/Stats.h"
#include "oomd/StatsClient.h"
#include "oomd/config/ConfigCompiler.h"
#include "oomd/config/JsonConfigParser.h"
#include "oomd/include/Assert.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/Defines.h"
#include "oomd/util/Fs.h"

#ifdef MESON_BUILD
#include "Version.h"
#else
#define GIT_VERSION "unknown"
#endif

static constexpr auto kConfigFilePath = "/etc/oomd.json";
static constexpr auto kCgroupFsRoot = "/sys/fs/cgroup";
static constexpr auto kSocketPath = "/run/oomd/oomd-stats.socket";

static void printUsage() {
  std::cerr
      << "usage: oomd [OPTION]...\n\n"
         "optional arguments:\n"
         "  --help, -h                 Show this help message and exit\n"
         "  --version                  Print version and exit\n"
         "  --config, -C CONFIG        Config file (default: /etc/oomd.json)\n"
         "  --interval, -i INTERVAL    Event loop polling interval (default: 5)\n"
         "  --cgroup-fs, -f FS         Cgroup2 filesystem mount point (default: /sys/fs/cgroup)\n"
         "  --check-config, -c CONFIG  Check config file (default: /etc/oomd.json)\n"
         "  --list-plugins, -l         List all available plugins\n"
         "  --drop-in-dir, -w DIR      Directory to watch for drop in configs\n"
         "  --socket-path, -s PATH     Specify stats socket path (default: /run/oomd/oomd-stats.socket)\n"
         "  --dump-stats, -d           Dump accumulated stats\n"
         "  --reset-stats, -r          Reset stats collection\n"
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

static bool cgroup_fs_valid(const std::string& path) {
  std::string cgroup2ParentPath = Oomd::Fs::getCgroup2MountPoint();
  return Oomd::Fs::isUnderParentPath(cgroup2ParentPath, path);
}

static std::unique_ptr<Oomd::Config2::IR::Root> parseConfig(
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

  return ir;
}

int main(int argc, char** argv) {
  std::string flag_conf_file = kConfigFilePath;
  std::string cgroup_fs = kCgroupFsRoot;
  std::string drop_in_dir;
  std::string stats_socket_path = kSocketPath;
  int interval = 5;
  bool should_check_config = false;

  int option_index = 0;
  int c = 0;
  bool should_dump_stats = false;
  bool should_reset_stats = false;

  const char* const short_options = "hvC:w:i:f:c:ls:dr";
  option long_options[] = {
      option{"help", no_argument, nullptr, 'h'},
      option{"version", no_argument, nullptr, 'v'},
      option{"config", required_argument, nullptr, 'C'},
      option{"interval", required_argument, nullptr, 'i'},
      option{"cgroup-fs", required_argument, nullptr, 'f'},
      option{"check-config", required_argument, nullptr, 'c'},
      option{"list-plugins", no_argument, nullptr, 'l'},
      option{"drop-in-dir", required_argument, nullptr, 'w'},
      option{"socket-path", required_argument, nullptr, 's'},
      option{"dump-stats", no_argument, nullptr, 'd'},
      option{"reset-stats", no_argument, nullptr, 'r'},
      option{nullptr, 0, nullptr, 0}};

  while ((c = getopt_long(
              argc, argv, short_options, long_options, &option_index)) != -1) {
    switch (c) {
      case 'h':
        printUsage();
        return 0;
      case 'v':
        std::cout << GIT_VERSION << std::endl;
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
      case 's':
        stats_socket_path = std::string(optarg);
        break;
      case 'd':
        should_dump_stats = true;
        break;
      case 'r':
        should_reset_stats = true;
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

  if (should_dump_stats) {
    try {
      Oomd::StatsClient client(stats_socket_path);
      auto map = client.getStats();
      if (!map) {
        std::cerr << "Failed to retrieve stats";
        return 1;
      }
      Json::Value root;
      for (const auto& pair : *map) {
        root[pair.first] = pair.second;
      }
      std::cout << root.toStyledString() << std::endl;
    } catch (const std::runtime_error& e) {
      std::cerr << e.what() << std::endl;
      return 1;
    }
    return 0;
  }

  if (should_reset_stats) {
    try {
      Oomd::StatsClient client(stats_socket_path);
      int res = client.resetStats();
      if (res != 0) {
        std::cerr << "Reset stats error: received error code= " << res
                  << std::endl;
        return 1;
      }
    } catch (const std::runtime_error& e) {
      std::cerr << e.what() << std::endl;
      return 1;
    }
    return 0;
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
    auto ir = parseConfig(flag_conf_file);
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
    std::cerr << cgroup_fs << " is not a valid cgroup2 filesystem" << std::endl;
    return EXIT_CANT_RECOVER;
  }

  std::cerr << "oomd running with conf_file=" << flag_conf_file
            << " interval=" << interval << std::endl;

  auto ir = parseConfig(flag_conf_file);
  if (!ir) {
    return EXIT_CANT_RECOVER;
  }

  auto engine = Oomd::Config2::compile(*ir);
  if (!engine) {
    OLOG << "Config failed to compile";
    return EXIT_CANT_RECOVER;
  }

  if (!Oomd::Stats::init(stats_socket_path)) {
    OLOG << "Stats collection failed to initialize";
    return 1;
  }

  Oomd::Oomd oomd(
      std::move(ir), std::move(engine), interval, cgroup_fs, drop_in_dir);
  return oomd.run();
}
