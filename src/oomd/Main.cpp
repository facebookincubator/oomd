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
#include <sys/file.h>
#include <sys/types.h>
#include <sys/unistd.h>
#include <cstring>
#ifdef MESON_BUILD
#include <filesystem>
#else
#include <experimental/filesystem>
#endif
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

#include <getopt.h>

#include "oomd/Log.h"
#include "oomd/Oomd.h"
#include "oomd/PluginRegistry.h"
#include "oomd/Stats.h"
#include "oomd/StatsClient.h"
#include "oomd/config/ConfigCompiler.h"
#include "oomd/config/JsonConfigParser.h"
#include "oomd/include/CgroupPath.h"
#include "oomd/include/CoreStats.h"
#include "oomd/include/Defines.h"
#include "oomd/util/FsExceptionless.h"
#include "oomd/util/Util.h"

#ifdef MESON_BUILD
#include "Version.h" // @manual
#else
#define GIT_VERSION "unknown"
#endif

#ifdef MESON_BUILD
namespace fs = std::filesystem;
#else
namespace fs = std::experimental::filesystem;
#endif

static constexpr auto kConfigFilePath = "/etc/oomd.json";
static constexpr auto kCgroupFsRoot = "/sys/fs/cgroup";
static constexpr auto kRuntimeDir = "/run/oomd";
static constexpr auto kRuntimeLock = "oomd.lock";
static constexpr auto kStatsSocket = "oomd-stats.socket";
static constexpr auto kKmsgPath = "/dev/kmsg";
static const struct Oomd::IOCostCoeffs default_hdd_coeffs = {
    .read_iops = 1.31e-3,
    .readbw = 1.13e-7,
    .write_iops = 2.58e-1,
    .writebw = 5.04e-7,
    .trim_iops = 0,
    .trimbw = 0,
};
static const struct Oomd::IOCostCoeffs default_ssd_coeffs = {
    .read_iops = 1.21e-2,
    .readbw = 6.25e-7,
    .write_iops = 1.07e-3,
    .writebw = 2.61e-7,
    .trim_iops = 2.37e-2,
    .trimbw = 9.10e-10,
};

static void printUsage() {
  std::cerr
      << "usage: oomd [OPTION]...\n\n"
         "optional arguments:\n"
         "  --help, -h                 Show this help message and exit\n"
         "  --version, -v              Print version and exit\n"
         "  --config, -C CONFIG        Config file (default: /etc/oomd.json)\n"
         "  --interval, -i INTERVAL    Event loop polling interval (default: 5)\n"
         "  --cgroup-fs, -f FS         Cgroup2 filesystem mount point (default: /sys/fs/cgroup)\n"
         "  --check-config, -c CONFIG  Check config file (default: /etc/oomd.json)\n"
         "  --list-plugins, -l         List all available plugins\n"
         "  --drop-in-dir, -w DIR      Directory to watch for drop in configs\n"
         "  --runtime-dir, -D PATH     Directory for runtime files (default: /run/oomd)\n"
         "  --dump-stats, -d           Dump accumulated stats\n"
         "  --reset-stats, -r          Reset stats collection\n"
         "  --device DEVS              Comma separated <major>:<minor> pairs for IO cost calculation (default: none)\n"
         "  --ssd-coeffs COEFFS        Comma separated values for SSD IO cost calculation (default: see doc)\n"
         "  --hdd-coeffs COEFFS        Comma separated values for HDD IO cost calculation (default: see doc)\n"
         "  --kmsg-override PATH       File to log kills to (default: /dev/kmsg)"
      << std::endl;
}

static bool system_reqs_met() {
  // 4.20 mempressure file
  auto psi = Oomd::FsExceptionless::readFileByLine("/proc/pressure/memory");
  if (psi && psi->size()) {
    return true;
  }

  // Experimental mempressure file
  psi = Oomd::FsExceptionless::readFileByLine("/proc/mempressure");
  if (psi && psi->size()) {
    return true;
  }

  std::cerr
      << "PSI not detected. Is your system running a new enough kernel?\n";
  return false;
}

static bool cgroup_fs_valid(const std::string& path) {
  auto cgroup2ParentPath = Oomd::FsExceptionless::getCgroup2MountPoint();
  if (!cgroup2ParentPath) {
    return false;
  }
  return Oomd::FsExceptionless::isUnderParentPath(*cgroup2ParentPath, path);
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

static Oomd::IOCostCoeffs parseCoeffs(const std::string& str_coeffs) {
  Oomd::IOCostCoeffs coeffs = {};
  auto parts = Oomd::Util::split(str_coeffs, ',');
  auto coeff_fields = {&coeffs.read_iops,
                       &coeffs.readbw,
                       &coeffs.write_iops,
                       &coeffs.writebw,
                       &coeffs.trim_iops,
                       &coeffs.trimbw};

  size_t idx = 0;
  for (auto& coeff_field : coeff_fields) {
    *coeff_field = idx >= parts.size() ? 0 : std::stod(parts[idx]);
    idx++;
  }
  return coeffs;
}

static Oomd::SystemMaybe<std::unordered_map<std::string, Oomd::DeviceType>>
parseDevices(const std::string& str_devices) {
  std::unordered_map<std::string, Oomd::DeviceType> io_devs;
  auto parts = Oomd::Util::split(str_devices, ',');
  for (const auto& dev_id : parts) {
    auto dev_type = Oomd::FsExceptionless::getDeviceType(dev_id);
    if (!dev_type) {
      return SYSTEM_ERROR(dev_type.error());
    }
    io_devs[dev_id] = *dev_type;
  }
  return io_devs;
}

static void initializeCoreStats() {
  // Zero out core keys so that there aren't any "missing" keys
  // until the associated event occurs
  for (const char* key : Oomd::CoreStats::kAllKeys) {
    Oomd::setStat(key, 0);
  }
}

static bool initRuntimeDir(const fs::path& runtime_dir) {
  std::array<char, 64> err_buf = {};
  std::error_code ec;

  // Ignore return value of fs::create_directories because it indicates
  // whether directories were created or not. We don't care about this
  // information, only whether or not the directories exist without error
  // after the call.
  fs::create_directories(runtime_dir);
  if (ec) {
    OLOG << "Failed to create runtime directory=" << runtime_dir << ": " << ec;
    return false;
  }

  fs::path lockfile = runtime_dir / kRuntimeLock;

  // Don't bother storing file lock FD. Just let it close when process exits.
  int lockfd = ::open(lockfile.c_str(), O_CREAT, S_IRUSR | S_IWUSR);
  if (lockfd < 0) {
    OLOG << "Failed to open lock file=" << lockfile << ": "
         << ::strerror_r(errno, err_buf.data(), err_buf.size());
    return false;
  }

  if (::flock(lockfd, LOCK_EX | LOCK_NB)) {
    OLOG << "Failed to acquire exclusive runtime lock=" << lockfile << ": "
         << ::strerror_r(errno, err_buf.data(), err_buf.size());
    return false;
  }

  return true;
}

enum MainOptions {
  OPT_DEVICE = 256, // avoid collision with char
  OPT_SSD_COEFFS,
  OPT_HDD_COEFFS,
};

int main(int argc, char** argv) {
  std::string flag_conf_file = kConfigFilePath;
  std::string cgroup_fs = kCgroupFsRoot;
  std::string drop_in_dir;
  std::string runtime_dir = std::string(kRuntimeDir);
  std::string stats_socket_path = runtime_dir + "/" + kStatsSocket;
  std::string dev_id;
  std::string kmsg_path = kKmsgPath;
  int interval = 5;
  bool should_check_config = false;

  int option_index = 0;
  int c = 0;
  bool should_dump_stats = false;
  bool should_reset_stats = false;
  Oomd::SystemMaybe<std::unordered_map<std::string, Oomd::DeviceType>> io_devs;
  struct Oomd::IOCostCoeffs hdd_coeffs = default_hdd_coeffs;
  struct Oomd::IOCostCoeffs ssd_coeffs = default_ssd_coeffs;

  const char* const short_options = "hvC:w:i:f:c:lD:dr";
  option long_options[] = {
      option{"help", no_argument, nullptr, 'h'},
      option{"version", no_argument, nullptr, 'v'},
      option{"config", required_argument, nullptr, 'C'},
      option{"interval", required_argument, nullptr, 'i'},
      option{"cgroup-fs", required_argument, nullptr, 'f'},
      option{"check-config", required_argument, nullptr, 'c'},
      option{"list-plugins", no_argument, nullptr, 'l'},
      option{"drop-in-dir", required_argument, nullptr, 'w'},
      option{"runtime-dir", required_argument, nullptr, 'D'},
      option{"dump-stats", no_argument, nullptr, 'd'},
      option{"reset-stats", no_argument, nullptr, 'r'},
      option{"device", required_argument, nullptr, OPT_DEVICE},
      option{"ssd-coeffs", required_argument, nullptr, OPT_SSD_COEFFS},
      option{"hdd-coeffs", required_argument, nullptr, OPT_HDD_COEFFS},
      option{"kmsg-override", required_argument, nullptr, 'k'},
      option{nullptr, 0, nullptr, 0}};

  while ((c = getopt_long(
              argc, argv, short_options, long_options, &option_index)) != -1) {
    size_t parsed_len;
    bool parse_error = false;

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
        try {
          interval = std::stoi(optarg, &parsed_len);
        } catch (const std::invalid_argument& e) {
          parse_error = true;
        }
        if (parse_error || interval < 1 || parsed_len != strlen(optarg)) {
          std::cerr << "Interval not a >0 integer: " << optarg << std::endl;
          return 1;
        }

        break;
      case 'f':
        cgroup_fs = std::string(optarg);
        break;
      case 'D':
        runtime_dir = std::string(optarg);
        stats_socket_path = runtime_dir + "/" + kStatsSocket;
        break;
      case 'd':
        should_dump_stats = true;
        break;
      case 'r':
        should_reset_stats = true;
        break;
      case OPT_DEVICE:
        io_devs = parseDevices(optarg);
        if (!io_devs) {
          std::cerr << "Invalid devices: " << io_devs.error().what() << '\n';
          return 1;
        }
        break;
      case OPT_SSD_COEFFS:
        try {
          ssd_coeffs = parseCoeffs(optarg);
        } catch (const std::invalid_argument& e) {
          std::cerr << "Invalid SSD coefficients: " << e.what() << '\n';
          return 1;
        }
        break;
      case OPT_HDD_COEFFS:
        try {
          hdd_coeffs = parseCoeffs(optarg);
        } catch (const std::invalid_argument& e) {
          std::cerr << "Invalid HDD coefficients: " << e.what() << '\n';
          return 1;
        }
        break;
      case 'k':
        kmsg_path = std::string(optarg);
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
        std::cerr << "Failed to retrieve stats" << std::endl;
        return 1;
      }
      Json::Value root(Json::objectValue);
      for (const auto& pair : *map) {
        root[pair.first] = pair.second;
      }
      std::cout << root.toStyledString() << std::endl;
    } catch (const std::runtime_error& e) {
      std::cerr << e.what() << std::endl;
      return 1;
    }
    if (!should_reset_stats) {
      return 0;
    }
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
  if (!Oomd::Log::init(kmsg_path)) {
    std::cerr << "Logging failed to initialize. Try running with sudo\n";
    return 1;
  }

  if (should_check_config) {
    auto ir = parseConfig(flag_conf_file);
    if (!ir) {
      return 1;
    }

    Oomd::PluginConstructionContext compile_context(cgroup_fs);

    auto engine = Oomd::Config2::compile(*ir, compile_context);
    if (!engine) {
      OLOG << "Config is not valid";
      return 1;
    }

    return 0;
  }

  //
  // Daemon code below here
  //

  if (!initRuntimeDir(runtime_dir)) {
    return 1;
  }

  // NB: do not start stats module unless we are going to daemonize
  if (!Oomd::Stats::init(stats_socket_path)) {
    OLOG << "Stats module failed to initialize";
    return 1;
  }

  initializeCoreStats();

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

  Oomd::PluginConstructionContext compile_context(cgroup_fs);

  auto engine = Oomd::Config2::compile(*ir, compile_context);
  if (!engine) {
    OLOG << "Config failed to compile";
    return EXIT_CANT_RECOVER;
  }

  Oomd::Oomd oomd(
      std::move(ir),
      std::move(engine),
      interval,
      cgroup_fs,
      drop_in_dir,
      *io_devs,
      hdd_coeffs,
      ssd_coeffs);
  return oomd.run();
}
