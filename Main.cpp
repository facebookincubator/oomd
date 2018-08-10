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
#include <iostream>
#include <string>

#include <getopt.h>

#include "oomd/Config.h"
#include "oomd/Log.h"
#include "oomd/Oomd.h"

static constexpr auto kConfigFilePath = "/etc/oomd.json";

static void printUsage() {
  std::cerr
      << "usage: fb-oomd [-h] [--config CONFIG] [--dry] [--report] [--verbose]\n\n"
         "optional arguments:\n"
         "  -h, --help            show this help message and exit\n"
         "  --config CONFIG, -C CONFIG\n"
         "                        Config file (default: /etc/oomd.json)\n"
         "  --dry, -d             Dry run - do not actually kill\n"
         "  --verbose, -v\n"
      << std::endl;
}

int main(int argc, char** argv) {
  std::string flag_conf_file = kConfigFilePath;
  bool flag_dry = false;
  bool flag_verbose = false;

  int option_index = 0;
  int c = 0;

  const char* const short_options = "hC:drv";
  option long_options[] = {option{"sandcastle_mode", no_argument, nullptr, 0},
                           option{"xattr_reporting", no_argument, nullptr, 0},
                           option{"help", no_argument, nullptr, 'h'},
                           option{"config", required_argument, nullptr, 'C'},
                           option{"dry", no_argument, nullptr, 'd'},
                           option{"report", no_argument, nullptr, 'r'},
                           option{"verbose", no_argument, nullptr, 'v'},
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
      case 'd':
        flag_dry = true;
        break;
      case 'r':
        std::cerr << "Noop for backwards compatible report\n";
        break;
      case 'v':
        flag_verbose = true;
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

  if (flag_verbose) {
    OLOG << "oomd running with conf_file=" << flag_conf_file
         << " dry=" << flag_dry << " verbose=" << flag_verbose;
  }

  Oomd::Log::init_or_die();
  auto conf = Oomd::Config(flag_conf_file, flag_dry, flag_verbose);
  Oomd::Oomd oomd;
  conf.apply(oomd);
  return oomd.run();
}
