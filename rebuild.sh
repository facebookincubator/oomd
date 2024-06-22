#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
set -e

# Remove the build directory
sudo rm -r build/

# Create the build directory and compile the project
meson build && ninja -C build

# Change to the build directory and install the project
cd build && sudo ninja install

# Run the OOMD daemon with the specified configuration file
sudo /usr/local/bin/oomd --config /usr/local/etc/oomd/oomd.json &