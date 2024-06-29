#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
set -e

sudo rm -rf build/

# Create the build directory and configure the project for debug
meson build --buildtype=debug

# Compile the project using Ninja
ninja -C build

# Change to the build directory and install the project
cd build && sudo ninja install
