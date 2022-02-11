#!/bin/bash
#
# Copyright (C) 2018-present, Meta Platforms, Inc. and affiliates

set -euo pipefail

if ! command -v clang-format &> /dev/null; then
    echo "Please install clang-format first"
    exit 1
fi

find . \( -iname '*.[ch]' -o -iname '*.cpp' \) -exec clang-format -i {} \;
