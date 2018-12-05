#!/bin/bash

set -euo pipefail

if ! command -v clang-format &> /dev/null; then
    echo "Please install clang-format first"
    exit 1
fi

find . \( -iname '*.[ch]' -o -iname '*.cpp' \) -exec clang-format -i {} \;
