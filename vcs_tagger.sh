#!/bin/bash
#
# Copyright (C) 2018-present, Meta Platforms, Inc. and affiliates

set -euo pipefail

dir="$1"
fallback="$2"

# Go into provided dir so builds started from outside the project
# directory still generate the right tags
cd "$dir" &>/dev/null && git describe --abbrev=7 --dirty --tags 2>/dev/null ||
  echo "$fallback"
