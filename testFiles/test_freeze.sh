#!/bin/bash
CGROUP_TEST_NAME="test"
TEST_CGROUP_PATH="/sys/fs/cgroup/test"
FREEZER_CGROUP_PATH="/sys/fs/cgroup/freezer"

# Create the cgroup named 'test'
if [ ! -d "$TEST_CGROUP_PATH" ]; then
	mkdir -p "$TEST_CGROUP_PATH"
fi
# Set up the freezer cgroup if it's not already
# if [ ! -d "$FREEZER_CGROUP_PATH" ]; then
#     sudo mkdir "$FREEZER_CGROUP_PATH"
#     sudo mount -t cgroup -o freezer freezer "$FREEZER_CGROUP_PATH"
# fi   

sudo cgexec -g memory:/$CGROUP_TEST_NAME stress-ng --brk 1 --task 1 &

