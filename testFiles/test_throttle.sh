#!/bin/bash
CGROUP_THROTTLE_NAME="throttle"
THROTTLE_CGROUP_PATH="/sys/fs/cgroup/throttle"

# Create the cgroup named 'test'
if [ ! -d "$THROTTLE_CGROUP_PATH" ]; then
	mkdir -p "$THROTTLE_CGROUP_PATH"
fi

sudo cgexec -g memory:/$CGROUP_THROTTLE_NAME stress-ng --brk 1 --task 1 &

