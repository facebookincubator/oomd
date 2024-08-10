#!/bin/bash
CGROUP_THROTTLE_NAME="throttle"

sudo cgexec -g memory:/$CGROUP_THROTTLE_NAME stress-ng --brk 1 --task 1 &

