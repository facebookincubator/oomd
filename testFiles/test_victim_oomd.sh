#!/bin/bash
# clearing swap space
CGROUP_TEST_NAME="test"
TEST_CGROUP_PATH="/sys/fs/cgroup/test"


# Create the cgroup named 'test'
if [ ! -d "$TEST_CGROUP_PATH" ]; then
	mkdir -p "$TEST_CGROUP_PATH"
fi

sudo swapoff -a
sudo swapon -a
sudo ./unfreeze_and_kill.sh

# run victim process
g++ victim.cpp -o victim.out
sudo ./victim.out 5120 60 &

# run batch job
sudo cgexec -g memory:/$CGROUP_TEST_NAME stress-ng --vm 1 --vm-bytes 70% --timeout 5m


PID=$!

if [ -z "$PID" ]; then
    echo "Victim program not running or PID not found."
    exit 1
fi

echo "before page out"
# pmap -x $PID

