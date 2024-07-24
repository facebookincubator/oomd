#!/bin/bash

CGROUP_TEST_NAME="test"
TEST_CGROUP_PATH="/sys/fs/cgroup/test"


# Create the cgroup named 'test'
if [ ! -d "$TEST_CGROUP_PATH" ]; then
	mkdir -p "$TEST_CGROUP_PATH"
fi

# clearing swap space
sudo swapoff -a
sudo swapon -a
sudo ./unfreeze_and_kill.sh

# run victim process
g++ victim.cpp -o victim.out
sudo ./victim.out 5120 60 &

# run batch job
sudo cgexec -g memory:/$CGROUP_TEST_NAME stress-ng --vm 1 --vm-bytes 70% --timeout 5m &

PID=$!

if [ -z "$PID" ]; then
    echo "Victim program not running or PID not found."
    exit 1
fi

sleep 15

# Add the process to the freezer cgroup
echo "$PID" | sudo tee /sys/fs/cgroup/freezer/my_freezer/cgroup.procs

# Freeze the process
echo "Freezing the process..."
echo FROZEN | sudo tee /sys/fs/cgroup/freezer/my_freezer/freezer.state

# Trigger memory reclamation in the cgroup
echo 1 | sudo tee /sys/fs/cgroup/$CGROUP_TEST_NAME/memory.reclaim

# Verify memory reclamation status
cat /sys/fs/cgroup/$CGROUP_TEST_NAME/memory.stat