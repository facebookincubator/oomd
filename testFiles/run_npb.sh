#!/bin/bash

# Define the path to the IS executable
EXECUTABLE_PATH="./bin/is.C.x"
CGROUP_TEST_NAME="test"
CGROUP_NO_FREEZE="test_no_freeze"

# Flush filesystem buffers
sync

# Drop all caches
echo "Dropping caches..."
sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'

echo "Caches dropped."


# create no freeze cgroup
sudo cgcreate -g memory:/$CGROUP_NO_FREEZE
sudo cgcreate -g memory:/$CGROUP_TEST_NAME




cd NPB3.4.2/NPB3.4-MPI

# Check if the executable exists
if [ -f "$EXECUTABLE_PATH" ]; then
    echo "Executable for IS with CLASS=C already exists."
else
    echo "Executable for IS with CLASS=C not found. Compiling..."
    # Navigate to the IS directory within NPB
    cd NPB3.4.2/NPB3.4-MPI
    # Compile the IS benchmark with CLASS=C
    make is NPROCS=4 CLASS=C
    # Check if the compilation was successful
    if [ -f "$EXECUTABLE_PATH" ]; then
        echo "Compilation successful. Executable for IS with CLASS=C created."
    else
        echo "Compilation failed. Please check the Makefile and source code."
    fi
    cd ../../
fi

echo "Running benchmark"
# Start mpirun in the background and capture its PID
sudo cgexec -g memory:/$CGROUP_TEST_NAME mpirun -np 4 ./bin/is.C.x > ../../results/oomd_strees_25s &
MPIRUN_PID=$!

# wait 2 sec and run stress for 35 sec
echo "sleeping for 2 seconds"
sleep 2
echo "running stressor for 25s"
# sudo cgexec -g memory:/$CGROUP_NO_FREEZE stress-ng --brk 1 --task 1 --timeout 50s
sudo cgexec -g memory:/$CGROUP_NO_FREEZE stress-ng --vm 4 --vm-bytes 90% --vm-keep --timeout 25s
wait $MPIRUN_PID

