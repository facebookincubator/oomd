#!/bin/bash

# Pass the "TESTED_PLUGIN" parameter as an argument
TESTED_PLUGIN="$1"

if [ -z "$TESTED_PLUGIN" ]; then
    echo "Error: No TESTED_PLUGIN provided. Usage: ./script_name.sh <TESTED_PLUGIN>"
    exit 1
fi

echo "Running script with TESTED_PLUGIN: $TESTED_PLUGIN"

sudo ./stop_benchmark_services.sh

EXECUTABLE_PATH="./bin/is.C.x"
REDIS_CGROUP_RELATIVE_PATH=bench-redis # relative to cgroup mount point
NAS_CGROUP_RELATIVE_PATH=bench-nas # relative to cgroup mount point
MEMTIER_RUNTIME=20
WAIT_BETWEEN_TIME=10
RESULTS_ABS_PATH="/home/guyy/oomd/testFiles/results"
MEMTIER_DATA_SIZE=$((10 * 1024 * 1024))

# make sure cgroups exist
sudo ./init_cgroups.sh

# Flush filesystem buffers
sync

# Drop all caches
echo "Dropping caches..."
sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'
echo "Caches dropped."

cd NPB3.4.2/NPB3.4-MPI

# Check if the executable exists
if [ -f "$EXECUTABLE_PATH" ]; then
    echo "Executable for IS with CLASS=C already exists."
else
    echo "Executable for IS with CLASS=C not found. Compiling..."
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

# Start redis
echo "Starting Redis"
sudo systemctl start redis-server

# Start NAS in for 10 sec in the background and capture its PID
echo "Running benchmark"

if pgrep -x oomd > /dev/null; then
    echo "oomd is running"
    sudo systemd-run --slice=$NAS_CGROUP_RELATIVE_PATH --unit=nas-job \
        --working-directory=$(pwd) \
        --property=StandardOutput=file:$RESULTS_ABS_PATH/nas_yes_oomd_${TESTED_PLUGIN}.out \
        --property=StandardError=file:$RESULTS_ABS_PATH/nas_yes_oomd_${TESTED_PLUGIN}.out \
        --remain-after-exit \
        mpirun -np 4 ./bin/is.C.x
else
    echo "oomd is not running"
    sudo mpirun -np 4 ./bin/is.C.x > $RESULTS_ABS_PATH/nas_no_oomd_${TESTED_PLUGIN}.out &
fi

# wait for memtier
echo "Sleeping for $WAIT_BETWEEN_TIME" 
sleep $WAIT_BETWEEN_TIME

# Run memtier for 10 sec
echo "Running memtier for $MEMTIER_RUNTIME"
if pgrep -x oomd > /dev/null; then
    echo "oomd is running"
    sudo systemd-run --slice=$REDIS_CGROUP_RELATIVE_PATH --unit=memtier-job \
        --working-directory=$(pwd) \
        --property=StandardOutput=file:$RESULTS_ABS_PATH/memtier_yes_oomd_${TESTED_PLUGIN}.out \
        --property=StandardError=file:$RESULTS_ABS_PATH/memtier_yes_oomd_${TESTED_PLUGIN}.out \
        --remain-after-exit \
        memtier_benchmark -s 127.0.0.1 -p 6379 -c 50 -t 4 --test-time $MEMTIER_RUNTIME --pipeline=10 --data-size=$MEMTIER_DATA_SIZE
else
    echo "oomd is not running"
    sudo memtier_benchmark -s 127.0.0.1 -p 6379 -c 50 -t 4 --test-time $MEMTIER_RUNTIME --pipeline=10 --data-size=$MEMTIER_DATA_SIZE > $RESULTS_ABS_PATH/memtier_no_oomd_${TESTED_PLUGIN}.out 2>&1 &
fi

sudo systemctl wait nas-job  # Wait for the job to finish
echo "NAS finished"