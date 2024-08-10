EXECUTABLE_PATH="./bin/is.C.x"
REDIS_CGROUP_RELATIVE_PATH=bench-redis # relative to cgroup mount point
NAS_CGROUP_RELATIVE_PATH=bench-nas # relative to cgroup mount point
MEMTIER_RUNTIME=20
WAIT_BETWEEN_TIME=10
RESULTS_ABS_PATH=/home/guyy/oomd/testFiles/results
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

# start redis
echo "Starting Redis"
sudo systemctl start redis-server

# Start NAS in for 10 sec in the background and capture its PID
echo "Running benchmark"

if pgrep -x oomd > /dev/null
then
    echo "oomd is running"
    # sudo systemd-run --unit=nas-job --slice=$NAS_CGROUP_RELATIVE_PATH mpirun -np 4 ./bin/is.C.x > $RESULTS_ABS_PATH/nas_yes_oomd.out 2>&1 &
    # sudo cgexec -g .:bench.slice/bench-nas.slice mpirun -np 4 ./bin/is.C.x > $RESULTS_ABS_PATH/nas_yes_oomd.out 2>&1 &
    sudo systemd-run --slice=$NAS_CGROUP_RELATIVE_PATH --unit=nas-job \
        --working-directory=$(pwd) \
        --property=StandardOutput=file:$RESULTS_ABS_PATH/nas_yes_oomd.out \
        --property=StandardError=file:$RESULTS_ABS_PATH/nas_yes_oomd.out \
        --remain-after-exit \
        mpirun -np 4 ./bin/is.C.x
else
    echo "oomd is not running"
    sudo mpirun -np 4 ./bin/is.C.x > $RESULTS_ABS_PATH/nas_no_oomd.out &
fi

# wait for memtier
echo "Sleeping for $WAIT_BETWEEN_TIME" 
sleep $WAIT_BETWEEN_TIME

# Run memtier for 10 sec
echo "Running memtier for $MEMTIER_RUNTIME"
if pgrep -x oomd > /dev/null
then
    echo "oomd is running"
    # sudo systemd-run --unit=memtier-job --slice=$REDIS_CGROUP_RELATIVE_PATH memtier_benchmark -s 127.0.0.1 -p 6379 -c 50 -t 4 --test-time $MEMTIER_RUNTIME --pipeline=10 --data-size=5000000 > $RESULTS_ABS_PATH/memtier_yes_oomd.out 2>&1 &
    # sudo cgexec -g .:bench.slice/bench-redis.slice memtier_benchmark -s 127.0.0.1 -p 6379 -c 50 -t 4 --test-time $MEMTIER_RUNTIME --pipeline=10 --data-size=5000000 > $RESULTS_ABS_PATH/memtier_yes_oomd.out 2>&1 &
    sudo systemd-run --slice=$REDIS_CGROUP_RELATIVE_PATH --unit=memtier-job \
        --working-directory=$(pwd) \
        --property=StandardOutput=file:$RESULTS_ABS_PATH/memtier_yes_oomd.out \
        --property=StandardError=file:$RESULTS_ABS_PATH/memtier_yes_oomd.out \
        --remain-after-exit \
        memtier_benchmark -s 127.0.0.1 -p 6379 -c 50 -t 4 --test-time $MEMTIER_RUNTIME --pipeline=10 --data-size=5000000
else
    echo "oomd is not running"
    sudo memtier_benchmark -s 127.0.0.1 -p 6379 -c 50 -t 4 --test-time $MEMTIER_RUNTIME --pipeline=10 --data-size=5000000 > $RESULTS_ABS_PATH/memtier_no_oomd.out 2>&1 &

fi

sudo systemctl wait nas-job  # Wait for the job to finish
echo "NAS finished"