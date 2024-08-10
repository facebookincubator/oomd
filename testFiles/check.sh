# Drop all caches
echo "Dropping caches..."
sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'
echo "Caches dropped."

cd NPB3.4.2/NPB3.4-MPI

EXECUTABLE_PATH="./bin/is.C.x"
REDIS_CGROUP_NAME="redis"
NAS_CGROUP_NAME="nas"
MEMTIER_RUNTIME=10
WAIT_BETWEEN_TIME=10

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
    sudo cgexec -g memory:bench/nas mpirun -np 4 ./bin/is.C.x > ../../results/nas_yes_oomd.out &
else
    echo "oomd is not running"
    sudo cgexec -g memory:bench/nas mpirun -np 4 ./bin/is.C.x > ../../results/nas_no_oomd.out &
fi

MPIRUN_PID=$!

# wait for memtier
echo "Sleeping for $WAIT_BETWEEN_TIME" 
sleep $WAIT_BETWEEN_TIME

# Run memtier for $MEMTIER_RUNTIME
echo "Running memtier for $MEMTIER_RUNTIME"
if pgrep -x oomd > /dev/null
then
    echo "oomd is running"
    sudo cgexec -g memory:bench/redis memtier_benchmark -s 127.0.0.1 -p 6379 -c 50 -t 4 --test-time $MEMTIER_RUNTIME --pipeline=10 --data-size=5000000 > ../../results/memtier_yes_oomd.out &
else
    echo "oomd is not running"
    memtier_benchmark -s 127.0.0.1 -p 6379 -c 50 -t 4 --test-time $MEMTIER_RUNTIME --pipeline=10 --data-size=5000000 > ../../results/memtier_no_oomd.out &

fi

wait $MPIRUN_PID
echo "NAS finished"