#!/bin/bash
g++ main.cpp -o a.out
g++ victim.cpp -o victim.out
./victim.out &

PID=$(pgrep victim.out)

if [ -z "$PID" ]; then
    echo "Victim program not running or PID not found."
    exit 1
fi

sudo ./a.out "$PID"

echo "cgroup.procs:"
cat /sys/fs/cgroup/freezer/my_freezer/cgroup.procs
rm -rf a.out