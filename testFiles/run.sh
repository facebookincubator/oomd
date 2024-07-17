#!/bin/bash

# clearing swap space
sudo swapoff -a
sudo swapon -a
sudo ./unfreeze_and_kill.sh

g++ FreezeAndPageOut.cpp -o a.out
g++ victim.cpp -o victim.out
./victim.out 250 &

PID=$!

if [ -z "$PID" ]; then
    echo "Victim program not running or PID not found."
    exit 1
fi

echo "before page out"
sleep 3
pmap -x $PID

sudo ./a.out "$PID"

echo "after page out"
pmap -x $PID


rm -rf a.out

#wait for swap to fill
sleep 5

g++ -g PageInAndUnfreeze.cpp -o b.out
sudo ./b.out "$PID"

