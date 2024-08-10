# Define cgroup name and path
CGROUP_TEST_NAME="test1"
TEST_CGROUP_PATH="/sys/fs/cgroup/$CGROUP_TEST_NAME"
# export LD_PRELOAD=/home/guyy/oomd/testFiles/override_madvise2.so
# Create the cgroup named 'test' if it doesn't exist
if [ ! -d "$TEST_CGROUP_PATH" ]; then
    sudo mkdir -p "$TEST_CGROUP_PATH"
fi

#Compile the dynamic library
g++ -shared -fPIC -o override_madvise.so override_madvise.cpp -ldl -lstdc++

# Set appropriate permissions for the cgroup directory
sudo chown $(whoami):$(whoami) "$TEST_CGROUP_PATH"
sudo chmod 755 "$TEST_CGROUP_PATH"

# Run stress-ng within the 'test' cgroup with sudo
./LD_PRELOAD=/home/guyy/oomd/testFiles/override_madvise.so ./madvise_test.o &
sudo LD_PRELOAD=/home/guyy/oomd/testFiles/override_madvise.so cgexec -g memory:/$CGROUP_TEST_NAME stress-ng --brk 1 --task 1 --timeout 2m
# Run strace to trace madvise calls within the 'test' cgroup and log output to a file
# sudo strace -f -e trace=madvise cgexec -g memory:/$CGROUP_TEST_NAME stress-ng --brk 1 --task 1 --timeout 2m 2>&1 | tee strace_madvise.log