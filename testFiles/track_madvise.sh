# Define cgroup name and path
CGROUP_TEST_NAME="test1"
TEST_CGROUP_PATH="/sys/fs/cgroup/memory/$CGROUP_TEST_NAME"
export LD_PRELOAD=/home/guyy/oomd/testFiles/override_madvise.so
# Create the cgroup named 'test' if it doesn't exist
if [ ! -d "$TEST_CGROUP_PATH" ]; then
    sudo mkdir -p "$TEST_CGROUP_PATH"
fi

sudo cgexec -g memory:/$CGROUP_TEST_NAME stress-ng --brk 1 --task 1 --timeout 2m
# Run strace to trace madvise calls within the 'test' cgroup and log output to a file
# sudo strace -f -e trace=madvise cgexec -g memory:/$CGROUP_TEST_NAME stress-ng --brk 1 --task 1 --timeout 2m 2>&1 | tee strace_madvise.log