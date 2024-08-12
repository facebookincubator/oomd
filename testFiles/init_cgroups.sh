#!/bin/bash

SYSTEMD_CONFIG_PATH="/etc/systemd/system"
BENCH_SLICE="bench.slice"
NAS_SLICE="bench-nas.slice"
REDIS_SLICE="bench-redis.slice"
THROTTLE_SLICE="throttle.slice"
TEST_SLICE="test.slice"
GB=$((1024 * 1024 * 1024))

create_slice() {
    local slice_name="$1"
    local mem_limit="$2"

    # Create the slice configuration file
    echo "[Slice]
MemoryMax=$mem_limit
" | sudo tee "$SYSTEMD_CONFIG_PATH/$slice_name"

    # Enable and start the slice
    sudo systemctl daemon-reload
    sudo systemctl start "$slice_name"
}

# Create and start the slices
create_slice "$BENCH_SLICE" $((6 * GB))
create_slice "$NAS_SLICE" $((2 * GB))
create_slice "$REDIS_SLICE" $((4 * GB))
create_slice "$THROTTLE_SLICE" ""
create_slice "$TEST_SLICE" $((6 * GB))

# so sub slices will have cpu controllers too
sudo bash -c 'echo "+cpu" > /sys/fs/cgroup/bench.slice/cgroup.subtree_control'
echo "Systemd slices created and started successfully."
