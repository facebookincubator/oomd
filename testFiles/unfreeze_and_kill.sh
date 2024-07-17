echo "THAWED" | sudo tee /sys/fs/cgroup/freezer/my_freezer/freezer.state
pkill victim.out
pkill stress