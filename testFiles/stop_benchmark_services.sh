sudo systemctl stop nas-job
sudo systemctl disable nas-job
sudo rm /etc/systemd/system/nas-job
sudo systemctl stop memtier-job
sudo systemctl disable memtier-job
sudo rm /etc/systemd/system/memtier-job
sudo systemctl daemon-reload
sudo systemctl reset-failed