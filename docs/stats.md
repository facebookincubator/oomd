# Structured statistics

oomd stores statistics in a thread-safe key-value map. Keys are strings, and
values are integers. The daemon also serves these values through a Unix socket.

## Internal API

### Get all statistics

    std::unordered_map<std::string, int> getStats()

This function returns a copy of the current map.

### Increment a value

    int incrementStat(const std::string& key, int val)

This function adds `val` to the value for `key`. It creates the key if needed.
It returns 0 on success and 1 if the statistics service is not initialized.

### Set a value

    int setStat(const std::string& key, int val)

This function sets `key` to `val`. It returns 0 on success and 1 if the
statistics service is not initialized.

### Reset all values

    int resetStats()

This function sets all existing values to zero. It returns 0 on success and 1
if the statistics service is not initialized.

## Command-line interface

The running daemon listens on `oomd-stats.socket` in its runtime directory.
The default runtime directory is `/run/oomd`. Use `--runtime-dir` if the daemon
uses a different directory.

Use `--dump-stats` or `-d` to print the current values as JSON:

    $ oomd --dump-stats
    {
      "oomd.dropin.added" : 0,
      "oomd.dropin.fired" : 0,
      "oomd.kills" : 1
    }

Use `--reset-stats` or `-r` to set all values to zero. If both options are
present, oomd prints the values before it resets them.
