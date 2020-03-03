# Auxiliary plugins

Auxiliary plugins are plugins that are either not generic enough to be be
considered core or require optional dependencies.

# Actions

## systemd_restart

### Arguments

    service
    post_action_delay=15 (optional)
    dry=false (optional)

### Description

Restarts systemd service: `service`.

STOP on success, CONTINUE otherwise.

## senpai

### Arguments

    cgroup
    limit_min_bytes=1<<30 (optional)
    limit_max_bytes=500<<30 (optional)
    interval=6 (optional)
    pressure_ms=10 (optional)
    pressure_ms=10 (optional)
    max_probe=0.01 (optional)
    max_backoff=1.0 (optional)
    coeff_probe=10 (optional)
    coeff_backoff=20 (optional)

### Description

Slowly applies memory pressure to `cgroup` by decreasing memory.high until the
cgroup begins to experience some memory pressure, or memory.high is as low as
memory.min. Once memory pressure is detected, back off by increasing
memory.high. Over time, `cgroup`'s memory.current will converge to the actual
amount of memory it needs to operate.

`cgroup` is the cgroup to act on. `cgroup` supports multi-cgroup and wildcarded
paths. Eg:

    cgroup=workload.slice/workload-*.slice,system.slice

The root host can be encoded as "/".

`limit_min_bytes` is the minimum value that should be written to memory.high.

`limit_max_bytes` is the maximum value that should be written to memory.high.

`interval` is the number of event loop ticks to wait before adjusting memory.high.
If memory pressure is experienced `interval` is ignored.

`pressure_ms` is the target # of milliseconds per event loop tick that the target
cgroup is under `some` memory pressure.

`max_probe` is how aggressively to probe for `pressure_ms` memory pressure.

`max_backoff` is how aggressively to back off after `pressure_ms` memory
pressure is reached.

`coeff_probe` determines when `max_probe` aggression is reached . A coefficient
of 10 means the adjustment curve reaches the limit when pressure is 10x the
target pressure.

`coeff_backoff` determines when `max_backoff` back off is reached. A coefficient
of 10 means the adjustment curve reaches the limit when pressure is 10x the
target pressure.

STOP on success, CONTINUE otherwise.
