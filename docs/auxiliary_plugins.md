# Auxiliary plugins

Auxiliary plugins are plugins that are either not generic enough to be
considered core or require optional dependencies.

# Actions

## systemd_restart

### Arguments

    service
    post_action_delay=15 (optional)
    dry=false (optional)
    machine_type="" (optional)

### Description

Restarts the systemd service named by `service`. If `machine_type` is set,
oomd connects to that systemd machine and restarts the service there.

STOP on success, CONTINUE otherwise.
