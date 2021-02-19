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
