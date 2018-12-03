# Core plugins

Note that this document is organized in two sections: detectors and actions.
In theory, there's nothing stopping you from using a detector as an action
and an action as a detector. The oomd runtime is completely agnostic. However,
it would probably not be very useful in many cases.

# Detectors

## pressure_rising_beyond

### Arguments

    cgroup
    resource
    threshold
    duration
    fast_fall_ratio=0.85 (optional)
    cgroup_fs=/sys/fs/cgroup (optional)

### Description

`cgroup` specifies the "parent" cgroup(s) to monitor. Eg. if
cgroup=system.slice, we would be monitoring everything inside system.slice.
`cgroup` supports multi-cgroup and wildcard paths. Eg:

    cgroup=workload.slice/workload-*.slice,system.slice

Note that extra spaces are not permitted betwen ','s.

`resource` is io|memory

CONTINUE if 1m pressure > `threshold` for longer than `duration` && trending
above threshold (10s > `threshold`) && 10s not falling rapidly. STOP
otherwise.

## pressure_above

### Arguments

    cgroup
    resource
    threshold
    duration

### Description

`cgroup` has the same semantics and features as `pressure_rising_beyond`.

`resource` is io|memory

CONTINUE if 10s pressure > `threshold` longer than `duration` STOP
otherwise.

## memory_reclaim

### Arguments

    duration

### Description

CONTINUE if memory has been reclaimed in the past `duration` period.
STOP otherwise.

## swap_free

### Arguments

    threshold_pct

### Description
CONTINUE if percentage of free:total swap drops below `threshold_pct` %.
STOP otherwise.

## dump_cgroup_overview

### Arguments

    cgroup
    always=false

### Description
`cgroup` has the same semantics and features as `pressure_rising_beyond`.

Dumps the system overview for `cgroup` to stderr if memory pressure is
non-negligible.

If `always` is set to `true`, then cgroup overviews will always be printed.

Always returns CONTINUE.


# Actions

## kill_by_memory_size_or_growth

### Arguments

    cgroup
    size_threshold=50 (optional)
    growing_size_percentile=80 (optional)
    post_action_delay=15 (optional)
    cgroup_fs=/sys/fs/cgroup (optional)
    dry=false (optional)

### Description

`cgroup` specifies the cgroup(s) that should be considered for killing.
Eg. if cgroup=system.slice/*, everything inside system.slice would be
considered for killing. `cgroup` also support multi-cgroup and wildcard
paths. Eg.

    cgroup=workload.slice/workload-*.slice/*,system.slice/*

Note that extra spaces are not permitted betwen ','s.

Kill the biggest (memory.current - memory.low) child cgroup if larger
than `size_threshold` or kill the fastest growing of the biggest
`growing_size_percentile` by size.  True if killed something, which
terminates actions. Sleeps for `post_action_delay` if a kill was performed.
If `dry` is set to true, does not actually perform kill but prints via
logs what kill it would have done.

Avoids killing cgroups which aren't experiencing memory pressure at all as
they aren't contributing the pressure anyway.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

STOP if killed something (even if dry=true). CONTINUE otherwise.

## kill_by_swap_usage

### Arguments

    cgroup
    post_action_delay=15 (optional)
    dry=false (optional)

### Description

`cgroup` follows the same semantics and options as
`kill_by_memory_size_or_growth`.

Sleeps for `post_action_delay` following a kill.

`dry` follows the same semantics and options as
`kill_by_memory_size_or_growth`

Note that there is no `cgroup_fs` option. This plugin does not inspect
cgroup states beyond what the core runtime provides. The core runtime
knows about cgroup_fs location via the cmd line args.

Kills the child with the largest swap usage.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

STOP if killed something (even if dry=true), CONTINUE otherwise.

## kill_by_pressure

### Arguments

    cgroup
    resource
    post_action_delay=15 (optional)
    cgroup_fs=/sys/fs/cgroup (optional)
    dry=false (optional)

### Description

`cgroup` follows the same semantics and options as
`kill_by_memory_size_or_growth`.

`resource` is io|memory

Sleeps for `post_action_delay` following a kill.

`dry` follows the same semantics and options as
`kill_by_memory_size_or_growth`

Kills the child generating the most pressure.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

STOP if killed something (even if dry=true), CONTINUE otherwise.
