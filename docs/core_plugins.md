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

The root host can be encoded as "/".

`resource` is io|memory

CONTINUE if 1m pressure > `threshold` for longer than `duration` && trending
above threshold (10s > `threshold`) && 10s not falling rapidly. STOP
otherwise.

## memory_above

### Arguments

    cgroup
    threshold (optional)
    threshold_anon (optional)
    duration

### Description

`cgroup` has the same semantics and features as `pressure_rising_beyond`.

`threshold` and `threshold_anon` take either an absolute memory amount or a
percentage of total memory used. Either one of these parameters must be
specified. When both are specified, only `threshold_anon` is effective.

An absolute memory amount threshold accepts combinations of K|M|G|T
suffixed components. For example, `1.5M 32K 512` is interpreted as
`1.5 * 2^20 + 32 * 2^10 + 512` bytes. NOTE: FOR BACKWARD COMPATIBILITY, A
BARE NUMBER IS INTERPRETED AS MEGABYTES.

A percentage threshold must be in the format `N%`, where
`0 <= N <= 100`.

If `threshold` is specified, CONTINUE if 10s total memory usage >
`threshold` longer than `duration`, STOP otherwise.

If `threshold_anon` is specified, CONTINUE if 10s anonymous memory
usage > `threshold_anon` longer than `duration`, STOP otherwise.

## pressure_above

### Arguments

    cgroup
    resource
    threshold
    duration
    cgroup_fs=/sys/fs/cgroup (optional)

### Description

`cgroup` has the same semantics and features as `pressure_rising_beyond`.

`resource` is io|memory

CONTINUE if 10s pressure > `threshold` longer than `duration` STOP
otherwise.

## memory_reclaim

### Arguments

    cgroup
    duration

### Description

`cgroup` has the same semantics and features as `pressure_rising_beyond`.

CONTINUE if `cgroup`'s memory has been reclaimed in the past `duration` period.
STOP otherwise.

## swap_free

### Arguments

    threshold_pct

### Description
CONTINUE if percentage of free:total swap drops below `threshold_pct` %.
STOP otherwise.

## exists

### Arguments

    cgroup
    negate=false (optional)

### Description

`cgroup` supports comma separated arguments.

When `negate` is `false`, if `cgroup` exists, CONTINUE. STOP
otherwise.

When `negate` is `true`, if `cgroup` doesn't exist, CONTINUE. STOP
otherwise.

## dump_cgroup_overview

### Arguments

    cgroup
    always=false

### Description
`cgroup` has the same semantics and features as `pressure_rising_beyond`.
However, this detector cannot monitor the root host.

Dumps the system overview for `cgroup` to stderr if memory pressure is
non-negligible.

If `always` is set to `true`, then cgroup overviews will always be printed.

Always returns CONTINUE.

## adjust_cgroup

### Arguments

    cgroup
    memory
    memory_scale=1 (optional)

### Description

`cgroup` has the same format and features as `pressure_rising_beyond`
but specifies the target cgroups directly rather than the parents.

`memory_scale` is the scale factor to be multipled to memory usage of
the cgroup. It can be zero or a positive floating point number. For
example, `1.5` makes the cgroup's memory usage to be scaled up by 50%.

`memory` is the number of bytes to be added to memory usage of the
cgroup. It can be negative, 0 or positive. It accepts combinations of
K|M|G|T suffixed components. For example, `1.5M 32K 512` is
interpreted as 1.5 * 2^20 + 32 * 2^10 + 512 bytes.

When both `memory_scale` and `memory` are specified, `memory_scale` is
applied first.

Adjustments are applied to the shared context at the time when the
plugin is invoked and can be updated by later invocations.

Always returns CONTINUE.


# Actions

## kill_by_memory_size_or_growth

### Arguments

    cgroup
    size_threshold=50 (optional)
    min_growth_ratio=1.25 (optional)
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

Kill the biggest (memory.current - memory.low) child cgroup if larger than
`size_threshold` percent or kill the fastest growing over
`min_growth_ratio` of the biggest `growing_size_percentile` by size.  True
if killed something, which terminates actions. Sleeps for
`post_action_delay` if a kill was performed.  If `dry` is set to true, does
not actually perform kill but prints via logs what kill it would have done.

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

## kill_by_io_cost

### Arguments

    cgroup
    post_action_delay=15 (optional)
    cgroup_fs=/sys/fs/cgroup (optional)
    dry=false (optional)

### Description

`cgroup` follows the same semantics and options as
`kill_by_memory_size_or_growth`.

Sleeps for `post_action_delay` following a kill.

`dry` follows the same semantics and options as
`kill_by_memory_size_or_growth`

Kills the child generating the most io cost.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

STOP if killed something (even if dry=true), CONTINUE otherwise.
