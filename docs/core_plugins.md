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

If `threshold` is specified, CONTINUE if total memory usage > `threshold`
longer than `duration`, STOP otherwise.

If `threshold_anon` is specified, CONTINUE if anonymous memory usage >
`threshold_anon` longer than `duration`, STOP otherwise.

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

`cgroup` supports comma separated arguments and wildcards.

When `negate` is `false`, if `cgroup` exists, CONTINUE. STOP
otherwise.

When `negate` is `true`, if `cgroup` doesn't exist, CONTINUE. STOP
otherwise.

## kernel_panic

### Arguments

    No argument

### Description

This plugin triggers kernel panic when executed, which helps capture elusive
memory issues that goes away after OOM kills.

## nr_dying_descendants

### Arguments

    cgroup
    count
    lte=true (optional)
    negate=false (optional)

### Description

`cgroup` supports comma separated arguments and wildcards. The plugin triggers
if any of the globbed cgroups matches the condition.

When `lte` is `true`, if `nr_dying_descendants(cgroup) <= count`, CONTINUE.
STOP otherwise.

When `lte` is `false`, if `nr_dying_descendants(cgroup) > count`, CONTINUE.
STOP otherwise.

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


# Actions

## kill_by_memory_size_or_growth

### Arguments

    cgroup
    recursive=false (optional)
    size_threshold=50 (optional)
    min_growth_ratio=1.25 (optional)
    growing_size_percentile=80 (optional)
    post_action_delay=15 (optional)
    dry=false (optional)
    always_continue=false (optional)
    reap_memory=false (optional)

### Description

`cgroup` specifies the cgroup(s) that should be considered for killing.
Eg. if cgroup=system.slice/*, everything inside system.slice would be
considered for killing. `cgroup` also support multi-cgroup and wildcard
paths. Eg.

    cgroup=workload.slice/workload-*.slice/*,system.slice/*

Note that extra spaces are not permitted betwen ','s.

If `recursive` is set, walk down the cgroup tree looking for the best leaf to
kill. Comparisons happen locally, between siblings, using the kill plugin's
specific heuristics. The cgroups listed in `cgroup` are treated as the initial
set of siblings. If you want a cgroup subtree to be killed all together or not
at all, set its memory.oom.group=1. One might express the
example above using `recursive` as

    cgroup=workload.slice/workload-*.slice/,system.slice/
    recursive=true

Note the lack of trailing "*".

Kill the biggest (memory.current - memory.low) child cgroup if larger than
`size_threshold` percent or kill the fastest growing over
`min_growth_ratio` of the biggest `growing_size_percentile` by size.  True
if killed something, which terminates actions.

Disables its action chain for `post_action_delay` if a kill was performed. Other
rulesets will run normally. This plugins' ruleset's detectors will run, but will
not trigger any actions.

If `dry` is set to true, does not actually perform kill but prints via logs what
kill it would have done.

Avoids killing cgroups which aren't experiencing memory pressure at all as
they aren't contributing the pressure anyway.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

cgroups with the "trusted.oomd_prefer" xattr set will be killed before any other
cgroups, even if others are better choices by the above logic. A cgroup with
"trusted.oomd_avoid" will not be killed unless there are no other cgroups to
kill. If multiple cgroups are "trusted.oomd_prefer"ed, the above logic will be
used to pick between them. If a cgroup has both of these xattrs it is considered
"prefer". The xattrs must be set on cgroups targeted in the `cgroup` arg;
they will have no effect if set on ancestors of the targeted cgroups.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.

If `reap_memory` is set to true, attempt to speed up process memory cleanup via process_mrelease syscall. See https://lwn.net/Articles/864184/ for details.

## kill_by_swap_usage

### Arguments

    cgroup
    recursive=false (optional)
    threshold=1 (optional)
    post_action_delay=15 (optional)
    dry=false (optional)
    always_continue=false (optional)
    reap_memory=false (optional)

### Description

`cgroup` and `recursive` follow the same semantics and options as
`kill_by_memory_size_or_growth`. oomd_prefer/oomd_avoid xattrs are respected
the same way as well.

`threshold` follows the same semantics and options as `memory_above`.

`post_action_delay` and `dry` follow the same semantics and options as
`kill_by_memory_size_or_growth`

Kills the child with the largest swap usage.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.

If `reap_memory` is set to true, attempt to speed up process memory cleanup via process_mrelease syscall. See https://lwn.net/Articles/864184/ for details.

## kill_by_pressure

### Arguments

    cgroup
    recursive=false (optional)
    resource
    post_action_delay=15 (optional)
    dry=false (optional)
    always_continue=false (optional)
    reap_memory=false (optional)

### Description

`cgroup` and `recursive` follow the same semantics and options as
`kill_by_memory_size_or_growth`. oomd_prefer/oomd_avoid xattrs are respected
the same way as well.

`resource` is io|memory

`post_action_delay` and `dry` follow the same semantics and options as
`kill_by_memory_size_or_growth`

Kills the child generating the most pressure.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.

If `reap_memory` is set to true, attempt to speed up process memory cleanup via process_mrelease syscall. See https://lwn.net/Articles/864184/ for details.

## kill_by_io_cost

### Arguments

    cgroup
    recursive=false (optional)
    post_action_delay=15 (optional)
    dry=false (optional)
    always_continue=false (optional)
    reap_memory=false (optional)

### Description

`cgroup` and `recursive` follow the same semantics and options as
`kill_by_memory_size_or_growth`. oomd_prefer/oomd_avoid xattrs are respected
the same way as well.

`post_action_delay` and `dry` follow the same semantics and options as
`kill_by_memory_size_or_growth`

Kills the child generating the most io cost.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.

If `reap_memory` is set to true, attempt to speed up process memory cleanup via process_mrelease syscall. See https://lwn.net/Articles/864184/ for details.

## kill_by_pg_scan

### Arguments

    cgroup
    recursive=false (optional)
    post_action_delay=15 (optional)
    dry=false (optional)
    always_continue=false (optional)
    reap_memory=false (optional)

### Description

`cgroup` and `recursive` follow the same semantics and options as
`kill_by_memory_size_or_growth`. oomd_prefer/oomd_avoid xattrs are respected
the same way as well.

`post_action_delay` and `dry` follow the same semantics and options as
`kill_by_memory_size_or_growth`

Kills the child with the highest pg scan rate.

cgroups that are killed have the "trusted.oomd_kill" xattr set to the number
of SIGKILLs sent to resident processes.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.

If `reap_memory` is set to true, attempt to speed up process memory cleanup via process_mrelease syscall. See https://lwn.net/Articles/864184/ for details.
