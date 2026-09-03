# Core plugins

This document groups the plugins as control-flow plugins, detectors, and
actions. The runtime does not enforce the detector and action groups. A
configuration can use a detector as an action or an action as a detector, but
the result is not always useful.

# Control-flow plugins

## continue

### Arguments

    No arguments

### Description

Always returns CONTINUE. It can be used in a detector group or an action chain.

## stop

### Arguments

    No arguments

### Description

Always returns STOP. It can be used in a detector group or an action chain.

# Detectors

## pressure_rising_beyond

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    resource
    threshold
    duration
    fast_fall_ratio=0.85 (optional)

### Description

`cgroup` specifies the parent cgroups to monitor. For example,
`cgroup=system.slice` monitors the cgroups in `system.slice`. `cgroup` supports
comma-separated patterns and wildcard paths. For example:

    cgroup=workload.slice/workload-*.slice,system.slice

Do not put spaces before or after a comma.

The root host can be encoded as "/".

`ruleset_cgroup` is similar to `cgroup` but is relative to the OomdContext's
ruleset cgroup value, if provided. See [`Ruleset Cgroup`](ruleset_cgroup.md) for
details.

`resource` is `io` or `memory`.

CONTINUE if 1m pressure > `threshold` for longer than `duration` && trending
above threshold (10s > `threshold`) && 10s not falling rapidly. STOP
otherwise.

## memory_above

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    threshold (optional)
    threshold_anon (optional)
    duration
    debug=false (optional)

### Description

`cgroup` and `ruleset_cgroup` have the same semantics and features as
`pressure_rising_beyond`.

`threshold` and `threshold_anon` take either an absolute memory amount or a
percentage of total host memory. Either one of these parameters must be
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

`debug=true` logs the sampled memory values and threshold state.

## pressure_above

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    resource
    threshold
    duration

### Description

`cgroup` and `ruleset_cgroup` have the same semantics and features as
`pressure_rising_beyond`.

`resource` is `io` or `memory`.

CONTINUE if 10s pressure > `threshold` longer than `duration` STOP
otherwise.

## memory_reclaim

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    duration

### Description

`cgroup` and `ruleset_cgroup` have the same semantics and features as `pressure_rising_beyond`.

CONTINUE if `cgroup`'s memory has been reclaimed in the past `duration` period.
STOP otherwise.

## swap_above_memory_max

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    threshold_pct
    debug=false (optional)

### Description

`cgroup` and `ruleset_cgroup` have the same semantics and features as
`pressure_rising_beyond`.
`threshold_pct` must be a non-negative integer no greater than `INT_MAX`.

For each cgroup, compare `memory.swap.current` with that same cgroup's live
`memory.max`. CONTINUE if at least one finite, positive `memory.max` has swap
strictly greater than `threshold_pct` percent of the limit. STOP otherwise.
Missing, non-positive, and unlimited limits do not match. `debug=true` logs
the sampled operands and a reason for each decision.

## swap_free

### Arguments

    threshold_pct
    swapout_bps_threshold=0 (optional)

### Description
CONTINUE if the percentage of free swap is below `threshold_pct` and the
current swap-out rate is at least `swapout_bps_threshold`. STOP otherwise.

## xattr_glob

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    xattr
    allowlist
    denylist (optional)
    debug=false (optional)

### Description

`cgroup` and `ruleset_cgroup` have the same semantics and features as
`pressure_rising_beyond`.
`xattr` is the cgroup extended attribute to read.
`allowlist` and `denylist` are comma-separated glob pattern lists. Empty
patterns are not valid. Commas are separators and cannot be escaped.
Whitespace is not removed. Each space is part of the pattern.
Patterns use `fnmatch(3)` syntax with no flags. Each pattern is matched against
the complete raw xattr value. Slash characters have no special meaning.
The xattr name must contain 1 to 255 bytes. The xattr value and patterns must
not contain a NUL byte. The two lists can contain at most 256 patterns in
total. Each pattern can contain at most 512 bytes, and all patterns can contain
at most 32 KiB in total. Each comma-separated list can contain at most 64 KiB.

STOP if no target cgroup is selected, if the xattr is missing, if xattr read
fails, if an xattr value is invalid, if an xattr value matches `denylist`, or
if no xattr value matches `allowlist`. An empty `allowlist` denies all values.
CONTINUE if at least one xattr value matches `allowlist` and no xattr value
matches `denylist`.
`debug=true` logs the result for each evaluated cgroup.

## exists

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    negate=false (optional)

### Description

`cgroup` and `ruleset_cgroup` have the same semantics and features as
`pressure_rising_beyond`.

When `negate=false`, return CONTINUE if any configured `cgroup` or
`ruleset_cgroup` pattern resolves to a cgroup. Return STOP otherwise.

When `negate=true`, return CONTINUE if no configured pattern resolves to a
cgroup. Return STOP otherwise.

## kernel_panic

### Arguments

    No arguments

### Description

This plugin triggers a kernel panic. It can help capture memory problems that
disappear after an OOM kill.

Its source is exported for opt-in builds, but the default Meson binary does
not compile or register it. Enabling it in a downstream binary requires a
separate safety review.

## nr_dying_descendants

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    count
    lte=true (optional)
    debug=false (optional)

### Description

`cgroup` and `ruleset_cgroup` have the same semantics and features as
`pressure_rising_beyond`.

`debug=true` logs the matching count.

When `lte` is `true`, if `nr_dying_descendants(cgroup) <= count`, CONTINUE.
STOP otherwise.

When `lte` is `false`, if `nr_dying_descendants(cgroup) > count`, CONTINUE.
STOP otherwise.

## dump_cgroup_overview

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    always=false

### Description
`cgroup` and `ruleset_cgroup` have the same semantics and features as
`pressure_rising_beyond`.

Dumps the system overview for `cgroup` to stderr if memory pressure is
non-negligible.

If `always` is set to `true`, then cgroup overviews will always be printed.

Always returns CONTINUE.


# Actions

## kill_by_memory_size_or_growth

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    recursive=false (optional)
    size_threshold=50 (optional)
    min_growth_ratio=1.25 (optional)
    growing_size_percentile=80 (optional)
    post_action_delay (optional ruleset-delay override)
    dry=false (optional)
    always_continue=false (optional)
    debug=false (optional)
    kernelkill=false (optional)
    reap_memory=true (optional)
    log_kmemalloc_prekill=false (optional)

### Description

`cgroup` specifies the cgroups that oomd considers for killing. For example,
`cgroup=system.slice/*` selects all matching cgroups in `system.slice`.
`cgroup` supports comma-separated patterns and wildcard paths. For example:

    cgroup=workload.slice/workload-*.slice/*,system.slice/*

Do not put spaces before or after a comma.

`ruleset_cgroup` is similar to `cgroup` but is relative to the OomdContext's
ruleset cgroup value, if provided. See [`Ruleset Cgroup`](ruleset_cgroup.md) for
details.

If `recursive` is set, walk down the cgroup tree looking for the best leaf to
kill. Comparisons happen locally, between siblings, using the kill plugin's
specific heuristics. The cgroups listed in `cgroup` are treated as the initial
set of siblings. If you want a cgroup subtree to be killed all together or not
at all, set its memory.oom.group=1. One might express the
example above using `recursive` as

    cgroup=workload.slice/workload-*.slice/,system.slice/
    recursive=true

Note the lack of trailing "*".

The plugin ranks candidates in three phases. It first ranks cgroups whose
current usage is at least `size_threshold` percent of total sibling usage. It
then ranks cgroups that are at or above the configured
`growing_size_percentile` cutoff and whose current-to-average usage ratio is at
least `min_growth_ratio`. It finally ranks all remaining cgroups. Size ranking
uses effective usage, which is current usage minus normalized memory
protection.

After a successful kill, the plugin returns STOP and pauses its action chain.
`post_action_delay` overrides the ruleset delay. If the argument is absent, the
plugin uses the ruleset delay, whose default is 15 seconds. Other rulesets
continue to run. If `always_continue=true`, the plugin returns CONTINUE and
does not apply this delay.

If `dry=true`, the plugin does not send a kill. It logs the target that it would
have killed.

Cgroups that are killed have their `trusted.oomd_kill` and `user.oomd_kill`
xattrs incremented by the reported number of killed processes. Kernel kill mode
can report an estimated number.

Cgroups with `trusted.oomd_prefer` or `user.oomd_prefer` are ranked before
other cgroups. Cgroups with `trusted.oomd_avoid` or `user.oomd_avoid` are ranked
last. A prefer attribute takes precedence over an avoid attribute. oomd checks
the attributes on each candidate at each ranking level. It does not inherit an
attribute from an ancestor.

STOP after a successful kill, or after a selection in dry mode, unless
`always_continue=true`. CONTINUE otherwise.

`debug=true` logs more candidate details. `kernelkill=true` freezes the target
and uses `cgroup.kill` instead of sending signals to each process.
`reap_memory=true` tries to release process memory with `process_mrelease`.
See [process_mrelease](https://lwn.net/Articles/864184/) for details.
`log_kmemalloc_prekill=true` logs `/tmp/oomd_kmemalloc_profiler` after a
successful kill or dry-mode selection.

## kill_by_swap_usage

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    recursive=false (optional)
    threshold=1 (optional)
    post_action_delay (optional ruleset-delay override)
    dry=false (optional)
    always_continue=false (optional)
    debug=false (optional)
    kernelkill=false (optional)
    reap_memory=true (optional)
    log_kmemalloc_prekill=false (optional)
    biased_swap_kill=false (optional)

### Description

The common `BaseKillPlugin` arguments follow the same semantics as
`kill_by_memory_size_or_growth`. The kill preference xattrs also have the same
effect.

`threshold` accepts a byte size or a percentage of total host swap. By default,
the plugin ranks children by total swap use. `biased_swap_kill=true` ranks them
by swap use above the swap share implied by their normalized memory protection.

Cgroups that are killed have their `trusted.oomd_kill` and `user.oomd_kill`
xattrs incremented by the reported number of killed processes.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.

## kill_by_pressure

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    recursive=false (optional)
    resource
    post_action_delay (optional ruleset-delay override)
    dry=false (optional)
    always_continue=false (optional)
    debug=false (optional)
    kernelkill=false (optional)
    reap_memory=true (optional)
    log_kmemalloc_prekill=false (optional)

### Description

The common `BaseKillPlugin` arguments follow the same semantics as
`kill_by_memory_size_or_growth`. The kill preference xattrs also have the same
effect.

`resource` is `io` or `memory`.

Kills the child generating the most pressure.

Cgroups that are killed have their `trusted.oomd_kill` and `user.oomd_kill`
xattrs incremented by the reported number of killed processes.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.

## kill_by_io_cost

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    recursive=false (optional)
    post_action_delay (optional ruleset-delay override)
    dry=false (optional)
    always_continue=false (optional)
    debug=false (optional)
    kernelkill=false (optional)
    reap_memory=true (optional)
    log_kmemalloc_prekill=false (optional)

### Description

The common `BaseKillPlugin` arguments follow the same semantics as
`kill_by_memory_size_or_growth`. The kill preference xattrs also have the same
effect.

Kills the child generating the most io cost.

Cgroups that are killed have their `trusted.oomd_kill` and `user.oomd_kill`
xattrs incremented by the reported number of killed processes.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.

## kill_by_pg_scan

### Arguments

    cgroup (optional)
    ruleset_cgroup (optional)
    recursive=false (optional)
    post_action_delay (optional ruleset-delay override)
    dry=false (optional)
    always_continue=false (optional)
    debug=false (optional)
    kernelkill=false (optional)
    reap_memory=true (optional)
    log_kmemalloc_prekill=false (optional)

### Description

The common `BaseKillPlugin` arguments follow the same semantics as
`kill_by_memory_size_or_growth`. The kill preference xattrs also have the same
effect.

The first call collects a page-scan sample and returns `ASYNC_PAUSED`. The next
call ranks cgroups by the increase in page scans over one event-loop tick. It
only considers cgroups with a positive increase.

Cgroups that are killed have their `trusted.oomd_kill` and `user.oomd_kill`
xattrs incremented by the reported number of killed processes.

STOP if killed something (even if dry=true), unless `always_continue`. CONTINUE
otherwise.
