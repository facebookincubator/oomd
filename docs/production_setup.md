# Production setup

This document describes a production setup for oomd.

## Host setup

### Kernel features

oomd requires a cgroup v2 mount and a memory-pressure runtime interface. It
uses `/proc/pressure/memory` when that PSI interface is available. It also
accepts the legacy `/proc/mempressure` interface. Older systems can require
this kernel command-line option to select the unified cgroup hierarchy:

* `systemd.unified_cgroup_hierarchy=1`

If the configuration reads per-cgroup swap usage, the kernel must also provide
cgroup swap accounting. Older kernels can require `swapaccount=1` to enable
it.

### cgroup2

The host must be running unified cgroup (cgroup2) alone. oomd is not designed
to support mixed (legacy and unified) hierarchies. oomd expects the cgroup2
filesystem to be mounted at `/sys/fs/cgroup` but the default can be changed
with `--cgroup-fs`.

### systemd

The provided service unit uses systemd, but the oomd executable does not
require systemd. On a systemd host, enable resource accounting for all units
that oomd monitors. You can enable it in the system defaults:

```
DefaultCPUAccounting=true
DefaultIOAccounting=true
DefaultMemoryAccounting=true
DefaultTasksAccounting=true
```

See the [systemd system configuration documentation][systemd-accounting] for
more details.

### PSI (Pressure Stall Information)

oomd uses the memory PSI interface when it is available. Verify that interface:

```
$ cat /proc/pressure/memory
some avg10=0.00 avg60=0.00 avg300=0.00 total=0
full avg10=0.00 avg60=0.00 avg300=0.00 total=0
```

On older supported kernels, verify that `/proc/mempressure` is available
instead.

### swap

Enable swap when the configuration uses swap-based detectors, actions, or
swap-based Senpai features. Swap can also give oomd more time to act before the
system runs out of memory.

See [In defence of swap](https://chrisdown.name/2018/01/02/in-defence-of-swap.html)
for more information.

Select the swap size for the workload and its recovery target.

## Service setup

Run oomd in a protected cgroup. This protection helps oomd operate when the
host has little available memory.

Group host-critical services in a slice such as `hostcritical.slice`. Put oomd
in this slice with other critical services such as `sshd.service`. Use
`MemoryMin=` on `oomd.service` to protect oomd. Configure the slice separately
if all services in the slice need protection. A 64 MiB allocation for oomd is
a reasonable starting point.

For example, add these settings to the service:

```
[Service]
Slice=hostcritical.slice
MemoryMin=64M
MemoryLow=64M
```

Then verify the service cgroup:

```
$ systemctl show oomd.service --property ControlGroup
ControlGroup=/hostcritical.slice/oomd.service
```

TODO: document io.latency config

## Monitoring

oomd provides structured statistics through a Unix socket. Use
`oomd --dump-stats` to read them and `oomd --reset-stats` to reset them. See
[stats.md](stats.md) for the API and runtime-directory details.

[systemd-accounting]: https://www.freedesktop.org/software/systemd/man/systemd-system.conf.html#DefaultCPUAccounting=
