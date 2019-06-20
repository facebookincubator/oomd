# Production setup

The document covers how oomd is set up in production at Facebook.

## Host setup

### cgroup2

The host must be running unified cgroup (cgroup2) alone. oomd is not designed
to support mixed (legacy and unified) hierarchies. oomd expects the cgroup2
filesystem to be mounted at `/sys/fs/cgroup` but the default can be changed
with `--cgroup-fs`.

### systemd

The host must be managed by systemd. Furthermore, resource accounting must be
turned on for all units monitored by oomd. The easiest way to turn on resource
accounting is by changing the system defaults:

```
DefaultCPUAccounting=true
DefaultIOAccounting=true
DefaultMemoryAccounting=true
DefaultTasksAccounting=true
```

Refer to https://www.freedesktop.org/software/systemd/man/systemd-system.conf.html#DefaultCPUAccounting=
for more details.

### kernel w/ PSI (pressure stall information)

oomd requires PSI to function. Kernels 4.20 and above should have PSI.
Verify your kernel has been compiled with PSI by running:

```
$  zcat /proc/config.gz | grep CONFIG_PSI
CONFIG_PSI=y
```

## Service setup

oomd must be run in a protected cgroup. In other words, we use a specialized
systemd service setup to guarantee oomd resources so that oomd can act in
resource starved scenarios.

You typically group host critical services in their own special cgroup.
Perhaps named `hostcritical.slice`. oomd should be grouped in here with
other host critical services like `sshd.service`. `hostcritical.slice` should
be guaranteed a minimum amount of reserved memory via `memory.min`. This
essentially `mlockall`s oomd except the kernel sets aside the memory ahead
of time. A portion of that memory - 64M should be enough - should be given to
oomd.

The resulting config should look something like:

```
$ systemctl cat oomd.service | grep Memory
MemoryMin=64M
MemoryLow=64M

$ systemctl show fb-oomd.service | grep ControlGroup
ControlGroup=/hostcritical.slice/oomd.service
```

TODO: document io.latency config

## Monitoring

Stats are currently collected by grep'ing through oomd logs files. This is
obviously very brittle but work is underway to provide a structured stats
interface.

TODO: document the WIP structured stats collection interface
