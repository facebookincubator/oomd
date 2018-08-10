# oomd

oomd is *userspace* Out-Of-Memory (OOM) killer for linux systems.

## Background

Out of memory killing has historically happened inside kernel space. On a
[memory overcommitted][0] linux system, malloc(2) and friends will never fail.
However, if an application dereferences the returned pointer and the system has
run out of physical memory, the linux kernel is forced take extreme measures,
up to and including killing processes. This is typically a slow and painful
process because the kernel spends an unbounded amount of time swapping in and
out pages and evicting the page cache. Furthermore, [configuring policy][1] is
not very flexible while being somewhat complicated.

oomd aims to solve this problem in userspace. oomd leverages [PSI][6] and
cgroupsv2 to monitor a system holistically. oomd then takes corrective action
in userspace before an OOM occurs in kernel space. Corrective action is
configured via a flexible plugin system, in which custom code can be written.
By default, this involves killing offending processes. This enables an
unparalleled level of flexibility where each workload can have custom
protection rules. Furthermore, time spent churning pages in kernelspace is
minimized. In practice at Facebook, we've regularly seen 30 minute host lockups
go away entirely.

## Building and installing

Note that oomd requires [PSI][6] to function. This kernel feature has not yet
been upstreamed (as of 7/18/18).

oomd currently depends on [meson][2] and [jsoncpp][4].

    $ git clone https://github.com/facebookincubator/oomd
    $ cd oomd
    $ meson build && ninja -C build
    $ cd build && sudo ninja install

## Configuring oomd

oomd receives runtime configuration from two sources: environment vars and a
config file. You typically do not need to change the default environment
values. However, to finely tune oomd, you may want to.

### Config file

Default location: /etc/oomd.json

Example config:

    {
        "cgroups": [
            {
                "target": "system.slice",
                "kill_list": [
                    {"chef.service": { "kill_pressure": "60", "max_usage": "100" } },
                    {"sshd.service": { "max_usage": "inf" } }
                ],
                "oomdetector": "default",
                "oomkiller": "noop"
            },
            {
                "target": "workload.slice",
                "kill_list": [],
                "oomdetector": "default",
                "oomkiller": "default"
            }
        ],
        "version": "0.2.0"
    }

This example config describes the following:

* oomd shall monitor two cgroups: 'system.slice' and 'workload.slice'
* system.slice
    * When an OOM is detected, first look at the kill list and attempt to kill
      chef.service first if it's generating >= 60% of total memory pressure OR
      it's using >= 100MB of memory
    * Never kill 'sshd.service'
    * Use the default oomdetector
    * Use the custom 'noop' killing plugin
* workload.slice
    * Do not specify kill list policy
    * Use the default oomdetector
    * Use the default oomkiller

### Environment variables

`OOMD_INTERVAL`

* How often oomd polls the system
* Unit: seconds (integer)
* Default: 5

`OOMD_VERBOSE_INTERVAL`

* How often oomd logs verbose state to /dev/kmsg
* Unit: seconds (integer)
* Default: 300

`OOMD_POST_KILL_DELAY`

* How long oomd will sleep after performing a corrective action
* Unit: seconds (integer)
* Default: 15

`OOMD_THRESHOLD`

* How sensitive oomd will be to slow growing OOMs
* Unit: none (integer)
* Values: [0, 100], 0 being extremely sensitive and 100 being not sensitive
* Default: 60

`OOMD_HIGH_THRESHOLD`

* How sensitive oomd will be to fast growing OOMs
* Unit: none (integer)
* Values: [0, 100], 0 being extremely sensitive and 100 being not sensitive
* Default: 80

`OOMD_HIGH_THRESHOLD_DURATION`

* How quick oomd will be to declare a fast growing OOM
* Unit: seconds (integer)
* Default: 10

`OOMD_LARGER_THAN`

* When killing via size heuristic, only kill targets using greater than X%
  of total used memory
* Unit: percent (integer)
* Values: [0, 100]
* Default: 50

`OOMD_GROWTH_ABOVE`

* When killing via growth heuristic, only consider P(X) top growers
* Unit: percent (integer)
* Values: [0, 100]
* Default: 80

`OOMD_MIN_SWAP_PCT`

* When swap is enabled, perform corrective actions when X% of swap is
  remaining
* Unit: percent (int)
* Default: 15

## Tests

oomd depends on [gtest/gmock][5] to run tests. Installing gtest/gmock from master
is preferred.

If meson detects gtest/gmock is installed, meson will generate build rules for tests.

    $ cd oomd
    $ rm -rf build
    $ meson build && ninja test -C build

## License

oomd is GPL 2 licensed, as found in the LICENSE file.


[0]: https://www.kernel.org/doc/Documentation/vm/overcommit-accounting
[1]: https://lwn.net/Articles/317814/
[2]: http://mesonbuild.com/
[4]: https://github.com/open-source-parsers/jsoncpp
[5]: https://github.com/google/googletest
[6]: http://git.cmpxchg.org/cgit.cgi/linux-psi.git/
