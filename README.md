# oomd

[![Build Status](https://github.com/facebookincubator/oomd/workflows/CI/badge.svg?branch=master)](https://github.com/facebookincubator/oomd/actions?query=workflow%3ACI+branch%3Amaster)

oomd is *userspace* Out-Of-Memory (OOM) killer for linux systems.

## Background

Out of memory killing has historically happened inside kernel space. On a
[memory overcommitted][0] linux system, malloc(2) and friends usually never
fail. However, if an application dereferences the returned pointer and the
system has run out of physical memory, the linux kernel is forced to take extreme
measures, up to and including killing processes. This is sometimes a slow and
painful process because the kernel can spend an unbounded amount of time
swapping in and out pages and evicting the page cache. Furthermore,
[configuring policy][1] is not very flexible while being somewhat complicated.

oomd aims to solve this problem in userspace. oomd leverages PSI and cgroupv2
to monitor a system holistically. oomd then takes corrective action in
userspace before an OOM occurs in kernel space. Corrective action is configured
via a flexible plugin system, in which custom code can be written. By default,
this involves killing offending processes. This enables an unparalleled level
of flexibility where each workload can have custom protection rules.
Furthermore, time spent livelocked in kernelspace is minimized. In practice at
Facebook, we've regularly seen 30 minute host lockups go away entirely.

## Installing on Debian 11+ or Ubuntu 20.04+

`# apt install oomd`

## Installing from RPMs on Fedora

oomd is packaged in Fedora as of Fedora 32 and can be installed with:

    $ sudo dnf install oomd

Finally, enable and start it with:

    $ sudo systemctl enable --now oomd.service

## Building from source

Note that oomd requires PSI to function. This kernel feature has been merged
into the 4.20 release.

oomd currently depends on [meson][2] and [jsoncpp][4]. [libsystemd][6] is an
optional dependency.

oomd also requires GCC 8+ or clang 6+. Other compilers have not been tested.

    $ git clone https://github.com/facebookincubator/oomd
    $ cd oomd/oomd
    $ meson build && ninja -C build
    $ cd build && sudo ninja install

## Configuration

See [docs/configuration.md](docs/configuration.md) for a high level overview
and some examples.

See [docs/core_plugins.md](docs/core_plugins.md) for a quick reference on
core plugin capabilities.

See [docs/production_setup.md](docs/production_setup.md) for guidelines on
how oomd should be set up in a production environment.

## Running tests

oomd depends on [gtest/gmock][5] to run tests. Installing gtest/gmock from master
is preferred.

If meson detects gtest/gmock is installed, meson will generate build rules for tests.

    $ cd oomd
    $ rm -rf build
    $ meson build && ninja test -C build

## Writing custom plugins

It is both possible and encouraged to write custom plugins. The codebase is designed
to make writing plugins as easy as possible.

See [docs/writing_a_plugin.md](docs/writing_a_plugin.md) for a tutorial.

## Help / Discussion / Support

Join our **#oomd** channel on irc.freenode.net!


## License

oomd is GPL 2 licensed, as found in the [LICENSE](LICENSE) file.


[0]: https://www.kernel.org/doc/Documentation/vm/overcommit-accounting
[1]: https://lwn.net/Articles/317814/
[2]: http://mesonbuild.com/
[4]: https://github.com/open-source-parsers/jsoncpp
[5]: https://github.com/google/googletest
[6]: https://github.com/systemd/systemd/tree/master/src/libsystemd/
