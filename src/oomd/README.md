# oomd

Userspace Out-Of-Memory (OOM) daemon. It monitors memory pressure via cgroups
and takes configured protective actions (e.g. killing or throttling processes)
according to a declarative ruleset of detector and action plugins.

This folder holds the daemon itself (`Main.cpp`, `Oomd.*`), its core engine
and plugin framework (`engine/`, `include/`, `dropin/`), configuration files
and examples (`config/`, `etc/`), and usage documentation (`docs/`).
