# Drop-in configs

Drop-in configs are a way to modify oomd's rule engine at runtime. For details
on regular (non-drop-in) configuration, please see
[configuration.md](configuration.md).

## Background

Drop-in configs are useful in environments hosting container or container-like
environments. For example, consider the following cgroup hierarchy:

```
/
├── system.slice
│   └── chef.service
└── workload.slice
    └── workload-container
        ├── sidecar
        └── task
            ├── nested-containers
            │   ├── container1
            │   ├── container2
            │   └── container3
            └── nested-containers-sidecar
```

oomd configs will usually set `task` as the kill target. This is correct and
optimal most of the time. However, in cases where containers are nested, eg.
when a container hosts its own container-like environment, this becomes
suboptimal.

In our example, `task` holds its own set of nested containers, `container1`,
`container2`, and `container3`. With the standard oomd config, all
three nested containers will be killed if any of the three OOM. What would be
nice is if `task` could tell oomd to kill one of the three nested containers.
Then also if the container is migrated off the host, the modification is
removed. This is what drop-in configs accomplish.

## Configuration

### Base config

You must make base config modifications to enable drop-in configs.
`disable-on-drop-in` disables execution of the targeted base ruleset if a
drop-in config is added. `detectors` and `actions` enable drop-in configs for
detector groups and the action chain, respectively. By default all options are
off.

### Drop-in config

Drop-in configs use the exact same format as base configs with the following
caveat:

* Both detector groups and actions may be omitted

## Usage

Run oomd with `--drop-in-dir DIR` to have oomd monitor `DIR` for drop-in configs.
Whenever a file is modified-in or moved-into `DIR`, oomd will try to parse the file
as an oomd config. If it parses and compiles, oomd will try to inject the
drop-in config into the engine. Filenames beginning with '.' are ignored.

### Mechanism

* Every drop-in config must target a ruleset in the base config
  * Targeting is done with the ruleset `name`
  * If there is more than one match, the first base ruleset will be chosen
  * If more than one drop-in config is present, they are added last-in-first-out
    (LIFO) order
  * A single drop-in config may contain more than one ruleset
  * Drop-in configs override the targeted ruleset on detector group or action
    group granularity

* Behind the scenes, oomd will clone a copy of the targeted ruleset and
  replace the dropped-in detector groups and action groups. That new ruleset
  will be run before the targeted ruleset.

## Example

### Base config

```
{
    "rulesets": [
        {
            "name": "user session protection",
            "drop-in": {
              "detectors": true,
              "actions": true,
              "disable-on-drop-in": true
            },
            "detectors": [
                [
                    "user pressure above 60 for 30s",
                    {
                        "name": "dump_cgroup_overview",
                        "args": {
                            "cgroup": "user.slice,workload.slice,www.slice"
                        }
                    },
                    {
                        "name": "pressure_above",
                        "args": {
                            "cgroup": "user.slice,workload.slice,www.slice",
                            "resource": "memory",
                            "threshold": "60",
                            "duration": "30"
                        }
                    },
                    {
                        "name": "memory_reclaim",
                        "args": {
                            "cgroup": "user.slice,workload.slice,www.slice",
                            "duration": "10"
                        }
                    }
                ]
            ],
            "actions": [
                {
                    "name": "continue"
                }
            ]
        }
    ]
}
```

### Drop-in config

```
{
    "rulesets": [
        {
            "name": "user session protection",
            "detectors": [
                [
                    "system pressure above 80 for 60s",
                    {
                        "name": "dump_cgroup_overview",
                        "args": {
                            "cgroup": "system.slice",
                            "always": true
                        }
                    }
                ]
            ]
        }
    ]
}
```
