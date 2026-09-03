# Configuration

## Design principles

oomd uses a declarative configuration file. A configuration can contain
independent memory protection rules that run in ordered detector and action
chains.

## Schema

oomd configs have a loosely defined BNF:

    ARG_VALUE:
    <string> | <number> | <bool>

    ARG:
    <string>: ARG_VALUE

    NAME:
    <string>

    PLUGIN:
    {
      "name": NAME,
      "args": {
        ARG[,ARG[,...]]
      }
    }

    DETECTOR:
    PLUGIN

    DETECTOR_GROUP:
    [ NAME, DETECTOR[,DETECTOR[,...]] ]

    ACTION:
    PLUGIN

    DROPIN:
    "drop-in": {
      "disable-on-drop-in": <bool>,
      "detectors": <bool>,
      "actions": <bool>
    }

    SILENCE_LOGS:
    "silence-logs": NAME[,NAME[,...]]

    POST_ACTION_DELAY:
    "post_action_delay": <int>

    PREKILL_HOOK_TIMEOUT:
    "prekill_hook_timeout": <int>

    CGROUP:
    "cgroup": NAME,

    XATTR_FILTER:
    "xattr_filter": NAME,

    RULESET:
    {
        "name": NAME,
        DROPIN,
        SILENCE_LOGS,
        POST_ACTION_DELAY,
        PREKILL_HOOK_TIMEOUT,
        CGROUP,
        XATTR_FILTER,
        "detectors": [ DETECTOR_GROUP[,DETECTOR_GROUP[,...]] ],
        "actions": [ ACTION[,ACTION[,...]] ]
    }

    ROOT:
    {
        "rulesets": [ RULESET[,RULESET[,...]]  ] (optional),
        "prekill_hooks": [ PLUGIN[,PLUGIN[,...]] ] (optional)
    }

An oomd configuration can contain RULESETs and prekill hooks. Each RULESET has
a set of DETECTOR_GROUPs and a set of ACTIONs. Each DETECTOR_GROUP has a set of
DETECTORs. DETECTORs and ACTIONs are PLUGIN types. The next section describes
how oomd evaluates a valid configuration.

See [prekill_hooks.md](prekill_hooks.md) for details of the experimental
"prekill_hooks" feature.

See [ruleset_cgroup.md](ruleset_cgroup.md) for details of the "ruleset_cgroup"
feature.

### Notes

- For `SILENCE_LOGS`, the currently supported log entities are
  - `engine`: oomd engine logs
  - `plugins`: logs written by plugins
- `post_action_delay` may be overridden by an action plugin's arg of the same
  name. After an ACTION returns STOP, the ruleset is paused for
  post_action_delay seconds.

## Runtime evaluation rules

- Each plugin `run()` method must return `CONTINUE`, `STOP`, or
  `ASYNC_PAUSED`.

  - `CONTINUE`
    - For DETECTORs, continue the current DETECTOR_GROUP chain
    - For ACTIONs, continue executing the current ACTION chain
  - `STOP`
    - For DETECTORs, evaluate the current DETECTOR_GROUP chain to false
    - For ACTIONs, abort execution of the current ACTION chain
  - `ASYNC_PAUSED`
    - For DETECTORs, treat it as `CONTINUE`
    - For ACTIONs, pause the action chain until the next event loop tick.

- DETECTOR_GROUPs evaluate to true if no DETECTOR returns `STOP`.

- For each RULESET, if _any_ DETECTOR_GROUP fires, the associated ACTION chain
  will begin execution

- ACTIONs may take multiple event loop ticks to complete. Returning
  `ASYNC_PAUSED` allows other RULESETs and all DETECTORs to run. An ACTION
  that returns `ASYNC_PAUSED` runs again on the next tick. It can return
  `ASYNC_PAUSED` again, or it can return `STOP` or `CONTINUE`. If it returns
  `CONTINUE`, the ACTION chain resumes with the next ACTION plugin.

- A RULESET with a cgroup NAME creates one ruleset instance for each wildcard
  match. The detectors in each instance decide whether its action chain starts.
  If an XATTR_FILTER is defined, oomd creates an instance only when the matched
  cgroup has an extended attribute with that exact name. A RULESET without a
  cgroup NAME runs once and does not use XATTR_FILTER.

### Notes

- For each event loop tick, all DETECTORs and DETECTOR_GROUPs will be run. This
  is to allow any detectors implementing sliding windows, if any, to update
  their windows

## Example

This example uses the JSON front end. JSON is the only supported configuration
front end.

    {
        "rulesets": [
            {
                "name": "memory pressure protection",
                "detectors": [
                    [
                        "workload is under pressure and system is under a lot of pressure",
                        {
                            "name": "pressure_rising_beyond",
                            "args": {
                              "cgroup": "workload.slice",
                              "resource": "memory",
                              "threshold": "5",
                              "duration": "15"
                            }
                        },
                        {
                            "name": "pressure_rising_beyond",
                            "args": {
                              "cgroup": "system.slice",
                              "resource": "memory",
                              "threshold": "40",
                              "duration": "15"
                            }
                        }
                    ],
                    [
                        "system is under a lot of pressure",
                        {
                            "name": "pressure_rising_beyond",
                            "args": {
                              "cgroup": "system.slice",
                              "resource": "memory",
                              "threshold": "80",
                              "duration": "30"
                            }
                        }
                    ]
                ],
                "actions": [
                    {
                        "name": "kill_by_memory_size_or_growth",
                        "args": {
                          "cgroup": "system.slice/*"
                        }
                    }
                ]
            },
            {
                "name": "low swap protection",
                "detectors": [
                    [
                        "swap is running low",
                        {
                            "name": "swap_free",
                            "args": {
                              "threshold_pct": "15"
                            }
                        }
                    ]
                ],
                "actions": [
                    {
                        "name": "kill_by_swap_usage",
                        "args": {
                          "cgroup": "system.slice/*,workload.slice/workload-wdb.slice/*,workload.slice/workload-tw.slice/*"
                        }
                    }
                ]
            }
        ]
    }

This config, in english, says the following:

- If the workload is under a memory pressure AND the system is under a moderate
  amount of pressure, kill a memory hog in the system

- If the systems is under a lot of memory pressure, kill a memory hog in the
  system

- If the system is running low on swap (this can cause pathological conditions),
  kill the cgroup using the most swap across the system and workloads.
