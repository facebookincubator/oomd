# Configuration

## Design principles

oomd is designed to be as flexible and as extensible as possible. To that end,
oomd is configured via a declarative configuration file. The idea is you can
have a set of memory protection rules that are orthogonal and intuitive to
reason about. In a sense it's a lot like iptables chains work (but much better,
I promise).

## Schema

oomd configs have a loosely defined BNF:

    ARG:
    <string>: <string>

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
    "disable-on-drop-in": <bool>,
    "detectors": <bool>,
    "actions": <bool>

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
    [
        NAME,
        DROPIN,
        SILENCE_LOGS,
        POST_ACTION_DELAY,
        PREKILL_HOOK_TIMEOUT,
        CGROUP,
        XATTR_FILTER,
        "detectors": [ [DETECTOR_GROUP[,DETECTOR_GROUP[,...]]] ],
        "actions": [ [ACTION[,ACTION[,...]]] ],
    ]

    ROOT:
    {
        "rulesets": [ RULESET[,RULESET[,...]]  ],
        "prekill_hooks": [ PLUGIN ]
    }

In plain english, the general idea is that each oomd config one or more
RULESETs. Each RULESET has a set of DETECTOR_GROUPs and a set of ACTIONs. Each
DETECTOR_GROUP has a set of DETECTORs. Both DETECTORs and ACTIONs are PLUGIN
types. That means _everything_ is a plugin in oomd. The rules on how a
conforming config is evaluated at runtime are described in the next section.

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

- Every plugin must return CONTINUE, STOP or ASYNC_PAUSE.

  - CONTINUE
    - For DETECTORs, noop chain
    - For ACTIONs, continue executing the current ACTION chain
  - STOP
    - For DETECTORs, evaluate the current DETECTOR_GROUP chain to false
    - For ACTIONs, abort execution of the current ACTION chain
  - ASYNC_PAUSE
    - For DETECTORs, not supported. If used, noop (in other words, CONTINUE)
    - For ACTIONs, pause the action chain until the next event loop tick.

- DETECTOR_GROUPs evaluate true if and only if all DETECTORs in the chain return
  CONTINUE

- For each RULESET, if _any_ DETECTOR_GROUP fires, the associated ACTION chain
  will begin execution

- ACTIONs may take multiple event loop ticks to complete. Returning ASYNC_PAUSE
  allows other RULESETs and all DETECTORs to run concurrently. An ACTION
  returning ASNYC_PAUSE will be run() again on the next tick, allowing it to do
  more work and either re-ASYNC_PAUSE, or STOP or CONTINUE. If it CONTINUEs, the
  ACTION chain will resume executing the subsequent ACTION plugins.

- Each RULESET will trigger once per wildcard match of the cgroup NAME. If an
  XATTR_FILTER is defined, then RULESETs only trigger if the cgroup NAME has an
  xattr name matching XATTR_FILTER.

### Notes

- For each event loop tick, all DETECTORs and DETECTOR_GROUPs will be run. This
  is to allow any detectors implementing sliding windows, if any, to update
  their windows

## Example

This example uses the JSON front end. At time of writing (11/20/18), JSON is the
only supported config front end. The config compiler has been designed with
extensibility in mind as well. It would not be difficult to add another config
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
