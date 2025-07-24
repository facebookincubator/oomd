# Ruleset Cgroup

Ruleset-level cgroup settings are used to write generic configs that apply to
every matching cgroup.

## Background

Oomd users may want to run detectors and actions on every matching cgroup
without the need to dynamically generate cgroup configs. Users may also want to
write sets of actions and detectors that target the particular cgroup slice that
triggerred an action rather than broadly applying a rule.

For example, a cgroup slice may contain four dynamically generated sub-slices.
We want to write a config that applies to each of those subslices individually,
such that detections on one child slice does not lead to an action on another
child slice.

## Configuration

Ruleset Cgroup targeting is configured in a top-level "cgroup" key and
"xattr_filter" key as a child of "rulesets".

For example:

```
  {
      "rulesets": [
        ...
        "cgroup": "workload.slice/workload-.\*.slice/.\*service"
        "xattr_filter": "oomd_example"
        ...
      ],
  }
```

If the following are present on the host with an xattr named "oomd_example":

- workload.slice/workload-a.slice/workload-a1.service
- workload.slice/workload-a.slice/workload-a2.service
- workload.slice/workload-b.slice/workload-b1.service

Then, this ruleset's set of detectors and actions will run three times, each
time setting the OomdContext to the particular cgroup value.

Detectors and actions will run on both their regular cgroup pattern, and the
ruleset cgroup pattern that is relative to the OomdContext's ruleset cgroup.
