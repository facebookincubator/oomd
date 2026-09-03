# Ruleset Cgroup

Ruleset-level cgroup settings are used to write generic configs that apply to
every matching cgroup.

## Background

Oomd users may want to run detectors and actions on every matching cgroup
without the need to dynamically generate cgroup configs. Users may also want to
write sets of actions and detectors that target the particular cgroup slice that
triggered an action rather than broadly applying a rule.

For example, a cgroup slice may contain four dynamically generated sub-slices.
We want to write a config that applies to each of those subslices individually,
such that detections on one child slice does not lead to an action on another
child slice.

## Configuration

Configure ruleset cgroup targeting with the `cgroup` and optional
`xattr_filter` fields in a ruleset object.

`xattr_filter` checks only that an extended attribute with that name exists on a
matching cgroup. It does not inspect the extended attribute value. Add an
`xattr_glob` detector to each detector group that must match an extended
attribute value.

For example:

```
{
  "rulesets": [
    {
      "name": "per-service policy",
      "cgroup": "workload.slice/workload-*.slice/*.service",
      "xattr_filter": "user.oomd_example",
      "detectors": [
        [
          "always",
          {"name": "continue"}
        ]
      ],
      "actions": [
        {"name": "stop"}
      ]
    }
  ]
}
```

If the following cgroups have an xattr named `user.oomd_example`:

- workload.slice/workload-a.slice/workload-a1.service
- workload.slice/workload-a.slice/workload-a2.service
- workload.slice/workload-b.slice/workload-b1.service

Then, oomd creates one ruleset instance for each cgroup. Each instance has its
own `OomdContext` ruleset cgroup value. Removal discards the instance. If a
matching path is recreated, oomd replaces the instance and resets its plugin
state.

A detector or action can configure a regular `cgroup` pattern, a
`ruleset_cgroup` pattern that is relative to this value, or both. A plugin that
configures both patterns uses the union of both matching cgroup sets.
