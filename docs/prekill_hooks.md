# Prekill Hooks

Prekill hooks are an experimental generic, pluggable way to do work just before
oomd kills a cgroup.

## Background

Owners of a process that oomd selects may want a heap dump or other memory
statistics before the process stops.

Prekill hooks direct oomd to collect these metrics, or do other arbitrary work,
just before it kills a cgroup. It is a generic interface not tied to any
particular metric collection approach or, specifically metric collection at all.

Hooks can time out. The kernel can also kill a process without oomd. A prekill
hook does not run for a kernel-initiated kill.

## Configuration

Configure prekill hooks in the top-level `prekill_hooks` array in the oomd JSON
configuration. This array is next to `rulesets`.

Prekill hooks are at the top level because they can run for kills from any
ruleset.

Prekill hooks are not interchangeable with plugins. Both use `name` and `args`
fields, but each type has a separate registry and interface.

```json
{
  "rulesets": [],
  "prekill_hooks": [
    {
      "name": "dummy_prekill_hook",
      "args": {
        "cgroup": "/foo,/bar/*/baz",
        "xattr": "user.enable_prekill_hook"
      }
    }
  ]
}
```

For each kill, oomd runs the first hook that matches the target cgroup. At most
one prekill hook runs for one kill.

Drop-in configurations can contain prekill hooks. These hooks have priority
over hooks in the base configuration. A hook from a newer drop-in has priority
over a hook from an older drop-in.

The `cgroup` argument is a list of comma-separated patterns. Patterns are cgroup
paths, except path components may be "*". No other glob matching works except
star for a single whole path component.

A cgroup path matches a pattern if it 1) exactly matches the pattern, 2) is an
ancestor of a path that would match the pattern, or 3) is a descendant of a path
that matches the pattern.

The optional `xattr` argument is an exact extended attribute name. A hook
matches when the target has this attribute. The attribute value is not read.
If both selectors are set, either selector can match. A hook with neither
selector does not match any target.

To run on all kills, set `"cgroup": "/"`.

Rulesets may set a "prekill_hook_timeout" in seconds. If unset, the default is 5
seconds.

```json
{
  "rulesets": [
    {
      "name": "memory pressure protection",
      "prekill_hook_timeout": "30",
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
  ],
  "prekill_hooks": []
}
```

The prekill hook timeout sets a window for all prekill hooks in an action
chain to finish running. For example, consider:

- a ruleset with two kill plugin actions and a 5-second prekill hook timeout
- the action chain fires
- the first action targets /foo.slice and fires a prekill hook on it
- the prekill hook finishes in 3 seconds
- /foo.slice fails to die, so the first action returns CONTINUE
- the second kill plugin runs, targets /bar.slice, and fires a prekill hook

The second prekill hook has only 2 seconds to run. At least 3 seconds of the
5-second window elapsed during the first hook.

## API

Implement a hook with subclasses of `PrekillHook` and
`PrekillHookInvocation`. `PrekillHook::init` already parses the common
`cgroup` and `xattr` selectors. Override it only to parse more arguments. Add
the custom arguments to `argParser_`, and then call
`PrekillHook::init(args, context)`.

      /* same as BasePlugin::init(args, context) */
      int PrekillHook::init(
          const Engine::PluginArgs& args,
          const PluginConstructionContext& context);

      /* main method for a hook, called just before the cgroup is killed */
      std::unique_ptr<PrekillHookInvocation> PrekillHook::fire(
          const CgroupContext& cgroup,
          const ActionContext& action_context) = 0;

      /* Invocation object returned from fire() is polled to see when the hook
         has finished running, and killing may begin */
      bool PrekillHookInvocation::didFinish() = 0;

Register the hook with `REGISTER_PREKILL_HOOK` in a `.cpp` file.

oomd calls `PrekillHook::fire(cgroup, action_context)` with the selected cgroup
and the current action context.

oomd uses a single-threaded event loop. `fire()` must not block for a long time.
It returns an invocation object. oomd polls `didFinish()` on each event-loop
tick. oomd does not kill the cgroup until `didFinish()` returns true or the
timeout expires.

If a `PrekillHookInvocation` times out, oomd destroys it before it kills the
cgroup. oomd also destroys a completed invocation before the kill.

oomd calls `fire`, `didFinish`, and the invocation destructor on the main
thread. These methods must not block for a long time. Use another thread for
blocking work. `PrekillHook::init()` can create that thread.

## Guarantees

- At most one prekill hook invocation runs in one action chain at one time.
  Different runnable ruleset instances can have concurrent invocations.
- If a prekill hook is run on a cgroup, the cgroup is not guaranteed to die.
  oomd can select another cgroup if the kill fails. It calls the matching
  prekill hook for the new target before it tries the next kill.
- PrekillHooks are not guaranteed to outlive the Invocations they fire().
  Invocations should encapsulate any data they need to run to completion.
