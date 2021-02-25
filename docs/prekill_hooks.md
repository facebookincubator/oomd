# Prekill Hooks

Prekill hooks are an experimental generic, pluggable way to do work just before
oomd kills a cgroup.

## Background

Owners of an oomed process may want a heap dump or other memory
statistics of the killed program at the time it died to get insight into
potential misbehavior.

Prekill hooks direct oomd to collect these metrics, or do other arbitrary work,
just before it kills a cgroup. It is a generic interface not tied to any
particular metric collection approach or, specifically metric collection at all.

Hooks may timeout, and should not be assumed to run to completion. Process
owners should know the kernel may oom kill their code separately from oomd, in
which case prekill hooks will obviously not run at all.

## Configuration

Prekill hooks are configured the oomd.json config json in a top-level
"prekill_hooks" key, adjacent to "rulesets".

Prekill hooks are at the top level because they run on every kill oomd makes,
across all rulesets.

Prekill hooks are not interchangeable with plugins but are configured in
the same way, via "name" and "args". Hooks can't be used where plugins are
expected, and vice versa.

  {
      "rulesets": [
        ...
      ],
      "prekill_hooks": [
          {
              "name": "hypothetical_prekill_hook",
              "args": {}
          }
      ]
  }

Oomd currently supports at most one prekill hook at a time. Prekill hooks are
an experimental feature, and it is not obvious how multiple prekill hooks should
compose.

Rulesets may set a "prekill_hook_timeout" in seconds. If unset, the default is 5
seconds.

  {
      "rulesets": [
            {
                "name": "memory pressure protection",
                "prekill_hook_timeout": "30",
                "detectors": [...],
                "actions": [...]
      ],
      "prekill_hooks": [...]
  }

The prekill hook timeout sets a window for all prekill hooks in an action
chain to finish running. For example, consider:
- a ruleset with two kill plugin actions and a 5s prekill hook timeout
- the action chain fires
- the first action targets /foo.slice and fires a prekill hook on it
- the prekill hook finishes in 3s
- /foo.slice fails to die, so the first action returns CONTINUE
- the second kill plugin runs, targets /bar.slice, and fires a prekill hook

The second prekill hook only has 2s to run before it times out, since it's been
3s (or more) since the action chain started, and the action chain set a 5s
max window for prekill hooks to run.

## API

Prekill hook implementers should subclass PrekillHook and PrekillHookInvocation
and implement these core methods:

      /* same as BasePlugin::init(args, context) */
      int PrekillHook::init(
          const Engine::PluginArgs& args,
          const PluginConstructionContext& context);

      /* main method for a hook, called just before the cgroup is killed */
      std::unique_ptr<PrekillHookInvocation> PrekillHook::fire(
            const CgroupContext&);

      /* Invocation object returned from fire() is polled to see when the hook
         has finished running, and killing may begin */
      bool PrekillHookInvocation::didFinish()

      /* Invocation object is destructed either when it finishes, or early
         if it times out */
      PrekillHookInvocation::~PrekillHookInvocation()

Hooks are kicked off with PrekillHook::fire(cgroup) with the cgroup oomd intends
to kill.

Oomd is designed as a single threaded event loop, so fire() shouldn't do long
work that blocks the main thread. Instead, it vends an Invocation object which
will be polled every main loop tick (typically 1s) for didFinish(). The cgroup
will not be killed until didFinish() returns true, or we reach a timeout.

If oomd determines a PrekillHookInvocation timed out, it is destructed and
PrekillHookInvocation::~PrekillHookInvocation() called. The destructor will be
called before the cgroup is killed, regardless of whether the
hook timed out or didFinish() returned true.

All methods (fire, didFinish, ~PrekillHookInvocation) will be always be called
on the main thread and should not block for nontrivial time.  If blocking work
is needed, it should be done in other threads, possibly spawned in
PrekillHook::init().

## Guarantees

- At most one prekill hook will be running per ruleset at any moment. There may
  be multiple instances of a prekill hook running at the same time, as part of
  different rulesets.
- If a prekill hook is run on a cgroup, the cgroup is not guaranteed to die.
  Oomd may fail to kill it. (Oomd will then pick a different cgroup to try to
  kill, and again call the prekill hook on its new target before trying to kill
  it.)
- PrekillHooks are not guaranteed to outlive the Invocations they fire().
  Invocations should encapsulate any data they need to run to completion.
