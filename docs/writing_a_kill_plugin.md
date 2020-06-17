# Writing a kill plugin

Kill plugins are regular plugins: they inherit from `BaseKillPlugin` which
inherits from `BasePlugin`. Read [writing_a_plugin.md](writing_a_plugin.md)
first; everything in that doc applies to kill plugins as well.

# BaseKillPlugin default behavior

Kill plugins are responsible for the policy picking which cgroup to kill out of
a set of options. The mechanism of killing, with support for all the
standard kill plugin behavior, is implemented by `BaseKillPlugin`.

`BaseKillPlugin` subclasses by default have support for the `cgroup`,
`recursive`, `dry`, `debug`, and `post_action_delay` params as described in
[core_plugins.md](core_plugins.md).

Additionally, plugins that follow the `BaseKillPlugin` template respect
oomd.prefer and oomd.avoid, though technically that’s not part of
`BaseKillPlugin`.

# Interface

The `BaseKillPlugin` interface is found in `plugins/BaseKillPlugin.h`.
`BaseKillPlugin` is a pure virtual class that defines what is expected of each
plugin. This document assumes you have already read through
`engine/BasePlugin.h`. If you have not, please do.

There are two methods you must override:

      virtual std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
          OomdContext& ctx,
          std::vector<OomdContext::ConstCgroupContextRef>& cgroups,
          std::function<void(const CgroupContext&)>& olog_kill_fn_out) = 0;

      virtual void ologKillTarget(
          OomdContext& ctx,
          const CgroupContext& target,
          const std::vector<OomdContext::ConstCgroupContextRef>& peers) = 0;

and two you may want to override:

      int init(
          const Engine::PluginArgs& args,
          const PluginConstructionContext& context);

      virtual void prerun(OomdContext& context) {};

Note that these are different from the 3 `BasePlugin` methods `run`, `init`, and
`prerun`. `run` has been implemented for you in `BaseKillPlugin`, and will call
out to the subclass’ `rankForKilling` and `ologKillTarget` implementations.
`init` has a default implementation which you may wish to override, but unlike
`BasePlugin`s, are not required to. `prerun` is the same.

# Anatomy of KillIOCost

When creating a new kill plugin, it’s easiest to copy the files of an existing
kill plugin and follow their format. KillIOCost is a simple, useful plugin that
uses most of the APIs we’re interested in. It’s spread across 3 files, plus
entries in the TARGETS and meson.build files.

### KillIOCost.h

      #include "oomd/plugins/BaseKillPlugin.h"

      namespace Oomd {

      template <typename Base = BaseKillPlugin>
      class KillIOCost : public Base {

KillIOCost inherits from a templated base class to facilitate unit testing. The
base class is always `BaseKillPlugin`, except in CorePluginsTest.cpp where we
pass in `BaseKillPluginMock` to mocks out the killing. It's safe to assume
`Base = BaseKillPlugin`.

      public:
        void prerun(OomdContext& ctx) override;

        static KillIOCost* create() {
          return new KillIOCost();
        }

        ~KillIOCost() override = default;

      protected:
        std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
            OomdContext& ctx,
            std::vector<OomdContext::ConstCgroupContextRef>& cgroups,
            std::function<void(const CgroupContext&)>& olog_kill_fn_out) override;

KillIOCost implements `prerun` and `rankForKilling`. Other plugins may want to
override `init` as well.

      };

      } // namespace Oomd

      #include "oomd/plugins/KillIOCost-inl.h"

Because KillIOCost has a templated base class, its method implementations can't
be in a `.cpp` file.

### KillIOCost.cpp

      #include "oomd/plugins/KillIOCost.h"

      #include "oomd/PluginRegistry.h"

      namespace Oomd {
      REGISTER_PLUGIN(kill_by_io_cost, KillIOCost<>::create);
      } // namespace Oomd

The `.cpp` file just registers the `kill_by_io_cost` plugin. List the `.cpp`
file in TARGETS and meson.build or the plugin will not be registered.


### KillIOCost-inl.h

      namespace Oomd {

      template <typename Base>
      int KillIOCost<Base>::init(
          const Engine::PluginArgs& args,
          const PluginConstructionContext& context) {
        // additional arg parsing and initialization here
        return Base::init(args, context);
      }

`BaseKillPlugin` implements  `init(...)`, parsing the  `cgroup`, `recursive`,
`post_action_delay`, `dry`, and `debug` plugin args by default. To eg. support
additional plugin-specific arguments, override init(...) and include a call to
`Base::init` as above. In the real code `KillIOCost` does not override
`init(...)` because `BaseKillPlugin::init(...)` is sufficient.

      template <typename Base>
      void KillIOCost<Base>::prerun(OomdContext& ctx) {
        // Make sure temporal counters be available when run() is invoked
        Base::prerunOnCgroups(
            ctx, [](const auto& cgroup_ctx) { cgroup_ctx.io_cost_rate(); });
      }

`Base::prerunOnCgroups(...)` supports `"recursive"` by default.

      template <typename Base>
      std::vector<OomdContext::ConstCgroupContextRef>
      KillIOCost<Base>::rankForKilling(
          OomdContext& ctx,
          const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) {

`BaseKillPlugin::run` calls `rankForKilling` repeatedly as it walks down the
cgroup tree. `BaseKillPlugin::run` handles getting the CgroupContexts from the
plugin's `"cgroup"` arg, recursing (or not) if the plugin's `"recursive"` arg is
set, respecting memory.oom.group, and actually killing the appropriate pids.
`KillIOCost::rankForKilling(...)` is responsible for picking which cgroup to
kill from among the plugin's `"cgroup"` argument, or among a set of siblings if
we're recursing. See comment in `BaseKillPlugin.h` for in-depth details.

We return a sorted vector, instead of the single best cgroup, because if killing
the best-choice fails, we'll try to kill the next-best, and so on.

        return OomdContext::sortDescWithKillPrefs(
            cgroups, [](const CgroupContext& cgroup_ctx) {
              return cgroup_ctx.io_cost_rate().value_or(0);
            });

`sortDescWithKillPrefs` adds default support for `oomd_prefer` and `oomd_avoid`.

      }

      template <typename Base>
      void KillIOCost<Base>::ologKillTarget(
          OomdContext& ctx,
          const CgroupContext& target,
          const std::vector<OomdContext::ConstCgroupContextRef>& /* unused */) {
        OLOG << "Picked \"" << target.cgroup().relativePath() << "\" ("
            << target.current_usage().value_or(0) / 1024 / 1024
            << "MB) based on io cost generation at "
            << target.io_cost_rate().value_or(0);
      }

`ologKillTarget` is called every time a cgroup is chosen from the cgroups
returned from `rankForKilling`. KillIOCost uses it to log io_cost_rate() to
help readers of the logs understand why this cgroup was chosen.

`ologKillTarget` will be called at least once per `rankForKilling`, on the first
element returned by `rankForKilling`. If killing that cgroup fails,
`ologKillTarget` will be called on subsequent elems returned by
`rankForKilling`, in order, as we try to kill them. If `"recursive"` is set,
`ologKillTarget` will be called on every cgroup in the path down to the victim
leaf cgroup.

The 3rd argument of `ologKillTarget` is the set of cgroups `target` was selected
from. See KillMemoryGrowth for an example where this is useful to log.

      }

      } // namespace Oomd
