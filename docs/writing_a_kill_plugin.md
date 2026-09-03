# Writing a kill plugin

Kill plugins are regular plugins: they inherit from `BaseKillPlugin` which
inherits from `BasePlugin`. Read [writing_a_plugin.md](writing_a_plugin.md)
first; everything in that doc applies to kill plugins as well.

# BaseKillPlugin default behavior

Kill plugins are responsible for the policy picking which cgroup to kill out of
a set of options. The mechanism of killing, with support for all the
standard kill plugin behavior, is implemented by `BaseKillPlugin`.

`BaseKillPlugin` subclasses by default have support for the `cgroup`,
`ruleset_cgroup`, `recursive`, `post_action_delay`, `dry`, `always_continue`,
`debug`, `kernelkill`, `reap_memory`, and `log_kmemalloc_prekill` arguments.
See [core_plugins.md](core_plugins.md) for the common kill behavior.

Additionally, plugins that follow the `BaseKillPlugin` template respect
`trusted.oomd_prefer`, `user.oomd_prefer`, `trusted.oomd_avoid`, and
`user.oomd_avoid`. `OomdContext::sortDescWithKillPrefs` implements this
preference order.

# Interface

The `BaseKillPlugin` interface is found in `plugins/BaseKillPlugin.h`.
`BaseKillPlugin` is a pure virtual class that defines what is expected of each
plugin. This document assumes you have already read through
`engine/BasePlugin.h`. If you have not, please do.

There are two methods you must override:

      virtual std::vector<OomdContext::ConstCgroupContextRef> rankForKilling(
          OomdContext& ctx,
          const std::vector<OomdContext::ConstCgroupContextRef>& cgroups) = 0;

      virtual void ologKillTarget(
          OomdContext& ctx,
          const CgroupContext& target,
          const std::vector<OomdContext::ConstCgroupContextRef>& peers) = 0;

and two you may want to override:

      int init(
          const Engine::PluginArgs& args,
          const PluginConstructionContext& context);

      virtual void prerun(OomdContext& context) {};

These methods are different from the three `BasePlugin` methods: `run`, `init`,
and `prerun`. `BaseKillPlugin` implements `run` and calls the subclass
`rankForKilling` and `ologKillTarget` methods. `BaseKillPlugin` also implements
`init`. Override `init` only when the subclass has more arguments. The
`prerun` override remains optional.

# Anatomy of KillIOCost

When you create a kill plugin, copy the files of an existing
kill plugin and follow their format. KillIOCost is a simple, useful plugin that
uses most of the APIs in this guide. It is spread across three files, plus
entries in the build files. See the build-file section in
[writing_a_plugin.md](writing_a_plugin.md).

### KillIOCost.h

      #include "oomd/plugins/BaseKillPlugin.h"

      namespace Oomd {

      template <typename Base = BaseKillPlugin>
      class KillIOCost : public Base {

KillIOCost inherits from a templated base class to facilitate unit testing. The
base class is always `BaseKillPlugin`, except in CorePluginsTest.cpp where we
pass in `BaseKillPluginMock` to mock the kill operation. You can assume
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
            const std::vector<OomdContext::ConstCgroupContextRef>& cgroups)
            override;

        void ologKillTarget(
            OomdContext& ctx,
            const CgroupContext& target,
            const std::vector<OomdContext::ConstCgroupContextRef>& peers)
            override;

KillIOCost implements `prerun`, `rankForKilling`, and `ologKillTarget`.

      };

      } // namespace Oomd

      #include "oomd/plugins/KillIOCost-inl.h"

Because KillIOCost has a templated base class, its method implementations cannot
be in a `.cpp` file.

### KillIOCost.cpp

      #include "oomd/plugins/KillIOCost.h"

      #include "oomd/PluginRegistry.h"

      namespace Oomd {
      REGISTER_PLUGIN(kill_by_io_cost, KillIOCost<>::create);
      } // namespace Oomd

The `.cpp` file just registers the `kill_by_io_cost` plugin. Make sure the
`.cpp` file is in the correct build target, or the plugin will not be
registered. For build-file details, see
[writing_a_plugin.md](writing_a_plugin.md).


### KillIOCost-inl.h

      namespace Oomd {

`KillIOCost` does not override `init(...)`. It inherits the implementation from
`BaseKillPlugin`. A subclass that adds arguments must override `init(...)`, add
its arguments to `argParser_`, and then call `Base::init(args, context)`.

      template <typename Base>
      void KillIOCost<Base>::prerun(OomdContext& ctx) {
        // Make temporal counters available when run() is invoked.
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
it recurses. See the comment in `BaseKillPlugin.h` for more details.

Return a sorted vector instead of one cgroup. If the first kill fails, oomd
tries the next candidate.

        return OomdContext::sortDescWithKillPrefs(
            cgroups, [](const CgroupContext& cgroup_ctx) {
              return cgroup_ctx.io_cost_rate().value_or(0);
            });

`sortDescWithKillPrefs` applies the standard prefer and avoid xattrs.

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

`ologKillTarget` runs when oomd selects a nonempty candidate. If that kill
fails, it runs for each later candidate that oomd tries. If `"recursive"` is
set, it also runs for each selected cgroup on the path to the victim leaf.

The 3rd argument of `ologKillTarget` is the set of cgroups `target` was selected
from. See KillMemoryGrowth for an example where this is useful to log.

      }

      } // namespace Oomd
