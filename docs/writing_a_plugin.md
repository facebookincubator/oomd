# Writing a plugin

Plugins are at the core of oomd. Everything that implements business logic
must be done in a plugin. First, read [configuration.md](configuration.md).
That document explains the high-level goals of plugins.

If you write a kill plugin, inherit from `BaseKillPlugin` to reuse the common
kill behavior. Read this document, and then read
[writing_a_kill_plugin.md](writing_a_kill_plugin.md).

## Interface

The `BasePlugin` interface is found in `engine/BasePlugin.h`. `BasePlugin`
is a pure virtual class that defines what is expected of each plugin. This
document assumes you have already read through `engine/BasePlugin.h`. If
you have not, please do.

Ignoring the comments and less relevant bits, every plugin must implement
the following two methods:

      virtual int init(
          const PluginArgs& args,
          const PluginConstructionContext& context) = 0;

      /* where PluginArgs is an alias of
         std::unordered_map<std::string, std::string> */

      virtual PluginRet run(OomdContext& context) = 0;

and optionally implement:

      virtual void prerun(OomdContext& context){}

### init(..)

The `init(..)` method is called by the config compiler. The config compiler
transforms (typically) JSON configuration into actual data structures oomd
will work with. As part of the compilation process, oomd will run `init(..)`
on every instantiated plugin.

`const PluginArgs& args` is a string map of arguments for the plugin. The JSON
`args` object can contain string, number, or Boolean values. The JSON parser
converts each value to a string in `PluginArgs`.

`const PluginConstructionContext& context` holds other init()-time context.
`context.cgroupFs()` is the cgroup filesystem that oomd monitors. The
`--cgroup-fs` option sets this path. The default path is `/sys/fs/cgroup`.

The plugin must return zero after successful initialization. A nonzero return
value makes configuration compilation fail. The plugin should log a useful
error before it returns a nonzero value.

### run(..)

The runtime calls detector `run(..)` methods on each event-loop tick. It calls
action `run(..)` methods only after a detector group fires. The `--interval`
or `-i` option sets the interval between ticks. Most plugin work occurs in
`run(..)`.

`OomdContext& context` is an object that contains state about the system.
Call `context.addToCacheAndGet(...)` to select cgroups and make their accounting
data available for the current event-loop tick.

### prerun(..)

`prerun(..)` is called by the core oomd runtime each event loop tick, before
`run(..)` has been called on any plugin. It is guaranteed to be invoked as long
as the plugin is enabled, even if it is an action plugin and not triggered.
Therefore, it is designed to execute stateful logic, such as calculating sliding
window metrics, storing time when a threshold is exceeded, etc.

If the plugin may rely on temporal cgroup counters such as average usage and io
cost rate (see `CgroupContext.h`) in `run(..)`, it must implement `prerun(..)`
to retrieve temporal counters for all of its cgroups to keep them from getting
stale. See `KillIOCost` for example.

## Plugin registration

You might have wondered, how does the config compiler know which plugin name
maps to which C++ class? This section goes into the details of plugin
registration.

oomd employs a static plugin registration system. In other words, oomd plugins
will insert themselves into a map of plugin name -> plugin factory method.
The details of static registration are out of the scope of this document, but
plenty of sources exist online that explain the details. In short, static
registration ensures that the map of X -> Y will be fully populated before
the program reaches `int main()`.

Plugins are required to register themselves to the plugin registry via the
`REGISTER_PLUGIN` macro defined in `oomd/PluginRegistry.h`. If you do not
register your plugin and try to use it in a config, the compilation process
will fail and oomd will not start up.

`REGISTER_PLUGIN` takes a zero-argument factory callable. A static `create`
method is the normal pattern. The callable must return a plugin pointer that
uses the standard deleter. Custom deleters are not supported.

## Build files

Add the plugin to each build that must compile it.

In fbsource, generic public plugin headers under `plugins/` are part of
`shared_plugins`. Generic public plugin source files under `plugins/` are part
of `oss_plugins` and the fleet `plugins` target. The source globs add them by
default.

Use the exception lists only when the plugin is not generic:

- Put Facebook-only plugin code under `plugins/facebook/` and list it in
  `fb_plugin_headers` or `fb_plugin_srcs` in `fbcode/oomd/BUCK`.
- List OSS-only plugin code in `oss_only_plugin_headers` or
  `oss_only_plugin_srcs` in `fbcode/oomd/BUCK`.

The `shared_plugins` target owns headers only. Link tests and binaries against
one complete plugin variant when they need plugin registration. Use
`oss_plugins` for generic plugin tests. Use the fleet `plugins` target for
Facebook-only plugin tests and the fleet binary.

For the public Meson build, add exported generic plugin source files and tests
to `public_tld/meson.build`. If the plugin needs an optional library, put the
source and tests in the matching optional dependency block.

After you add or move an exported C or C++ file, run
`buck test fbcode//oomd:oss_source_manifest_test -- --timeout=300`. This test
checks Meson source coverage, subject to reviewed exceptions. It also checks
exported source and header files for non-public includes and internal markers.

## Logging

Plugins are encouraged to use the oomd logging facilities.

`OLOG` is an ostream style macro that prints logs asynchronously. This is
useful as systems under intense memory pressure are not usually able to write
to filesystems or write to standard output or standard error. Avoid synchronous
logging on a production host because a write can block indefinitely.

`OLOG` is also smart enough to log inline (ie not async) when run in unit
tests. Logging async in unit tests can mess with gtest output parsing.

## Anatomy of ContinuePlugin

There is a functioning (but useless) example plugin included in oomd's set of
core plugins: ContinuePlugin. You can find the code at
`plugins/ContinuePlugin.h` and `plugins/ContinuePlugin.cpp.`

### ContinuePlugin.h

    #pragma once

    #include "oomd/engine/BasePlugin.h"

    namespace Oomd {

Plugins do not need to be in the `Oomd` namespace. The namespace reduces the
required qualification.

    class ContinuePlugin : public Engine::BasePlugin {

All plugins must derive from `BasePlugin`, as discussed in the previous
section.

     public:
      int init(
          const Engine::PluginArgs& /* unused */,
          const PluginConstructionContext& /* unused */) override {
        return 0;
      }

The `init(..)` method is implemented inline here. Note that we do not examine
our arguments or register any resource requirements. We return 0 to signify
success.

     Engine::PluginRet run(OomdContext& /* unused */) override {
        return Engine::PluginRet::CONTINUE;
      }

Our plugin does nothing besides return CONTINUE.

      static ContinuePlugin* create() {
        return new ContinuePlugin();
      }

      ~ContinuePlugin() override = default;

    };

    } // namespace Oomd

This is the static factory method. The class can use the default destructor.

### ContinuePlugin.cpp

    #include "oomd/plugins/ContinuePlugin.h"

    #include "oomd/PluginRegistry.h"

    namespace Oomd {

    REGISTER_PLUGIN(continue, ContinuePlugin::create);

    } // namespace Oomd

The only thing our cpp file does is register our plugin into the plugin
registry. Carefully note that we did not register our plugin in the header
file. Doing that might work for now, but will cause (somewhat cryptic) errors
if someone decides to include your plugin header and subclass some things.

What happens in this bad case is that there will be multiple places where
your plugin will be registered. The `REGISTER_PLUGIN` macro is designed such
that collision will occur if you try to register > 1 plugin with the same name.
