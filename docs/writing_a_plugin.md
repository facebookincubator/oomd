# Writing a plugin

Plugins are at the core of oomd. Everything that implements business logic
must be done in a plugin. If you haven't already, you should read
[configuration.md](configuration.md). That doc explains the high level goals
of plugins.


## Interface

The `BasePlugin` interface is found in `engine/BasePlugin.h`. `BasePlugin`
is a pure virtual class that defines what is expected of each plugin. This
document assumes you have already read through `engine/BasePlugin.h`. If
you have not, please do.

Ignoring the comments and less relevant bits, every plugin must implement
the following two methods:

      virtual int init(
          MonitoredResources& resources,
          std::unordered_map<std::string, std::string> args) = 0;

      virtual PluginRet run(OomdContext& context) = 0;

### init(..)

The `init(..)` method is called by the config compiler. The config compiler
transforms (typically) JSON configuration into actual data structures oomd
will work with. As part of the compilation process, oomd will run `init(..)`
on every instantiated plugin.

`MonitoredResources& resources` is a typedef'd std::unordered_set of strings.
Plugins should insert any cgroups they want accounting on into `resources`.
Accounting information will be passed back to the plugin at each event loop
tick in `run(..)` via `OomdContext& context`. Kill plugins will typically
place any cgroups they are instructed to possibly kill into `resources`.

`std::unordered_map<std::string, std::string args> args` is a map of
arguments that are provided to the plugin. Each plugin, as defined by the
config schema, is allowed to have a variable number of NAME=VAL arguments.
The core oomd runtime will provide those arguments as a NAME -> VAL map.

If plugin initialization is success, the plugin must return zero. A non-zero
return value will fail the config compilation process (and usually exit the
process). Plugins are encouraged to print a useful error message before
returning non-zero.

### run(..)

`run(..)` is called by the core oomd runtime each event loop tick. The
duration between each tick can be configured via `--interval,-i` on the
command line. `run(..)` is the work horse function of every plugin. This
is where most, if not all, of the work is expected to be done. You can do
pretty much whatever you want in the plugin. Make syscalls, inspect the file
system, mess with other plugins by modifying `OomdContext`, name it. (Not to
imply doing all of those things are a productive idea).

`OomdContext& context` is an object that contains state about the system.
`OomdContext&` typically contains accounting information on every cgroup
that the core oomd runtime has been instructed to monitor. This means that
cgroups other plugins placed into `resources` in `init(..)` will also be
available.

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

## Logging

Plugins are encouraged to use the oomd logging facilities.

`OLOG` is an ostream style macro that prints logs asynchronously. This is
useful as systems under intense memory pressure are not usually able to write
to filesystems or output things to stdout/sterr. It's usually not a good idea
to log inline on a production host, as writes can block indefinitely, thus
limiting the utility of oomd.

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

Plugins do not need to exist in the `Oomd` namespace but it'll save you some
typing.

    class ContinuePlugin : public Engine::BasePlugin {

All plugins must derive from `BasePlugin`, as discussed in the previous
section.

     public:
      int init(
          Engine::MonitoredResources& /* unused */,
          std::unordered_map<std::string, std::string> /* unused */) override {
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

    } // namespace Oomd

This is our static factory method. Note it has to be static, as the config
compiler uses the static plugin registry, and all factory functions in the
plugin registry must be static.

      ~ContinuePlugin() = default;

We can use the default destructor.

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
