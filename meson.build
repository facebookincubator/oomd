project('oomd', 'cpp',
  version : 'v0.3.2',
  meson_version : '>= 0.45',
  license : 'GPL2',
  default_options : ['stdsplit=false', 'cpp_std=c++17'])

cpp_args = ['-DMESON_BUILD']

inc = include_directories('src')

# Plumbing to #define a GIT_VERSION so Main.cpp can print it out
vcs_tagger = [meson.source_root() + '/vcs_tagger.sh',
    meson.source_root(),
    meson.project_version()]
version_h = vcs_tag(
    input : 'src/oomd/include/Version.h.in',
    output : 'Version.h',
    command : vcs_tagger)
versiondep = declare_dependency(sources : version_h)

srcs = files('''
    src/oomd/PluginConstructionContext.cpp
    src/oomd/Log.cpp
    src/oomd/Oomd.cpp
    src/oomd/OomdContext.cpp
    src/oomd/Stats.cpp
    src/oomd/StatsClient.cpp
    src/oomd/PluginRegistry.cpp
    src/oomd/config/ConfigCompiler.cpp
    src/oomd/config/ConfigTypes.cpp
    src/oomd/config/JsonConfigParser.cpp
    src/oomd/engine/DetectorGroup.cpp
    src/oomd/engine/Engine.cpp
    src/oomd/engine/Ruleset.cpp
    src/oomd/include/Assert.cpp
    src/oomd/include/CgroupPath.cpp
    src/oomd/plugins/AdjustCgroup.cpp
    src/oomd/plugins/BaseKillPlugin.cpp
    src/oomd/plugins/ContinuePlugin.cpp
    src/oomd/plugins/StopPlugin.cpp
    src/oomd/plugins/DumpCgroupOverview.cpp
    src/oomd/plugins/DumpKillInfoNoOp.cpp
    src/oomd/util/Fs.cpp
    src/oomd/util/Util.cpp
    src/oomd/plugins/MemoryAbove.cpp
    src/oomd/plugins/MemoryReclaim.cpp
    src/oomd/plugins/NrDyingDescendants.cpp
    src/oomd/plugins/PressureAbove.cpp
    src/oomd/plugins/PressureRisingBeyond.cpp
    src/oomd/plugins/SwapFree.cpp
    src/oomd/plugins/Senpai.cpp
    src/oomd/plugins/Exists.cpp
    src/oomd/plugins/KillIOCost.cpp
    src/oomd/plugins/KillMemoryGrowth.cpp
    src/oomd/plugins/KillSwapUsage.cpp
    src/oomd/plugins/KillPressure.cpp
'''.split())

fixture_srcs = files('''
    src/oomd/fixtures/FsFixture.cpp
    src/oomd/util/Fixture.cpp
'''.split())

deps = [versiondep,
        dependency('jsoncpp'),
        dependency('threads')]

# Optional dependencies
systemd_dep = dependency('libsystemd', required: false)
if systemd_dep.found()
  srcs += files('''
      src/oomd/plugins/systemd/BaseSystemdPlugin.cpp
      src/oomd/plugins/systemd/SystemdRestart.cpp
  '''.split())

  deps += [systemd_dep]
endif

oomd_lib = static_library('oomd',
                          srcs,
                          include_directories : inc,
                          cpp_args : cpp_args,
                          install : false,
                          dependencies : deps)
oomd_fixture_lib = static_library('oomd_fixture',
                                  fixture_srcs,
                                  include_directories : inc,
                                  cpp_args : cpp_args,
                                  dependencies : deps,
                                  link_with : oomd_lib)
executable('oomd',
           files('src/oomd/Main.cpp'),
           include_directories : inc,
           cpp_args : cpp_args,
           dependencies : deps,
           install: true,
           link_whole : oomd_lib)

prefixdir = get_option('prefix')
bindir = join_paths(prefixdir, get_option('bindir'))
sysconfdir = join_paths(prefixdir, get_option('sysconfdir'))
oomdconfdir = join_paths(sysconfdir, 'oomd')
systemunitdir = join_paths(prefixdir, 'lib/systemd/system')

substs = configuration_data()
substs.set('bindir', bindir)
substs.set('sysconfdir', sysconfdir)
substs.set('oomdconfdir', oomdconfdir)
substs.set('systemunitdir', systemunitdir)

configure_file(
    input : 'src/oomd/etc/oomd.service.in',
    output : 'oomd.service',
    configuration : substs,
    install_dir : systemunitdir)

configure_file(
    input : 'src/oomd/etc/desktop.json',
    output : 'oomd.json',
    configuration : substs,
    install_dir : oomdconfdir)

install_man('man/oomd.1')

# Core tests
# TODO: Normally we'd use a dictionary here, but they are only supported as of
# Meson version 0.47.0.  Ubuntu 18.04 provides only Meson version 0.45.1.
core_tests = [
  ['config',   files('src/oomd/config/JsonConfigParserTest.cpp')],
  ['oomd',     files('src/oomd/OomdTest.cpp')],
  ['util',     files('src/oomd/util/FixtureTest.cpp',
                     'src/oomd/util/FsTest.cpp',
                     'src/oomd/util/ScopeGuardTest.cpp',
                     'src/oomd/util/UtilTest.cpp')],
  ['context',  files('src/oomd/OomdContextTest.cpp')],
  ['log',      files('src/oomd/LogTest.cpp')],
  ['assert',   files('src/oomd/include/AssertTest.cpp')],
  ['cpath',    files('src/oomd/include/CgroupPathTest.cpp')],
  ['compiler', files('src/oomd/config/ConfigCompilerTest.cpp')],
  ['plugin',   files('src/oomd/plugins/CorePluginsTest.cpp')],
  ['stats',    files('src/oomd/StatsTest.cpp')],
]

# Optional tests
if systemd_dep.found()
    core_tests += [['systemd_plugin', files('src/oomd/plugins/systemd/SystemdPluginsTest.cpp')]]
endif


gtest_dep = dependency('gtest', main : true, required : false)
gmock_dep = dependency('gmock', required : false)
if gtest_dep.found() and gmock_dep.found()
    deps += [gtest_dep, gmock_dep]

    foreach test_source_tuple : core_tests

        test_name = test_source_tuple[0]
        sources = test_source_tuple[1]

        executable_name_suffix = test_name + '_tests'
        test_executable = executable('oomd_' + executable_name_suffix,
            sources,
            include_directories : inc,
            cpp_args : cpp_args,
            dependencies : deps,
            link_whole : [oomd_lib, oomd_fixture_lib])
        test(executable_name_suffix,
             test_executable,
             workdir : meson.source_root() + '/src')
    endforeach

endif
