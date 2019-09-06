project('oomd', 'cpp',
  version : '0.1.0',
  license : 'GPL2')

cpp_args = ['-std=c++17', '-g', '-rdynamic', '-DMESON_BUILD']

inc = include_directories('''
    oomd/include
    oomd/util
    oomd/
'''.split())

# Plumbing to #define a GIT_VERSION so Main.cpp can print it out
vcs_tagger = [meson.source_root() + '/vcs_tagger.sh',
    meson.source_root(),
    meson.project_version()]
version_h = vcs_tag(
    input : 'oomd/include/Version.h.in',
    output : 'Version.h',
    command : vcs_tagger)
versiondep = declare_dependency(sources : version_h)

srcs = files('''
    oomd/Log.cpp
    oomd/Oomd.cpp
    oomd/OomdContext.cpp
    oomd/Stats.cpp
    oomd/StatsClient.cpp
    oomd/PluginRegistry.cpp
    oomd/config/ConfigCompiler.cpp
    oomd/config/ConfigTypes.cpp
    oomd/config/JsonConfigParser.cpp
    oomd/engine/DetectorGroup.cpp
    oomd/engine/Engine.cpp
    oomd/engine/Ruleset.cpp
    oomd/include/Assert.cpp
    oomd/include/CgroupPath.cpp
    oomd/plugins/AdjustCgroup.cpp
    oomd/plugins/BaseKillPlugin.cpp
    oomd/plugins/ContinuePlugin.cpp
    oomd/plugins/DumpCgroupOverview.cpp
    oomd/util/Fs.cpp
    oomd/util/Util.cpp
    oomd/plugins/MemoryAbove.cpp
    oomd/plugins/MemoryReclaim.cpp
    oomd/plugins/PressureAbove.cpp
    oomd/plugins/PressureRisingBeyond.cpp
    oomd/plugins/SwapFree.cpp
    oomd/plugins/Exists.cpp
    oomd/plugins/KillIOCost.cpp
    oomd/plugins/KillMemoryGrowth.cpp
    oomd/plugins/KillSwapUsage.cpp
    oomd/plugins/KillPressure.cpp
'''.split())

deps = [versiondep,
        dependency('jsoncpp'),
        dependency('threads')]

# Optional dependencies
systemd_dep = dependency('libsystemd', required: false)
if systemd_dep.found()
  srcs += files('''
      oomd/plugins/systemd/BaseSystemdPlugin.cpp
      oomd/plugins/systemd/SystemdRestart.cpp
  '''.split())

  deps += [systemd_dep]
endif

oomd_lib = static_library('oomd',
                          srcs,
                          include_directories : inc,
                          cpp_args : cpp_args,
                          install : false,
                          dependencies : deps)
executable('oomd_bin',
           files('oomd/Main.cpp'),
           include_directories : inc,
           cpp_args : cpp_args,
           dependencies : deps,
           install: true,
           link_whole : oomd_lib)


# Core tests
# TODO: Normally we'd use a dictionary here, but they are only supported as of
# Meson version 0.47.0.  Ubuntu 18.04 provides only Meson version 0.45.1.
core_tests = [
  ['config',   files('oomd/config/JsonConfigParserTest.cpp')],
  ['oomd',     files('oomd/OomdTest.cpp')],
  ['util',     files('oomd/util/FsTest.cpp', 'oomd/util/ScopeGuardTest.cpp',
                     'oomd/util/UtilTest.cpp')],
  ['context',  files('oomd/OomdContextTest.cpp')],
  ['log',      files('oomd/LogTest.cpp')],
  ['assert',   files('oomd/include/AssertTest.cpp')],
  ['cpath',    files('oomd/include/CgroupPathTest.cpp')],
  ['compiler', files('oomd/config/ConfigCompilerTest.cpp')],
  ['plugin',   files('oomd/plugins/CorePluginsTest.cpp')],
  ['stats',    files('oomd/StatsTest.cpp')],
]

# Optional tests
if systemd_dep.found()
    core_tests += [['systemd_plugin', files('oomd/plugins/systemd/SystemdPluginsTest.cpp')]]
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
            link_whole : oomd_lib)
        test(executable_name_suffix, test_executable)
   endforeach


    # Copy over fixtures to build dir so 'ninja test' will work
    r = run_command('rm', '-r', meson.build_root() + '/oomd')
    r = run_command('mkdir', '-p', meson.build_root() + '/oomd')
    if r.returncode() != 0
      error('Failed to create test fixture directory')
    endif

    r = run_command('cp', '-r', meson.source_root() + '/oomd/fixtures', meson.build_root() + '/oomd')
    if r.returncode() != 0
      error('Failed to copy test fixtures')
    endif
endif
