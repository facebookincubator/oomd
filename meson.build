project('oomd', 'cpp',
  version : '0.1.0',
  license : 'GPL2')

cpp_args = ['-std=c++17', '-g', '-rdynamic']

# The ".." include path is necessary if one
# does not want to build via the "build-from-github.sh" script.
# Even then, the repo must not be cloned under an alternate name;
# the clone directory must be "oomd".
inc = include_directories('''
    include
    util
    .
    ..
'''.split())

srcs = files('''
    Log.cpp
    Oomd.cpp
    OomdContext.cpp
    PluginRegistry.cpp
    config/ConfigCompiler.cpp
    config/ConfigTypes.cpp
    config/JsonConfigParser.cpp
    engine/DetectorGroup.cpp
    engine/Engine.cpp
    engine/Ruleset.cpp
    include/Assert.cpp
    include/CgroupPath.cpp
    plugins/AdjustCgroup.cpp
    plugins/BaseKillPlugin.cpp
    plugins/ContinuePlugin.cpp
    plugins/DumpCgroupOverview.cpp
    util/Fs.cpp
    util/Util.cpp
    plugins/MemoryAbove.cpp
    plugins/MemoryReclaim.cpp
    plugins/PressureAbove.cpp
    plugins/PressureRisingBeyond.cpp
    plugins/SwapFree.cpp
    plugins/KillMemoryGrowth.cpp
    plugins/KillSwapUsage.cpp
    plugins/KillPressure.cpp
'''.split())

deps = [dependency('jsoncpp'),
        dependency('threads')]

# Optional dependencies
systemd_dep = dependency('libsystemd', required: get_option('systemd'))
if systemd_dep.found()
  srcs += files('''
      plugins/systemd/BaseSystemdPlugin.cpp
      plugins/systemd/SystemdRestart.cpp
  '''.split())

  deps += [systemd_dep]
endif

oomd_lib = library('oomd',
                   srcs,
                   include_directories : inc,
                   cpp_args : cpp_args,
                   install: true,
                   dependencies : deps)
executable('oomd_bin',
           files('Main.cpp'),
           include_directories : inc,
           cpp_args : cpp_args,
           dependencies : deps,
           install: true,
           install_rpath: join_paths(get_option('prefix'), get_option('libdir')),
           link_with : oomd_lib)


# Core tests
# TODO: Normally we'd use a dictionary here, but they are only supported as of
# Meson version 0.47.0.  Ubuntu 18.04 provides only Meson version 0.45.1.
core_tests = [
  ['config',   files('config/JsonConfigParserTest.cpp')],
  ['oomd',     files('OomdTest.cpp')],
  ['util',     files('util/FsTest.cpp', 'util/ScopeGuardTest.cpp',
                     'util/UtilTest.cpp')],
  ['context',  files('OomdContextTest.cpp')],
  ['log',      files('LogTest.cpp')],
  ['assert',   files('include/AssertTest.cpp')],
  ['cpath',    files('include/CgroupPathTest.cpp')],
  ['compiler', files('config/ConfigCompilerTest.cpp')],
  ['plugin',   files('plugins/CorePluginsTest.cpp')],
]

# Optional tests
if systemd_dep.found()
    core_tests += [['systemd_plugin', files('plugins/systemd/SystemdPluginsTest.cpp')]]
endif


gtest_dep = dependency('gtest', main : true, required : get_option('test'))
gmock_dep = dependency('gmock', required : get_option('test'))
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
            link_with : oomd_lib)
        test(executable_name_suffix, test_executable)
   endforeach


    # Copy over fixtures to build dir so 'ninja test' will work
    r = run_command('rm', '-r', meson.build_root() + '/oomd')
    r = run_command('mkdir', '-p', meson.build_root() + '/oomd')
    if r.returncode() != 0
      error('Failed to create test fixture directory')
    endif

    r = run_command('cp', '-r', meson.source_root() + '/fixtures', meson.build_root() + '/oomd')
    if r.returncode() != 0
      error('Failed to copy test fixtures')
    endif
endif
