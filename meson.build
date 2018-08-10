project('oomd', 'cpp',
  version : '0.1.0',
  license : 'GPL2')

# '..' is needed as an include directory b/c of how internal FB tooling
# requires a specific header path
inc = include_directories('''
    include
    shared
    util
    .
    ..
'''.split())

srcs = files('''
    Config.cpp
    Config.h
    Log.cpp
    Log.h
    Oomd.cpp
    Oomd.h
    OomDetector.cpp
    OomDetector.h
    OomKiller.cpp
    OomKiller.h
    PluginRegistry.cpp
    PluginRegistry.h
    plugins/OomKillerNoop.cpp
    plugins/OomKillerNoop.h
    plugins/Plugins.h
    shared/KillList.h
    shared/OomdContext.cpp
    shared/OomdContext.h
    shared/Tunables.cpp
    shared/Tunables.h
    util/Fs.h
    util/ScopeGuard.h
'''.split())

deps = [dependency('jsoncpp'),
        dependency('threads')]
oomd_lib = library('oomd',
                   srcs,
                   include_directories : inc,
                   dependencies : deps)
executable('oomd_bin',
           files('Main.cpp'),
           include_directories : inc,
           dependencies : deps,
           link_with : oomd_lib)

gtest_dep = dependency('gtest', main : true, required : false)
gmock_dep = dependency('gmock', required : false)
if gtest_dep.found() and gmock_dep.found()
    deps += [gtest_dep, gmock_dep]
    config_tests = executable('oomd_config_tests',
                              'ConfigTest.cpp',
                              include_directories : inc,
                              dependencies : deps,
                              link_with : oomd_lib)
    detector_tests = executable('oomd_detector_tests',
                                'OomDetectorTest.cpp',
                                include_directories : inc,
                                dependencies : deps,
                                link_with : oomd_lib)
    killer_tests = executable('oomd_killer_tests',
                              'OomKillerTest.cpp',
                              include_directories : inc,
                              dependencies : deps,
                              link_with : oomd_lib)
    oomd_tests = executable('oomd_oomd_tests',
                            'OomdTest.cpp',
                            include_directories : inc,
                            dependencies : deps,
                            link_with : oomd_lib)
    context_tests = executable('oomd_context_tests',
                            files('shared/OomdContextTest.cpp'),
                            include_directories : inc,
                            dependencies : deps,
                            link_with : oomd_lib)
    util_tests = executable('oomd_util_tests',
                            'util/FsTest.cpp',
                            'util/ScopeGuardTest.cpp',
                            include_directories : inc,
                            dependencies : deps,
                            link_with : oomd_lib)
    log_tests = executable('oomd_log_tests',
                            'LogTest.cpp',
                            include_directories : inc,
                            dependencies : deps,
                            link_with : oomd_lib)
    test('context_tests', context_tests)
    test('util_tests', util_tests)
    test('config_tests', config_tests)
    test('detector_tests', detector_tests)
    test('killer_tests', killer_tests)
    test('oomd_tests', oomd_tests)
    test('log_tests', log_tests)

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
