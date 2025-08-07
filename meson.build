project(
    'clip',
    'cpp',
    version: '0.1',
    meson_version: '>= 1.3.0',
    default_options: ['warning_level=3', 'cpp_std=c++23'],
)

catch2 = dependency('catch2-with-main')
tuplet = dependency('tuplet')

clip_dep = declare_dependency(
    include_directories: include_directories('include'),
)

test_exe = executable(
    'clip_test',
    'src/test.cpp',
    dependencies: [clip_dep, catch2, tuplet],
)

test('clip', test_exe)

meson.override_dependency('clip', clip_dep)

install_headers('include/clip/clip.hpp', subdir: 'clip')


