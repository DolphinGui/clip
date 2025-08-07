project(
    'tuplet',
    'cpp',
    version: '2.1.1',
    meson_version: '>= 1.3.0',
    default_options: ['warning_level=3', 'cpp_std=c++20'],
)

tuplet_dep = declare_dependency(
    include_directories: include_directories('include'),
)

meson.override_dependency('tuplet', tuplet_dep)

install_headers(
    'include/tuplet/tuple.hpp',
    'include/tuplet/format.hpp',
    subdir: 'tuplet',
)


