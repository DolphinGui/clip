#pragma once

#ifndef CLIP_IMPORT
#include <type_traits>
#else
import std;
#endif

namespace clip {

template <template <typename...> class Template, typename T>
struct is_instantiation_of : std::false_type {};

template <template <typename...> class Template, typename... Args>
struct is_instantiation_of<Template, Template<Args...>> : std::true_type {};

template <bool, typename True, typename False> struct if_else_impl;
template <bool b, typename True, typename False>
using if_else = if_else_impl<b, True, False>::type;

template <typename True, typename False>
struct if_else_impl<true, True, False> : std::type_identity<True> {};
template <typename True, typename False>
struct if_else_impl<false, True, False> : std::type_identity<False> {};

} // namespace clip
