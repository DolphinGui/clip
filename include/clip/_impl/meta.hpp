#pragma once

#ifndef CLIP_IMPORT
#include <type_traits>
#else
import std;
#endif

namespace clip::meta {

// literally just std::type_identity, but renamed
// so that errors are slightly more readable
template <typename T> struct returns {
  using type = T;
};

template <typename... Ts> struct tuple;

template <typename, typename> struct cat_m;
template <typename... T1, typename... T2>
struct cat_m<tuple<T1...>, tuple<T2...>> : returns<tuple<T1..., T2...>> {};
template <typename T1, typename T2> using cat = cat_m<T1, T2>::type;

template <typename, typename> struct push_m;
template <typename T1, typename... T2>
struct push_m<T1, tuple<T2...>> : returns<tuple<T1, T2...>> {};
template <typename T1, typename T2> using push = push_m<T1, T2>::type;

template <typename, typename> struct append_m;
template <typename T1, typename... T2>
struct append_m<T1, tuple<T2...>> : returns<tuple<T2..., T1>> {};
template <typename T1, typename T2> using append = append_m<T1, T2>::type;

template <std::size_t index, typename> struct get_m;

template <typename T, typename... Ts>
struct get_m<0, tuple<T, Ts...>> : returns<T> {};

template <std::size_t index, typename T, typename... Ts>
struct get_m<index, tuple<T, Ts...>>
    : returns<typename get_m<index - 1, tuple<Ts...>>::type> {};

template <std::size_t index, typename... Ts>
using get = get_m<index, Ts...>::type;

template <typename> struct sizeof_tuple_m;
template <typename... Ts>
struct sizeof_tuple_m<tuple<Ts...>>
    : std::integral_constant<std::size_t, sizeof...(Ts)> {};
template <typename Tup>
constexpr inline std::size_t sizeof_tuple = sizeof_tuple_m<Tup>::value;

template <template <typename...> class Template, typename T>
struct is_instantiation_of_m : std::false_type {};

template <template <typename...> class Template, typename... Args>
struct is_instantiation_of_m<Template, Template<Args...>> : std::true_type {};

template <template <typename...> class Template, typename T>
constexpr inline bool is_instantiation_of =
    is_instantiation_of_m<Template, T>::value;

template <bool, typename True, typename False> struct if_else_m;
template <bool b, typename True, typename False>
using if_else = if_else_m<b, True, False>::type;

template <typename True, typename False>
struct if_else_m<true, True, False> : returns<True> {};
template <typename True, typename False>
struct if_else_m<false, True, False> : returns<False> {};

} // namespace clip::meta
