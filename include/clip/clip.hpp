
#include <print>
#ifdef __cpp_modules
module;
#define EXPORT export
#else
#pragma once
#define EXPORT
#endif

#include "tuplet/tuple.hpp"
#include <algorithm>
#include <charconv>
#include <concepts>
#include <format>
#include <optional>
#include <ranges>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <vector>

#ifdef __cpp_modules
export module clip;
#endif

namespace clip {

using tuplet::tuple;

struct parse_error : std::runtime_error {
  parse_error(std::string_view option, std::string what)
      : std::runtime_error(
            std::format("Error parsing option {}: {}", option, what)) {};
};

struct argument_not_found : std::runtime_error {
  argument_not_found(std::string_view option)
      : std::runtime_error(std::format("Argument {} not found", option)) {}
};

template <template <typename...> class Template, typename T>
struct is_instantiation_of : std::false_type {};

template <template <typename...> class Template, typename... Args>
struct is_instantiation_of<Template, Template<Args...>> : std::true_type {};

constexpr auto none = [] { return std::string_view(); };

template <typename T, auto sname = none, auto lname = none, auto about_s = none,
          bool pos = !sname().empty()>
struct Argument {
  using type = T;
  static_assert(std::default_initializable<T>,
                "T must be default initializable");
  constexpr static bool is_argument = true;
  constexpr static std::string_view short_name() { return sname(); }
  constexpr static bool has_short() {
    return !std::string_view(sname()).empty();
  }
  constexpr static std::string_view long_name() { return lname(); }
  constexpr static bool has_long() {
    return !std::string_view(lname()).empty();
  }
  constexpr static std::string_view about() { return about_s(); }
  constexpr static bool has_about() {
    return !std::string_view(about_s()).empty();
  }
  constexpr static bool positional = pos;
  constexpr static bool required = is_instantiation_of<std::optional, T>::value;
};

template <typename...> struct Parser;

template <typename T> struct is_arg : std::false_type {};

template <typename T>
  requires(T::is_argument)
struct is_arg<T> : std::true_type {};

template <typename... T>
concept Arg = (... && is_arg<T>::value);

template <Arg... Args> auto _parse_tuple(std::vector<std::string_view> args);

template <typename... Args> struct Parser {
  template <Arg A> constexpr auto arg(A) { return Parser<Args..., A>{}; }
  constexpr auto parse(int argc, char *argv[]) const {
    auto arguments = std::vector<std::string_view>(argv, argv + argc);
    return _parse_tuple<Args...>(arguments);
  }
};

template <typename T>
concept Parsable = requires(std::string_view sv) {
  { T::parse(sv) } -> std::convertible_to<T>;
};

template <typename T>
concept Vectorish = is_instantiation_of<std::vector, T>::value;

template <typename T>
T parse_string(std::string_view sv, std::string_view option) {
  if constexpr (std::same_as<T, bool>) {
    if (sv == "true")
      return true;
    if (sv == "false")
      return false;
    throw parse_error(option, "Error parsing boolean");
  } else if constexpr (std::is_arithmetic_v<T>) {
    T value;
    auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), value);
    if (ptr != sv.data() + sv.size())
      throw parse_error(
          option,
          std::format(
              "Error parsing number, not all characters could be parsed: {}",
              ptr));
    if (ec != std::errc{}) {
      auto err = std::make_error_condition(ec);
      throw parse_error(option,
                        std::format("Error parsing number: {}", err.message()));
    }
    return value;
  } else if constexpr (std::same_as<T, std::string>) {
    return sv;
  } else if constexpr (Parsable<T>) {
    return T::parse(sv);
  } else {
    static_assert(false, "Unknown parse type");
  }
}

template <Arg Argument>
auto _parse(std::vector<std::string_view> &args, int pos) {
  using T = Argument::type;
  T value = {};
  int p = 0;
  for (auto a = args.begin() + 1; a < args.end(); ++a) {
    auto const &arg = *a;
    // positional parsing must always be done second
    if constexpr (Argument::positional) {
      if (p == pos) {
        if (arg.starts_with("-"))
          throw parse_error(Argument::long_name(),
                            std::format("Expected value, not flag {}", arg));
        value = parse_string<T>(arg, Argument::long_name());
        goto done;
      }
    } else {
      if (Argument::has_long() && arg.starts_with("--") &&
          arg.substr(2) == Argument::long_name()) {
        // must be a flag, set flag to true
        if constexpr (std::same_as<T, bool>) {
          value = true;
          goto done;
        } else {
          // not a flag, parse next string and remove them from the vector
          auto val = a + 1;
          if (val->starts_with("-"))
            throw parse_error(T::long_name(),
                              std::format("Expected value, not flag {}", *val));
          value = parse_string<T>(*val, Argument::long_name());
          goto done;
        }
      }
    }
    ++p;
  }
  if constexpr (Argument::required)
    throw argument_not_found(Argument::has_long() ? Argument::long_name()
                                                  : Argument::short_name());
done:
  return value;
}
template <Arg... Args> auto _parse_tuple(std::vector<std::string_view> args) {
  tuple<Args...> result = {};
  int pos = 0;
  auto eval_nonpos = [&](auto &&prev_val) {
    using Argument = std::decay_t<decltype(prev_val)>;
    if constexpr (!Argument::positional) {
      return _parse<Argument>(args, 0);
    } else {
      return prev_val;
    }
  };
  auto eval_pos = [&](auto &&prev_val) {
    using Argument = std::decay_t<decltype(prev_val)>;
    if constexpr (!Arg<Argument>) {
      return prev_val;
    } else {
      ++pos;
      return _parse<Argument>(args, pos - 1);
    }
  };

  return result.map(eval_nonpos).map(eval_pos);
}

} // namespace clip
