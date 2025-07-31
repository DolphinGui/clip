#ifdef __cpp_modules
module;
#define EXPORT export
#else
#pragma once
#define EXPORT
#endif

#include "tuplet/tuple.hpp"
#include <charconv>
#include <concepts>
#include <format>
#include <iterator>
#include <optional>
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
  parse_error(std::string option, std::string what)
      : std::runtime_error(
            std::format("Error parsing option {}: {}", option, what)) {};
};

struct argument_not_found : std::runtime_error {
  argument_not_found(std::string option)
      : std::runtime_error(std::format("Argument {} not found", option)) {}
};

template <template <typename...> class Template, typename T>
struct is_instantiation_of : std::false_type {};

template <template <typename...> class Template, typename... Args>
struct is_instantiation_of<Template, Template<Args...>> : std::true_type {};

template <size_t len> struct str_const {
  char value[len + 1]{};
  constexpr str_const() = default;
  template <size_t strlen>
  constexpr str_const(const char (&lit)[strlen]) noexcept {
    for (size_t i = 0; i < len; ++i) {
      value[i] = lit[i];
    }
  }
  template <size_t begin, size_t end = len>
  constexpr str_const<end - begin> substr() const noexcept
    requires(end > begin && end <= len)
  {
    str_const<end - begin> result;
    for (size_t i = 0; i != end - begin; ++i) {
      result.value[i] = value[i + begin];
    }
    return result;
  }
  constexpr char *begin() { return value; }
  constexpr char *data() { return value; }
  constexpr char *end() { return value + len; }
  constexpr bool operator==(str_const<len> s) const {
    for (size_t i = 0; i < len; ++i) {
      if (s[i] != value[i])
        return false;
    }
    return true;
  }
  template <size_t s>
    requires(s != len)
  constexpr bool operator==(str_const<s>) const {
    return false;
  }
  constexpr char operator[](size_t i) { return value[i]; };
  constexpr bool empty() const { return len == 0; }
  constexpr operator bool() const { return !empty(); }
  constexpr std::string str() const { return std::string(value, len); }
  constexpr static size_t length = len;
};
template <size_t len>
constexpr bool operator==(str_const<len> lhs, std::string_view rhs) {
  if (len != rhs.size())
    return false;
  for (size_t i = 0; i < len; ++i) {
    if (lhs[i] != rhs[i])
      return false;
  }
  return true;
}
template <size_t len>
constexpr bool operator==(std::string_view lhs, str_const<len> rhs) {
  return rhs == lhs;
}
template <size_t strlen>
str_const(const char (&lit)[strlen]) -> str_const<strlen - 1>;

constexpr auto none = str_const<0>();

template <typename T, str_const sname = none, str_const lname = none,
          str_const about_s = none, bool pos = sname.empty(),
          bool req = !std::same_as<bool, T> &&
                     !is_instantiation_of<std::optional, T>::value>
struct Argument {
  using type = T;
  static_assert(std::default_initializable<T>,
                "T must be default initializable");
  constexpr static bool is_argument = true;
  constexpr static auto short_name() { return sname; }
  constexpr static bool has_short() { return sname; }
  constexpr static auto long_name() { return lname; }
  constexpr static bool has_long() { return lname; }
  constexpr static auto about() { return about_s; }
  constexpr static bool has_about() { return about_s; }
  // by default, arguments with no short name are assumed to be positional
  constexpr static bool positional = pos;
  // flag values are assumed not to be required, and only optional values
  // are assumed to be optional
  constexpr static bool required = req;
};

template <str_const, str_const, typename...> struct Parser;

template <typename T> struct is_arg : std::false_type {};

template <typename T>
  requires(T::is_argument)
struct is_arg<T> : std::true_type {};

template <typename... T>
concept Arg = (... && is_arg<T>::value);

template <typename T> struct is_subcommand : std::false_type {};

template <typename T>
  requires(T::is_subcommand)
struct is_subcommand<T> : std::true_type {};

template <typename... T>
concept Sub = (... && is_subcommand<T>::value);

template <Arg... Args> auto _parse_tuple(std::vector<std::string_view> &args);

template <str_const name = none, str_const about_s = none, typename... Args>
struct Parser {
  template <Arg A> constexpr auto arg(A = {}) {
    return Parser<name, about_s, Args..., A>{};
  }
  constexpr static auto parse(std::vector<std::string_view> arguments) {
    auto n = _parse_tuple<Args...>(arguments);
    if (!arguments.empty())
      throw parse_error(std::string(arguments.front()), "Unknown option");
    if constexpr (is_subcommand) {
      return std::optional(n);
    } else {
      return n;
    }
  }
  constexpr static auto parse(int argc, char *argv[]) {
    return parse(std::vector<std::string_view>(argv + 1, argv + argc));
  }

  constexpr static bool is_argument = name;
  constexpr static bool is_subcommand = is_argument;
  constexpr static bool positional = true;
  constexpr static bool required = false;
  constexpr static auto long_name() { return name; }
  constexpr static bool has_long() { return name; }
  constexpr static auto about() { return about_s; }
  constexpr static bool has_about() { return about_s; }
  using type = decltype(Parser::parse(0, nullptr));
};

template <typename T>
concept Parsable = requires(std::string_view sv) {
  { T::parse(sv) } -> std::convertible_to<T>;
};

template <typename T>
concept Vectorish = is_instantiation_of<std::vector, T>::value;

template <typename T>
T parse_string(std::string_view sv, std::string_view option);

template <typename T, size_t size>
T parse_string(std::string_view sv, str_const<size> option) {
  if constexpr (std::same_as<T, bool>) {
    if (sv == "true")
      return true;
    if (sv == "false")
      return false;
    throw parse_error(option.str(), "Error parsing boolean");
  } else if constexpr (std::is_arithmetic_v<T>) {
    T value;
    auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), value);
    if (ptr != sv.data() + sv.size())
      throw parse_error(
          option.str(),
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
    return std::string(sv);
  } else if constexpr (Parsable<T>) {
    return T::parse(sv);
  } else if constexpr (is_instantiation_of<std::optional, T>::value) {
    using Inner = std::decay_t<decltype(std::declval<T>().value())>;
    return parse_string<Inner>(sv, option);
  } else {
    static_assert(std::same_as<T, void>, "Unknown parse type");
  }
}

template <Arg Argument>
auto _parse(std::vector<std::string_view> &args, int pos) {
  using T = Argument::type;
  T value = {};
  int p = 0;
  for (auto a = args.begin(); a < args.end(); ++a) {
    auto const &arg = *a;
    if constexpr (Sub<Argument>) {
      // subcommands swallow up all subsequent flags if the match
      if (arg == Argument::long_name()) {
        auto arguments = std::vector<std::string_view>(
            std::make_move_iterator(a + 1), std::move_iterator(args.end()));
        args.erase(a, args.end());
        return Argument::parse(std::move(arguments));
      }
    } else if constexpr (Argument::positional) {
      // positional parsing must always be done second
      if (p == pos) {
        if (arg.starts_with("-"))
          throw parse_error(Argument::long_name().str(),
                            std::format("Expected value, not flag {}", arg));
        value = parse_string<T>(arg, Argument::long_name());
        args.erase(a);
        return value;
      }
    } else {
      if (Argument::has_long() && arg.starts_with("--") &&
          arg.substr(2) == Argument::long_name()) {
        // must be a flag, set flag to true
        if constexpr (std::same_as<T, bool>) {
          value = true;
          args.erase(a);
          return value;
        } else {
          // not a flag, parse next string and remove them from the vector
          auto val = a + 1;
          if (val >= args.end()) {
            throw parse_error(Argument::long_name().str(),
                              "Expected value, found none");
          }
          if (val->starts_with("-"))
            throw parse_error(Argument::long_name().str(),
                              std::format("Expected value, not flag {}", *val));
          value = parse_string<T>(*val, Argument::long_name());
          args.erase(a, a + 2);
          return value;
        }
      }
      if (Argument::has_short() && arg.starts_with("-")) {
        // do gnu-style flag combination parsing
        if constexpr (std::same_as<T, bool>) {
          if (arg.contains(Argument::short_name())) {
            value = true;
            args.erase(a);
            return value;
          }
        } else if (arg.substr(1) == Argument::short_name()) {
          // not a flag, parse next string and remove them from the vector
          auto val = a + 1;
          if (val >= args.end()) {
            throw parse_error(Argument::long_name().str(),
                              "Expected value, found none");
          }
          if (val->starts_with("-"))
            throw parse_error(Argument::long_name().str(),
                              std::format("Expected value, not flag {}", *val));
          value = parse_string<T>(*val, Argument::long_name());
          args.erase(a, a + 2);
          return value;
        }
      }
    }
    ++p;
  }
  if constexpr (Argument::required)
    throw argument_not_found(Argument::has_long()
                                 ? Argument::long_name().str()
                                 : Argument::short_name().str());
  return value;
}

template <Arg... Args> auto _parse_tuple(std::vector<std::string_view> &args) {
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
