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
#include <expected>
#include <format>
#include <iterator>
#include <optional>
#include <print>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
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
  char value[len + 1]{}; // literally just to avoid 0-len arrays
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
  constexpr std::string_view view() const {
    if constexpr (len == 0)
      return std::string_view();
    else
      return std::string_view(value, len);
  }
  constexpr explicit operator std::string() const { return str(); }
  constexpr explicit operator std::string_view() const { return view(); }
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

struct FormatOptions {
  size_t cols = 0, indent = 0;
};

constexpr auto none = str_const<0>();
constexpr bool positional = true;
constexpr bool flag = false;
template <typename T, bool pos = false, str_const sname = none,
          str_const lname = none, str_const about_s = none,
          bool req = !std::same_as<bool, T> &&
                     !is_instantiation_of<std::optional, T>::value &&
                     !is_instantiation_of<std::vector, T>::value>
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
  static_assert(has_short() || has_long(),
                "unnnamed arguments are not allowed");
  // by default, arguments with no short name are assumed to be positional
  constexpr static bool positional = pos;
  // flag values are assumed not to be required, and only optional values
  // are assumed to be optional
  constexpr static bool required = req;
};

struct _vArgument {
  std::string_view shortname;
  std::string_view longname;
  std::string_view about;
  bool positional, required;
};

template <str_const, str_const, str_const, typename...> struct Parser;

template <typename T> struct is_arg : std::false_type {};

template <typename T>
  requires(T::is_argument)
struct is_arg<T> : std::true_type {};

template <typename... T>
concept Arg = (... && is_arg<T>::value);

template <typename T> struct is_positional : std::false_type {};

template <typename T>
  requires(T::is_positional)
struct is_positional<T> : std::false_type {};

template <typename... T>
concept Positional = (... && is_positional<T>::value);

template <typename T> struct is_subcommand : std::false_type {};

template <typename T>
  requires(T::is_subcommand)
struct is_subcommand<T> : std::true_type {};

template <typename... T>
concept Sub = (... && is_subcommand<T>::value);

template <Arg... Args> auto _parse_tuple(std::vector<std::string_view> &args);

template <str_const shorthand = none, str_const name = none,
          str_const about_s = none, typename... Args>
struct Parser {
  template <Arg A> constexpr auto arg(A = {}) {
    return Parser<shorthand, name, about_s, Args..., A>{};
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

  constexpr static std::string help();
  constexpr static void output_help(auto it, size_t indentation);

  constexpr static bool is_argument = name;
  constexpr static bool is_subcommand = is_argument;
  constexpr static bool positional = true;
  constexpr static bool required = false;
  constexpr static auto long_name() { return name; }
  constexpr static bool has_long() { return name; }
  constexpr static auto short_name() { return shorthand; }
  constexpr static bool has_short() { return shorthand; }
  constexpr static auto about() { return about_s; }
  constexpr static bool has_about() { return about_s; }
  using type = decltype(Parser::parse(0, nullptr));
};

template <typename T>
concept UserParsable = requires(std::string_view sv) {
  { T::parse(sv) } -> std::convertible_to<T>;
};

template <typename T>
concept Parsable = std::same_as<T, bool> || std::is_arithmetic_v<T> ||
                   std::same_as<T, std::string> || UserParsable<T> ||
                   is_instantiation_of<std::optional, T>::value ||
                   is_instantiation_of<std::vector, T>::value ||
                   is_instantiation_of<std::variant, T>::value;

template <typename T>
concept UserHelpable =
    requires(std::back_insert_iterator<std::string> out, size_t indent) {
      { T::output_help(out, indent) } -> std::same_as<void>;
    };

template <typename T, typename... Ts>
std::expected<std::variant<T, Ts...>, std::string>
_parse_variant(std::string_view sv);

template <typename T>
std::expected<T, std::string> parse_string(std::string_view sv) {
  if constexpr (std::same_as<T, bool>) {
    if (sv == "true")
      return true;
    if (sv == "false")
      return false;
    return std::unexpected("Error parsing boolean");
  } else if constexpr (std::is_arithmetic_v<T>) {
    T value;
    auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), value);
    if (ptr != sv.data() + sv.size())
      return std::unexpected(std::format(
          "Error parsing number, not all characters could be parsed: {}", ptr));
    if (ec != std::errc{}) {
      auto err = std::make_error_condition(ec);
      return std::unexpected(
          std::format("Error parsing number: {}", err.message()));
    }
    return value;
  } else if constexpr (std::same_as<T, std::string>) {
    return std::string(sv);
  } else if constexpr (UserParsable<T>) {
    return T::parse(sv);
  } else if constexpr (is_instantiation_of<std::optional, T>::value) {
    using Inner = std::decay_t<decltype(std::declval<T>().value())>;
    return parse_string<Inner>(sv);
  } else if constexpr (is_instantiation_of<std::variant, T>::value) {
    static_assert(std::variant_size_v<T> > 0, "Cannot parse empty variant");
    return [&]<typename... Ts>(std::variant<Ts...>) {
      return _parse_variant<Ts...>(sv);
    }(T{});
  } else {
    static_assert(std::same_as<T, void>, "Unknown parse type");
  }
}

template <typename T, typename... Ts>
std::expected<std::variant<T, Ts...>, std::string>
_parse_variant(std::string_view sv) {
  auto result = parse_string<T>(sv);
  if (result) {
    return std::variant<T, Ts...>(std::move(*result));
  }
  if constexpr (sizeof...(Ts) == 0) {
    return std::unexpected<std::string>("Could not parse any variant types");
  } else {
    auto var = _parse_variant<Ts...>(sv);
    if (!var)
      return std::unexpected<std::string>(std::move(var.error()));
    else
      return std::visit(
          [](auto &&v) { return std::variant<T, Ts...>(std::move(v)); }, *var);
  }
}

template <Arg Argument>
auto _parse(std::vector<std::string_view> &args, size_t pos) {
  using T = Argument::type;
  bool has_parsed = false;
  T value = {};
  if constexpr (Argument::positional && !Sub<Argument>) {
    // positional parsing must always be done second
    auto a = args.begin() + pos;
    auto &arg = *a;
    if (arg.starts_with("-"))
      throw parse_error(Argument::long_name().str(),
                        std::format("Expected value, not flag {}", arg));
    auto result = parse_string<T>(arg);
    if (!result)
      throw parse_error(Argument::long_name().str(), result.error());
    args.erase(a);
    return *result;
  } else
    for (auto a = args.begin(); a < args.end(); ++a) {
      auto const &arg = *a;
      if constexpr (Sub<Argument>) {
        // subcommands swallow up all subsequent flags if the match
        if ((Argument::has_long() && Argument::long_name() == arg) ||
            (Argument::has_short() && Argument::short_name() == arg)) {
          auto arguments = std::vector<std::string_view>(
              std::make_move_iterator(a + 1), std::move_iterator(args.end()));
          args.erase(a, args.end());
          return Argument::parse(std::move(arguments));
        }
      } else {
        if (Argument::has_long() && arg.starts_with("--") &&
            arg.substr(2) == Argument::long_name()) {
          // must be a flag, set flag to true
          if constexpr (std::same_as<T, bool>) {
            a = args.erase(a) - 1;
            value = true;
            has_parsed = true;
            continue;
          }
        }

        if (Argument::has_short() && arg.starts_with("-")) {
          // do gnu-style flag combination parsing
          if constexpr (std::same_as<T, bool>) {
            if (arg.contains(Argument::short_name())) {
              a = args.erase(a) - 1;
              value = true;
              has_parsed = true;
              continue;
            }
          }
        }
        if ((Argument::has_short() &&
             arg.substr(1) == Argument::short_name()) ||
            (Argument::has_long() && arg.substr(2) == Argument::long_name())) {
          // not a flag, parse next string and remove them from the vector
          auto val = a + 1;
          if (val >= args.end()) {
            throw parse_error(Argument::long_name().str(),
                              "Expected value, found none");
          }
          if (val->starts_with("-"))
            throw parse_error(Argument::long_name().str(),
                              std::format("Expected value, not flag {}", *val));
          if constexpr (is_instantiation_of<std::vector, T>::value) {
            using Inner = std::decay_t<decltype(std::declval<T>()[0])>;
            auto result = parse_string<Inner>(*val);
            if (!result)
              throw parse_error(Argument::long_name().str(), result.error());
            value.push_back(*result);
            a = args.erase(a, a + 2) - 1;
            has_parsed = true;
            continue;
          } else {
            auto result = parse_string<T>(*val);
            if (!result)
              throw parse_error(Argument::long_name().str(), result.error());
            a = args.erase(a, a + 2) - 1;
            value = *result;
            has_parsed = true;
            continue;
          }
        }
      }
    }
  if (Argument::required && !has_parsed)
    throw argument_not_found(Argument::has_long()
                                 ? Argument::long_name().str()
                                 : Argument::short_name().str());
  return value;
}

template <typename... Ts> auto _apply_tuple(tuple<Ts...> t, auto &&f) {
  using Sequence = std::make_index_sequence<sizeof...(Ts)>;
  auto impl = [&]<size_t... is>(std::index_sequence<is...>) {
    return tuple(f(tuplet::get<is>(t))...);
  };
  return impl(Sequence{});
}

template <Arg... Args> auto _parse_tuple(std::vector<std::string_view> &args) {
  tuple<Args...> result = {};
  auto eval_subcommand = [&](auto &&prev_val) {
    using Argument = std::decay_t<decltype(prev_val)>;
    if constexpr (Sub<Argument>) {
      return _parse<Argument>(args, 0);
    } else {
      return prev_val;
    }
  };
  auto eval_nonpos = [&](auto &&prev_val) {
    using Argument = std::decay_t<decltype(prev_val)>;
    if constexpr (Arg<Argument> && !Positional<Argument>) {
      return _parse<Argument>(args, 0);
    } else {
      return prev_val;
    }
  };
  int pos = 0;
  auto eval_pos = [&](auto &&prev_val) {
    using Argument = std::decay_t<decltype(prev_val)>;
    if constexpr (!Arg<Argument>) {
      return prev_val;
    } else {
      return _parse<Argument>(args, pos++);
    }
  };

  return result.map(eval_subcommand).map(eval_nonpos).map(eval_pos);
}

template <typename T> constexpr std::string_view out_type();

template <typename T>
  requires(std::is_arithmetic_v<T>)
constexpr std::string_view out_type() {
  return "<NUM>";
}

template <typename T>
  requires(std::is_convertible_v<T, std::string>)
constexpr std::string_view out_type() {
  return "<STR>";
}

template <typename T>
  requires(is_instantiation_of<std::optional, T>::value)
constexpr std::string_view out_type() {
  using Inner = std::decay_t<decltype(std::declval<T>().value())>;
  return out_type<Inner>();
}

template <typename T>
  requires(is_instantiation_of<std::vector, T>::value)
constexpr std::string_view out_type() {
  using Inner = std::decay_t<decltype(std::declval<T>()[0])>;
  return out_type<Inner>();
}

// this is done to try to reduce binary bloat
// todo virtualize everything else to reduce binary
// bloat
void _generate_options_help(auto output_it, FormatOptions f,
                            std::string_view sh = {}, std::string_view l = {},
                            std::string_view about = {}) {
  size_t position = 0;
  auto pad = [&](size_t length) {
    for (size_t _ = 0; _ < length; ++_) {
      *output_it++ = ' ';
    }
    position += length;
  };
  auto out = [&]<size_t l>(std::string_view s) {
    output_it = std::copy(s.begin(), s.end(), output_it);
    position += l;
  };
  auto newline = [&] {
    *output_it++ = '\n';
    position = 0;
    pad(f.indent);
  };

  pad(f.indent);
  if (!sh.empty()) {
    out("-");
    out(sh);
  }
  if (!l.empty()) {
    if (!sh.empty()) {
      out(", ");
    }
    out("--");
    out(l);
  }
  // currently the about output algorithm doesn't really respect whitespace
  // in the about code
  if (!about.empty()) {
    size_t row_length = f.cols - position;
    size_t rows = about.size() / row_length + 1;
    // this is a herustic to increase horizontal space if
    // too many rows would be required
    if (rows > 2) {
      // todo check under windows if \r is necessary or not
      f.indent += 2;
      newline();
    } else {
      f.indent = position;
    }
    auto it = about.begin();
    while (it < about.end()) {
      // ignore leading whitespace
      while (std::isspace(*it))
        ++it;
      // find end of next word
      auto word_end = it;
      while (!std::isspace(*word_end))
        ++word_end;
      // word would not fit on screen
      if (std::distance(it, word_end) + position > f.cols) {
        newline();
      }
      out(std::string_view(it, word_end));
    }
  }
}

template <Arg A, Arg... As>
void _gen_option_help(auto output_it, FormatOptions o) {
  auto shortname = A::short_name().view(), longname = A::long_name().view(),
       about = A::about().view();
  _generate_options_help(output_it, o, shortname, longname, about);
  *output_it++ = '\n';
  _gen_option_help<As...>(output_it, o);
}

} // namespace clip
