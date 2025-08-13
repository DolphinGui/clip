#pragma once

#ifndef CLIP_IMPORT
#include "tuplet/tuple.hpp"
#include <charconv>
#include <concepts>
#include <expected>
#include <format>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>
#else
import std;
// necessary bc tuplet has an overload for get
export import tuplet;
#endif

#include "clip/_impl/meta.hpp"
#include "clip/_impl/str_const.hpp"

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

enum struct Type { Flag, Positional, Subcommand };

template <typename T> struct is_arg : std::false_type {};

template <typename T>
  requires(T::is_argument)
struct is_arg<T> : std::true_type {};

template <typename T>
concept Arg = is_arg<T>::value;

struct FormatOptions {
  std::size_t cols = 0, indent = 0;
};

struct _vArgument;

constexpr inline auto none = str_const<0>();
constexpr inline bool positional = true;
constexpr inline bool flag = false;
template <typename T, bool pos = false, str_const sname = none,
          str_const lname = none, str_const about_s = none,
          bool req = !std::default_initializable<T>>
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
  constexpr static Type t = pos ? Type::Positional : Type::Flag;
  // flag values are assumed not to be required, and only optional values
  // are assumed to be optional
  constexpr static bool required = req;
  constexpr static _vArgument virtualize();
};

// The callback should _know_ the return type, and can just
// write through a void*
using ParseCallback = void (*)(void *, std::string_view);
struct _vArgument {
  std::string shortname = {};
  std::string longname = {};
  std::string about = {};
  std::string param = {};
  std::vector<_vArgument> children; // subcommand flags
  ParseCallback parser = {};
  Type t;

  inline bool has_short() const noexcept { return !shortname.empty(); }
  inline bool is_boolean() const noexcept { return param.empty(); }
};

template <typename T> constexpr std::string out_type();

template <typename T, bool pos, str_const sname, str_const lname,
          str_const about_s, bool req>
constexpr _vArgument
Argument<T, pos, sname, lname, about_s, req>::virtualize() {
  return {short_name().str(),
          long_name().str(),
          about().str(),
          out_type<T>(),
          {},
          {},
          pos ? Type::Positional : Type::Flag};
}

template <typename T, Arg A> struct DefaultResult : public std::optional<T> {
  using argument_type = A;
};

template <Arg... Args> auto _parse_tuple(std::vector<std::string> &args);

template <str_const shorthand = none, str_const name = "COMMAND",
          str_const about_s = "Describe command here", bool is_sub = false,
          typename... Args>
struct Parser {
  template <Arg A> constexpr auto arg(A = {}) {
    return Parser<shorthand, name, about_s, is_subcommand, Args..., A>{};
  }
  constexpr static auto parse(std::vector<std::string> arguments) {
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
    return parse(std::vector<std::string>(argv + 1, argv + argc));
  }

  constexpr static std::string help(FormatOptions);
  constexpr static void output_help(std::output_iterator<char const &> auto it,
                                    FormatOptions o);

  constexpr static bool is_argument = is_sub;
  constexpr static bool is_subcommand = is_sub;
  constexpr static bool required = false;
  constexpr static Type t = Type::Subcommand;
  constexpr static auto long_name() { return name; }
  constexpr static bool has_long() { return name; }
  constexpr static auto short_name() { return shorthand; }
  constexpr static bool has_short() { return shorthand; }
  constexpr static auto about() { return about_s; }
  constexpr static bool has_about() { return about_s; }
  using type = decltype(Parser::parse(0, nullptr));

  constexpr static _vArgument virtualize() {
    return {
        shorthand.str(),         name.str(), about_s.str(),    "",
        {Args::virtualize()...}, {},         Type::Subcommand,
    };
  }
};

constexpr inline auto help_arg =
    Argument<bool, flag, "h", "help", "Output command help", false>{};

template <str_const shorthand, str_const name,
          str_const about_s = "Describe command here">
using Subcommand = Parser<shorthand, name, about_s, true>;

template <str_const shorthand, str_const name,
          str_const about_s = "Describe command here">
using Command = Parser<shorthand, name, about_s, false>;

template <typename T>
concept UserParsable = requires(std::string_view sv) {
  { T::parse(sv) } -> std::convertible_to<T>;
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

template <typename T>
concept Sub = (T::t == Type::Subcommand);

template <typename T>
concept Flag = (T::t == Type::Flag);

template <typename T>
concept Pos = (T::t == Type::Positional);

template <Sub Subcommand> auto _parse_subcommand(std::vector<std::string> &);

template <Flag Flag> auto _parse_flag(std::vector<std::string> &);

template <Pos Positional> auto _parse_positional(std::vector<std::string> &);

template <Arg... Args> auto _parse_tuple(std::vector<std::string> &args) {
  tuple<Args...> result = {};
  auto eval_subcommand = [&](auto &&prev_val) {
    using Argument = std::decay_t<decltype(prev_val)>;
    if constexpr (Sub<Argument>) {
      return _parse_subcommand<Argument>(args);
    } else {
      return prev_val;
    }
  };
  auto eval_nonpos = [&](auto &&prev_val) {
    using Argument = std::decay_t<decltype(prev_val)>;
    if constexpr (Flag<Argument>) {
      return _parse_flag<Argument>(args);
    } else {
      return prev_val;
    }
  };
  auto eval_pos = [&](auto &&prev_val) {
    using Argument = std::decay_t<decltype(prev_val)>;
    if constexpr (Arg<Argument>) {
      return _parse_positional<Argument>(args);
    } else {
      return prev_val;
    }
  };

  return result.map(eval_subcommand).map(eval_nonpos).map(eval_pos);
}

template <Sub Argument> auto _parse_subcommand(std::vector<std::string> &args) {
  for (auto a = args.begin(); a < args.end(); ++a) {
    bool match = (Argument::has_long() && Argument::long_name() == *a) ||
                 (Argument::has_short() && Argument::short_name() == *a);
    if (match) {
      std::vector<std::string> arguments;
      auto begin = match ? a + 1 : a;
      arguments.reserve(args.end() - a);
      std::copy(begin, args.end(), std::back_inserter(arguments));
      args.erase(a, args.end());
      return Argument::parse(std::move(arguments));
    }
  }
  return typename Argument::type{};
}

template <Flag Argument> auto _parse_flag(std::vector<std::string> &args) {
  using T = Argument::type;
  bool has_parsed = false;
  T value = {};
  for (auto a = args.begin(); a < args.end(); ++a) {
    auto &arg = *a;
    if constexpr (std::same_as<T, bool>) {
      bool long_flag = Argument::has_long() && arg.starts_with("--") &&
                       arg.substr(2) == Argument::long_name();

      bool short_flag = Argument::has_short() && arg.starts_with("-") &&
                        arg.contains(Argument::short_name().str());
      bool short_flag_not_short = Argument::short_name().length != 1;
      bool short_flag_exact = arg.length() == 2;
      bool remove_short =
          (short_flag && (short_flag_not_short || short_flag_exact));

      if (long_flag || remove_short) {
        // must be a flag, set flag to true
        a = args.erase(a) - 1;
        value = true;
        has_parsed = true;
        continue;
      } else if (short_flag) {
        char l = Argument::short_name()[0];
        std::erase(arg, l);
        value = true;
        has_parsed = true;
        continue;
      }
    } else {
      bool long_arg = Argument::has_long() && arg.starts_with("--") &&
                      arg.substr(2) == Argument::long_name();
      bool short_arg = Argument::has_short() && arg.starts_with("-") &&
                       arg.substr(1) == Argument::short_name();
      if (short_arg || long_arg) {
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

template <Pos Argument> auto _parse_positional(std::vector<std::string> &args) {
  using T = Argument::type;
  if (args.empty())
    return T{};
  // positional parsing must always be done second
  auto a = args.begin();
  auto &arg = *a;
  if (arg.starts_with("-"))
    throw parse_error(Argument::long_name().str(),
                      std::format("Expected value, not flag {}", arg));
  auto result = parse_string<T>(arg);
  if (!result)
    throw parse_error(Argument::long_name().str(), result.error());
  args.erase(a);
  return *result;
}

template <typename T> constexpr std::string user_out_type() = delete;

template <typename T, typename... Ts>
constexpr std::string _variant_out_type(std::variant<T, Ts...>) {
  if constexpr (sizeof...(Ts) == 0) {
    return out_type<T>();
  } else {
    return out_type<T>() + "|" +
           _variant_out_type<Ts...>(std::variant<Ts...>{});
  }
}

template <typename T> constexpr std::string out_type() {
  if constexpr (std::same_as<T, bool>) {
    return "";
  }
  if constexpr (std::is_arithmetic_v<T>)
    return "NUM";
  else if constexpr (std::is_convertible_v<T, std::string>) {
    return "STR";
  } else if constexpr (is_instantiation_of<std::optional, T>::value) {
    using Inner = std::decay_t<decltype(std::declval<T>().value())>;
    return out_type<Inner>() + "?";
  } else if constexpr (is_instantiation_of<std::vector, T>::value) {
    using Inner = std::decay_t<decltype(std::declval<T>()[0])>;
    return out_type<Inner>();
  } else if constexpr (is_instantiation_of<std::variant, T>::value) {
    return _variant_out_type(T{});
  } else {
    return user_out_type<T>();
  }
}

// this is done to try to reduce binary bloat
// todo virtualize everything else to reduce binary
// bloat
void _generate_options_help(auto output_it, FormatOptions f,
                            _vArgument const &arg) {
  auto &[sh, l, about, param, _children, _parse, _t] = arg;
  std::size_t position = 0;
  auto pad = [&](std::size_t length) {
    output_it = std::fill_n(output_it, length, ' ');
    position += length;
  };
  auto out = [&](std::string_view s) {
    output_it = std::copy(s.begin(), s.end(), output_it);
    position += s.size();
  };
  auto newline = [&] {
    *output_it++ = '\n';
    position = 0;
    pad(f.indent);
  };

  pad(f.indent);
  if (!sh.empty()) {
    std::format_to(output_it, "-{}", sh);
  } else {
    pad(2);
  }
  if (!l.empty()) {
    if (sh.empty())
      std::format_to(output_it, "  --{:6} ", l);
    else
      std::format_to(output_it, ", --{:6} ", l);
  } else {
    pad(10);
  }
  std::format_to(output_it, "{:10}", param);

  // currently the about output algorithm doesn't really respect whitespace
  // in the about code
  if (!about.empty()) {
    out("   ");
    std::size_t row_length = f.cols - position;
    std::size_t rows = about.size() / row_length + 1;
    // this is a herustic to increase horizontal space if
    // too many rows would be required
    if (rows > 2) {
      // todo check under windows if \r is necessary or not
      f.indent += 8;
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
      while (!std::isspace(*word_end) && word_end < about.end())
        ++word_end;
      // word would not fit on screen
      if (std::distance(it, word_end) + position > f.cols) {
        newline();
      }
      out(std::string_view(it, word_end));
      out(" ");
      it = word_end;
    }
  }
}

template <str_const shorthand, str_const name, str_const about_s, bool is_sub,
          typename... Args>
constexpr void Parser<shorthand, name, about_s, is_sub, Args...>::output_help(
    std::output_iterator<char const &> auto it, FormatOptions o) {
  using std::fill_n;
  using This = Parser<shorthand, name, about_s, is_sub, Args...>;
  auto out = [&](std::string_view s) {
    it = std::copy(s.begin(), s.end(), it);
  };
  if (This::has_about()) {
    out(This::about().str());
    *it++ = '\n';
    *it++ = '\n';
  }
  _vArgument arguments[] = {Args::virtualize()...};

  out(name.str());

  for (_vArgument const &arg : arguments) {
    if (arg.t != Type::Positional)
      continue;
    out(" <");
    out(arg.longname);
    out(">");
  }

  *it++ = '\n';
  *it++ = '\n';

  if (((Args::t == Type::Positional) || ...)) {
    out("Arguments:\n");
    for (_vArgument const &arg : arguments) {
      if (arg.t != Type::Positional)
        continue;
      it = fill_n(it, o.indent + 2, ' ');
      std::format_to(it, "{:10}{}", arg.longname, arg.about);
      *it++ = '\n';
    }
    *it++ = '\n';
  }

  it = fill_n(it, o.indent, ' ');
  out("Usage: \n");
  o.indent += 2;
  for (_vArgument const &arg : arguments) {
    if (arg.t != Type::Flag)
      continue;
    _generate_options_help(it, o, arg);
    *it++ = '\n';
  }
}

template <str_const shorthand, str_const name, str_const about_s, bool is_sub,
          typename... Args>
constexpr std::string
Parser<shorthand, name, about_s, is_sub, Args...>::help(FormatOptions o) {
  std::string s;
  output_help(std::back_inserter(s), o);
  return s;
}

} // namespace clip
