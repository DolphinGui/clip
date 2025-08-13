#include <catch2/catch_test_macros.hpp>

#include "clip/clip.hpp"
#include "clip/completions/zsh.hpp"

#include <concepts>
#include <iterator>
#include <optional>
#include <print>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#define TYPE_ASSERT(obj, type) static_assert(std::same_as<decltype(obj), type>)

using clip::Argument;
using clip::Command;
using clip::flag;
using clip::help_arg;
using clip::none;
using clip::Parser;
using clip::positional;
using clip::Subcommand;
using clip::comp::complete_zsh;

template <typename T> using Opt = std::optional<T>;
using Str = std::string;
using Args = std::vector<std::string>;

TEST_CASE("Basic flag and positional parsing") {
  constexpr auto cmd =
      Command<none, "basic">{}
          .arg(help_arg)
          .arg(Argument<Str, positional, none, "firstarg">{})
          .arg(Argument<Str, positional, none, "secondarg", none, false>{})
          .arg(Argument<bool, flag, "b", "binary">{})
          .arg(Argument<Opt<Str>, flag, "o", "output">{});
  SECTION("Parsing basic positional args") {
    Args as = {"first", "second"};
    auto [help, first, second, binary, output] = cmd.parse(as);
    TYPE_ASSERT(help, bool);
    TYPE_ASSERT(first, Str);
    TYPE_ASSERT(second, Str);
    TYPE_ASSERT(output, Opt<Str>);
    TYPE_ASSERT(binary, bool);

    REQUIRE(first == "first");
    REQUIRE(second == "second");
    REQUIRE(!output.has_value());
    REQUIRE(binary == false);
  }

  SECTION("Parsing flags and arguments at the same time") {
    Args as = {"-o", "out", "first", "second", "--binary"};
    auto [help, first, second, binary, output] = cmd.parse(as);

    REQUIRE(first == "first");
    REQUIRE(second == "second");
    REQUIRE(output.has_value());
    REQUIRE(output.value() == "out");
    REQUIRE(binary == true);
  }

  SECTION("Parsing invalid options") {
    Args a = {"-output"};
    REQUIRE_THROWS_AS(cmd.parse(a), clip::parse_error);
    Args b = {"-help"};
    REQUIRE_THROWS(cmd.parse(b));
    Args c = {"-fdas"};
    REQUIRE_THROWS(cmd.parse(c));
    Args d = {"--out"
              "--binary"};
    REQUIRE_THROWS_AS(cmd.parse(d), clip::parse_error);
  }

  SECTION("Parsing gnu-style flags") {
    Args a = {"-hbo", "file.txt"};
    auto [help, first, second, binary, output] = cmd.parse(a);
    REQUIRE(help == true);
    REQUIRE(first.empty());
    REQUIRE(output.value() == "file.txt");
    REQUIRE(second.empty());
    REQUIRE(binary == true);
  }
}

TEST_CASE("Subcommand parsing") {
  constexpr auto list = Subcommand<"l", "list", none>{}.arg(help_arg);
  constexpr auto search =
      Subcommand<"se", "search", none>{}
          .arg(help_arg)
          .arg(Argument<Opt<Str>, positional, none, "TERM">{})
          .arg(Argument<bool, flag, "r", "regex">{});
  constexpr auto cmd =
      Command<none, "cmd">{}.arg(help_arg).arg(list).arg(search);
  SECTION("Parsing just help") {
    Args as = {"--help"};
    auto [help, list, search] = cmd.parse(as);
    auto [l_help] = list.value_or({});
    auto [s_help, term, regex] = search.value_or({});
    TYPE_ASSERT(help, bool);
    TYPE_ASSERT(l_help, bool);
    TYPE_ASSERT(s_help, bool);
    TYPE_ASSERT(term, Opt<Str>);
    TYPE_ASSERT(regex, bool);

    REQUIRE(help == true);
    REQUIRE(!list.has_value());
    REQUIRE(!search.has_value());
  }

  SECTION("Parsing an empty subcommand") {
    Args as = {"l"};
    Args as2 = {"list"};
    auto [help, list, search] = cmd.parse(as);
    auto [l_help] = list.value_or({});
    REQUIRE(help == false);
    REQUIRE(list.has_value());
    REQUIRE(l_help == false);
    REQUIRE(!search.has_value());
    auto [_h, l, _s] = cmd.parse(as2);
    REQUIRE(l == list);
  }
}

TEST_CASE("Testing generation functions") {

  constexpr auto cmd = Command<none, "search", none>{}
                           .arg(help_arg)
                           .arg(Argument<Opt<Str>, positional, none, "TERM">{})
                           .arg(Argument<bool, flag, "r", "regex">{});

  SECTION("Testing help") {
    auto n = cmd.help({80});
    // actually testing the contents of help is kinda hard
    // so I just assume if it's non-empty it's probably fine
    REQUIRE(!n.empty());
  }

  SECTION("Testing completion") {
    Str result;
    complete_zsh(cmd, std::back_inserter(result));
    REQUIRE(!result.empty());
  }
}
