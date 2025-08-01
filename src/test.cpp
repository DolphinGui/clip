#include <clip/clip.hpp>
#include <format>
#include <print>
#include <variant>

using clip::Argument;
using clip::flag;
using clip::none;
using clip::Parser;
using clip::positional;

constexpr auto subcommand =
    Parser<"se", "search", "runs search functions">{}
        .arg(Argument<std::string, positional, none, "FILE",
                      "file to be searched">{})
        .arg(Argument<std::vector<int>, flag, "l", "lines", "line numbers">{})
        .arg(Argument<std::variant<int, std::string>, flag, "v", "verbose">{});

constexpr auto parser =
    Parser<>{}
        .arg(subcommand)
        .arg(Argument<bool, flag, none, "verbose", "increases verbosity">{})
        .arg(Argument<std::optional<std::string>, flag, "o", "output",
                      "output file">{});

int main(int argc, char **argv) {
  auto g = parser.parse(argc, argv);
  auto &[search, verbose, output] = g;
  auto [file, lines, v2] = search.value_or({});
  std::println(
      "results: {}, {}, {}, {}, {}", verbose, output.value_or("null"), file,
      lines, std::visit([](auto &&arg) { return std::format("{}", arg); }, v2));
}
