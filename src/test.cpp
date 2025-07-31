#include <clip/clip.hpp>
#include <print>

using clip::Argument;
using clip::none;
using clip::Parser;

constexpr auto subcommand =
    Parser<"search", "runs search functions">{}
        .arg(Argument<std::string, none, "FILE", "file to be searched">{})
        .arg(Argument<std::vector<int>, "l", "lines", "line numbers">{});

constexpr auto parser =
    Parser<>{}
        .arg(Argument<bool, none, "verbose", "increases verbosity", false>{})
        .arg(Argument<std::optional<std::string>, "o", "output",
                      "output file">{})
        .arg(subcommand);

int main(int argc, char **argv) {
  auto g = parser.parse(argc, argv);
  auto &[verbose, output, search] = g;
  auto [file, lines] = search.value_or({});
  std::println("results: {}, {}, {}, {}", verbose, output.value_or("null"),
               file, lines);
}
