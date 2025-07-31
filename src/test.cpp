#include "fmt/format.h"
#include <clip/clip.hpp>
#include <iostream>
#include <print>

using clip::Argument;
using clip::none;
using clip::Parser;

constexpr auto subcommand =
    Parser<[] { return "search"; }, [] { return "runs search functions"; }>{}
        .arg(Argument<std::string, none, [] { return "FILE"; },
                      [] { return "File that is searched"; }>{});

constexpr auto parser =
    Parser<>{}
        .arg(Argument<bool, none, [] { return "verbose"; },
                      [] { return "controls verbosity"; }, false>{})
        .arg(Argument<std::optional<std::string>, [] { return "o"; },
                      [] { return "output"; }, [] { return "output file"; }>{})
        .arg(subcommand);

int main(int argc, char **argv) {
  auto g = parser.parse(argc, argv);
  auto &[verbose, output, search] = g;
  auto [file] = search.value_or({});
  std::println("results: {}, {}, {}", verbose, output.value_or("null"), file);
}
