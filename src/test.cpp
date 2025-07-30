#include "fmt/format.h"
#include <clip/clip.hpp>
#include <iostream>
#include <print>

using clip::Argument;
using clip::none;
using clip::Parser;

constexpr auto parser =
    Parser<Argument<bool, none, [] { return "verbose"; },
                    [] { return "controls verbosity"; }>>{};

int main(int argc, char **argv) {
  auto g = parser.parse(argc, argv);
  std::println("results: {}", tuplet::get<0>(g));
}
