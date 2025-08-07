#pragma once

#include "clip/clip.hpp"

#ifndef CLIP_IMPORT
#include <algorithm>
#include <format>
#else
import std;
#endif

namespace clip::comp {

template <typename T> struct zsh_completer;

inline auto _complete_to(auto out_it, std::string_view command,
                         std::span<const _vArgument> args, bool prologue)
    -> decltype(out_it);

template <str_const shorthand, str_const name, str_const about_s, bool is_sub,
          bool def, typename... Args>
struct zsh_completer<Parser<shorthand, name, about_s, is_sub, def, Args...>> {
  static_assert(!is_sub,
                "Completion scripts cannot be created for subarguments");

  static auto completion(auto output_iterator) -> decltype(output_iterator) {
    std::vector<_vArgument> args = {Args::virtualize()...};
    str_const n = name;
    return _complete_to(output_iterator, n.view(), args, true);
  }
};

inline auto _complete_to(auto out_it, std::string_view command,
                         std::span<const _vArgument> args, bool prologue)
    -> decltype(out_it) {

  if (prologue) {
    out_it = std::format_to(out_it, "#compdef _{0} {0}\ncompdef _{0} {0}\n",
                            command);
  }

  constexpr const char *begin_frag = R"(
function _{}(){{
    local line args
    function _subcommands {{
        local -a commands
        commands=(
 )";

  out_it = std::format_to(out_it, begin_frag, command);
  bool has_sub = false;
  for (auto const &arg : args) {
    if (arg.t != Type::Subcommand)
      continue;
    has_sub = true;
    if (arg.has_short()) {
      out_it = std::format_to(out_it, "'{}:{}'\n", arg.shortname, arg.about);
    }
    out_it = std::format_to(out_it, "'{}:{}'\n", arg.longname, arg.about);
  }

  constexpr const std::string_view end_sub = R"(
        )
        _describe 'command' commands
    }

    _arguments -C \
)";

  out_it = std::copy(end_sub.begin(), end_sub.end(), out_it);

  for (auto const &arg : args) {
    if (arg.t != Type::Flag)
      continue;
    std::string_view comp = "";
    if (!arg.is_boolean())
      comp = ": :";
    if (arg.has_short()) {
      out_it = std::format_to(out_it, "'-{}[{}]{}'  \\\n", arg.shortname,
                              arg.about, comp);
    }
    out_it = std::format_to(out_it, "'--{}[{}]{}'  \\\n", arg.longname,
                            arg.about, comp);
  }

  // Generally about is too long for completion message so use longname as
  // message instead Probably needs a place to add customization for completion
  // type
  int pos = 1;
  for (auto const &arg : args) {
    if (arg.t != Type::Positional)
      continue;
    out_it = std::format_to(out_it, "'{}:{}:'  \\\n", pos, arg.longname);
    ++pos;
  }

  if (has_sub) {
    out_it =
        std::format_to(out_it,
                       "':subcommand:_subcommands'  \\\n\"*::arg:->args\"\n  "
                       "_call_function - _{}_$line[1]\n",
                       command);
  }
  constexpr const std::string_view end = "}\n";
  out_it = std::copy(end.begin(), end.end(), out_it);
  if (has_sub) {
    for (auto const &sub : args) {
      if (sub.t != Type::Subcommand)
        continue;
      std::string name = std::format("{}_{}", command, sub.longname);
      out_it = _complete_to(out_it, name, sub.children, false);
    }
  }
  return out_it;
}
} // namespace clip::comp
