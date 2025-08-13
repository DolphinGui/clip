module;

#define CLIP_IMPORT
#include "clip/clip.hpp"
#include "clip/completions/zsh.hpp"

export module clip;

export namespace clip {
using clip::Argument;
using clip::Command;
using clip::Parser;
using clip::Subcommand;

using clip::tuple;
using clip::user_out_type;
using clip::UserParsable;

using clip::flag;
using clip::help_arg;
using clip::none;
using clip::positional;

namespace literals {
using clip::literals::operator""_tag;
};

namespace comp {
using clip::comp::complete_zsh;
using clip::comp::Zsh;
} // namespace comp
} // namespace clip
