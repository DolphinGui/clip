module;

#define CLIP_IMPORT
#include "clip/clip.hpp"
#include "clip/completions/zsh.hpp"

export module clip;

export namespace clip {
using clip::Argument;
using clip::Parser;

using clip::tuple;
using clip::user_out_type;
using clip::UserParsable;

using clip::flag;
using clip::none;
using clip::positional;

namespace comp {
using clip::comp::zsh_completer;
}
} // namespace clip
