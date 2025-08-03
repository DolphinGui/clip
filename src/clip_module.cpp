module;
#define CLIP_IMPORT
#include "clip/clip.hpp"
#include "clip/completions/zsh.hpp"

export module clip;

export namespace clip {
using ::Argument;
using ::Parser;
using ::tuplet;
using ::user_out_type;
using ::UserParsable;
namespace comp {
using ::zsh_completer;
}
} // namespace clip
