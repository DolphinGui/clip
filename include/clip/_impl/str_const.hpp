#pragma once

#ifndef CLIP_IMPORT
#include <cstdint>
#include <string>
#include <string_view>
#else
import std;
#endif

namespace clip {

template <std::size_t len> struct str_const {
  char value[len + 1]{}; // literally just to avoid 0-len arrays
  constexpr str_const() = default;
  template <std::size_t strlen>
  constexpr str_const(const char (&lit)[strlen]) noexcept {
    for (std::size_t i = 0; i < len; ++i) {
      value[i] = lit[i];
    }
  }
  template <std::size_t begin, std::size_t end = len>
  constexpr str_const<end - begin> substr() const noexcept
    requires(end > begin && end <= len)
  {
    str_const<end - begin> result;
    for (std::size_t i = 0; i != end - begin; ++i) {
      result.value[i] = value[i + begin];
    }
    return result;
  }
  constexpr char *begin() { return value; }
  constexpr char *data() { return value; }
  constexpr char *end() { return value + len; }
  constexpr bool operator==(str_const<len> s) const {
    for (std::size_t i = 0; i < len; ++i) {
      if (s[i] != value[i])
        return false;
    }
    return true;
  }
  template <std::size_t s>
    requires(s != len)
  constexpr bool operator==(str_const<s>) const {
    return false;
  }
  constexpr char operator[](std::size_t i) { return value[i]; };
  constexpr bool empty() const { return len == 0; }
  constexpr operator bool() const { return !empty(); }
  constexpr std::string str() const { return std::string(value, len); }
  constexpr explicit operator std::string() const { return str(); }
  constexpr static std::size_t length = len;
};
template <std::size_t len>
constexpr bool operator==(str_const<len> lhs, std::string_view rhs) {
  if (len != rhs.size())
    return false;
  for (std::size_t i = 0; i < len; ++i) {
    if (lhs[i] != rhs[i])
      return false;
  }
  return true;
}
template <std::size_t len>
constexpr bool operator==(std::string_view lhs, str_const<len> rhs) {
  return rhs == lhs;
}
template <std::size_t strlen>
str_const(const char (&lit)[strlen]) -> str_const<strlen - 1>;

} // namespace clip
