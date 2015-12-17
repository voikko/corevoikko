#ifndef STREAM_TYPE_HPP
#define STREAM_TYPE_HPP

#include "LexicalUnit.hpp"
#include "Optional.hpp"

#include <string>

namespace Apertium {
class StreamType {
public:
  std::wstring TheString;
  Optional<LexicalUnit> TheLexicalUnit;
};
}

#endif // STREAM_TYPE_HPP
