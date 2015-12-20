#ifndef INFLECTIONAL_GROUP_HPP
#define INFLECTIONAL_GROUP_HPP

#include "Tag.hpp"

#include <string>
#include <vector>

namespace Apertium {
class Morpheme {
public:
  std::wstring TheLemma;
  std::vector<Tag> TheTags;
};
}

#endif // MORPHEME_HPP
