#include "Morpheme.hpp"

#include "Tag.hpp"

#include <string>
#include <vector>

namespace Apertium {
Morpheme::operator std::wstring() const {
  std::wstring Morpheme_ = TheLemma;

  for (std::vector<Tag>::const_iterator TagIterator = TheTags.begin();
       TagIterator != TheTags.end(); ++TagIterator) {
    Morpheme_ += *TagIterator;
  }

  return Morpheme_;
}
}
