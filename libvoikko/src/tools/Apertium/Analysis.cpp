#include "Analysis.hpp"

#include "Morpheme.hpp"

#include <string>
#include <vector>

namespace Apertium {
Analysis::operator std::wstring() const {
  std::wstring Analysis_;
  Analysis_ += TheMorphemes.front();

  for (std::vector<Morpheme>::const_iterator MorphemeIterator =
           ++(TheMorphemes.begin());
       MorphemeIterator != TheMorphemes.end(); ++MorphemeIterator) {
    Analysis_ += L'+' + static_cast<std::wstring>(*MorphemeIterator);
  }

  return Analysis_;
}
}
