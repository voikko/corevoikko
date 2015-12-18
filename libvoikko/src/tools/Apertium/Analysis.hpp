#ifndef ANALYSIS_HPP
#define ANALYSIS_HPP

#include "Morpheme.hpp"

#include <string>
#include <vector>

namespace Apertium {
class Analysis {
public:
  operator std::wstring() const;
  std::vector<Morpheme> TheMorphemes;
};
}

#endif // ANALYSIS_HPP
