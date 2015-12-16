#ifndef ANALYSIS_HPP
#define ANALYSIS_HPP

#include "Morpheme.hpp"

#include <ostream>
#include <string>
#include <vector>

namespace Apertium {
class Analysis {
public:
  std::vector<Morpheme> TheMorphemes;
};
}

#endif // ANALYSIS_HPP
