#ifndef TAGGING_EXPRESSION_HPP
#define TAGGING_EXPRESSION_HPP

#include "Analysis.hpp"

#include <ostream>
#include <string>
#include <vector>

namespace Apertium {
class LexicalUnit {
public:
  operator std::wstring() const;
  std::wstring TheSurfaceForm;
  std::vector<Analysis> TheAnalyses;
};
}

#endif // LEXICAL_UNIT_HPP
