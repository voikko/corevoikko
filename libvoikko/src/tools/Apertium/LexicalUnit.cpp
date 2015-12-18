#include "LexicalUnit.hpp"

#include "Analysis.hpp"

#include <string>
#include <vector>

namespace Apertium {
LexicalUnit::operator std::wstring() const {
  if (TheAnalyses.size() == 0)
    return L'^' + TheSurfaceForm + L"/*" + TheSurfaceForm + L'$';

  std::wstring LexicalUnit_ = L'^' + TheSurfaceForm;

  for (std::vector<Analysis>::const_iterator AnalysisIterator =
           TheAnalyses.begin();
       AnalysisIterator != TheAnalyses.end(); ++AnalysisIterator) {
    LexicalUnit_ += L'/' + static_cast<std::wstring>(*AnalysisIterator);
  }

  return LexicalUnit_ + L'$';
}
}
