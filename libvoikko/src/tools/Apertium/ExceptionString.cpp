#include "ExceptionString.hpp"

#include <climits>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <string>
#include <utility>

namespace Apertium {
void swap(ExceptionString &A, ExceptionString &B) {
  using std::swap;
  swap(A.TheString, B.TheString);
  swap(A.TheSize, B.TheSize);
}

ExceptionString::ExceptionString(const std::wstring &What) {
  std::size_t WhatSize = MB_LEN_MAX * What.size();
  TheSize = WhatSize + 1;
  TheString = new char[TheSize];
  std::wcstombs(TheString, What.c_str(), WhatSize);
}

ExceptionString::ExceptionString(const ExceptionString &ExceptionString_) {
  TheSize = ExceptionString_.TheSize;
  TheString = new char[TheSize];
  std::strncpy(TheString, ExceptionString_.TheString, TheSize - 1);
}

ExceptionString &ExceptionString::operator=(ExceptionString ExceptionString_) {
  swap(*this, ExceptionString_);
  return *this;
}

ExceptionString::~ExceptionString() { delete TheString; }

ExceptionString::operator const char *() const { return TheString; }
}
