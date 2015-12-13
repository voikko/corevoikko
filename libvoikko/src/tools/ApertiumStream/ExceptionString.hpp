#ifndef EXCEPTIONSTRING_HPP
#define EXCEPTIONSTRING_HPP

#include <cstddef>
#include <string>

namespace Apertium {
class ExceptionString {
public:
  friend void swap(ExceptionString &A, ExceptionString &B);
  ExceptionString(const std::wstring &What);
  ExceptionString(const ExceptionString &ExceptionString_);
  ExceptionString &operator=(ExceptionString ExceptionString_);
  ~ExceptionString();
  operator const char *() const;

private:
  char *TheString;
  std::size_t TheSize;
};
}

#endif
