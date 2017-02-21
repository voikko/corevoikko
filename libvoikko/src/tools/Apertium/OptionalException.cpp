#include "OptionalException.hpp"

namespace Apertium {
OptionalException::~OptionalException() throw() {}

const char *OptionalException::what() const throw() {
  return "can't dereference Optional comprising null OptionalType pointer";
}
}
