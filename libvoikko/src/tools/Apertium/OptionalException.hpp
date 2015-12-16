#include <exception>

namespace Apertium {
class OptionalException : public std::exception {
public:
  ~OptionalException() throw();
  const char *what() const throw();
};
}
