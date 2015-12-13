#include <ostream>
#include <string>

#include "Tag.hpp"

namespace Apertium {
Tag::operator std::wstring &() { return TheTag; }
}
