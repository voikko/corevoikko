#include "Tag.hpp"

#include <string>

namespace Apertium {
Tag::operator std::wstring() const { return L'<' + TheTag + L'>'; }
}
