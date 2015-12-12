#ifndef TAG_HPP
#define TAG_HPP

#include <ostream>
#include <string>

namespace Apertium {
class Tag {
public:
  operator std::wstring &();
private:
  std::wstring TheTag;
};
}

#endif // TAG_HPP
