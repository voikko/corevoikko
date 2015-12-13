#ifndef APERTIUM_STREAM_HPP
#define APERTIUM_STREAM_HPP

#include "ExceptionString.hpp"
#include "LexicalUnit.hpp"
#include "Optional.hpp"

#include <cstddef>
#include <exception>
#include <istream>
#include <sstream>
#include <string>

namespace Apertium {
class ApertiumStream {
public:
  class Exception;
  class UnexpectedEndOfFile;
  class UnexpectedPreviousReservedCharacter;
  class UnexpectedReservedCharacter;
  class UnexpectedUnreservedCharacter;
  class UnexpectedLemma;
  ApertiumStream(std::wistream &CharacterStream_);
  Optional<LexicalUnit> getTheNextLexicalUnit();

private:
  void appendCharacter(LexicalUnit &LexicalUnit_, std::wstring &Lemma,
                       const wchar_t &Character_);
  std::wstring getWhat(const std::wstringstream &Message);
  std::wistream &TheCharacterStream;
  class PreviousReservedCharacter{
  public:
    void set(const wchar_t &Character_);
    Optional<wchar_t> ThePreviousReservedCharacter;
    bool isPreviousCharacter;
  } ThePreviousReservedCharacter;
  std::size_t TheLineNumber;
  std::wstring TheLine;
};

class ApertiumStream::Exception : public std::exception {
public:
  Exception(const std::wstring &What);
  ~Exception() throw();
  const char *what() const throw();

protected:
  ExceptionString What;
};

#define APERTIUM_STREAM_EXCEPTION(APERTIUM_STREAM_EXCEPTION_HEAD_NAME)         \
  class ApertiumStream::APERTIUM_STREAM_EXCEPTION_HEAD_NAME                    \
      : public ApertiumStream::Exception {                                     \
  public:                                                                      \
    APERTIUM_STREAM_EXCEPTION_HEAD_NAME(const std::wstring &What);             \
    ~APERTIUM_STREAM_EXCEPTION_HEAD_NAME() throw();                            \
  };

APERTIUM_STREAM_EXCEPTION(UnexpectedEndOfFile)
APERTIUM_STREAM_EXCEPTION(UnexpectedPreviousReservedCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedReservedCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedUnreservedCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedLemma)

#undef APERTIUM_STREAM_EXCEPTION
}

#endif // APERTIUM_STREAM_HPP
