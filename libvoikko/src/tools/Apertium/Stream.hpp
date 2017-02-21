#ifndef STREAM_HPP
#define STREAM_HPP

#include "ExceptionString.hpp"
#include "Optional.hpp"
#include "StreamType.hpp"

#include <cstddef>
#include <exception>
#include <istream>
#include <sstream>
#include <string>

namespace Apertium {
class Stream {
public:
  class Exception;
  class UnexpectedEndOfFile;
  class UnexpectedPreviousCase;
  class UnexpectedCase;
  class UnexpectedCharacter;
  class UnexpectedLemma;
  Stream(std::wistream &CharacterStream_);
  StreamType getTheNextStreamType();

private:
  void appendCharacter(StreamType &StreamType_, std::wstring &Lemma,
                       const wchar_t &Character_);
  StreamType getEndOfFile(StreamType &StreamType_, std::wstring &Lemma,
                          const wchar_t &Character_);
  std::wstring getWhat(const std::wstringstream &Message);
  std::wistream &TheCharacterStream;
  class PreviousCase {
  public:
    void set(const wchar_t &Character_);
    Optional<wchar_t> ThePreviousCase;
    bool isPreviousCharacter;
  } ThePreviousCase;
  std::size_t TheLineNumber;
  std::wstring TheLine;
};

class Stream::Exception : public std::exception {
public:
  Exception(const std::wstring &What);
  ~Exception() throw();
  const char *what() const throw();

protected:
  ExceptionString What;
};

#define APERTIUM_STREAM_EXCEPTION(APERTIUM_STREAM_EXCEPTION_NAME)              \
  class Stream::APERTIUM_STREAM_EXCEPTION_NAME : public Stream::Exception {    \
  public:                                                                      \
    APERTIUM_STREAM_EXCEPTION_NAME(const std::wstring &What);                  \
    ~APERTIUM_STREAM_EXCEPTION_NAME() throw();                                 \
  };

APERTIUM_STREAM_EXCEPTION(UnexpectedEndOfFile)
APERTIUM_STREAM_EXCEPTION(UnexpectedPreviousCase)
APERTIUM_STREAM_EXCEPTION(UnexpectedCase)
APERTIUM_STREAM_EXCEPTION(UnexpectedCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedLemma)

#undef APERTIUM_STREAM_EXCEPTION
}

#endif // STREAM_HPP
