#include "ApertiumStream.hpp"

#include "Analysis.hpp"
#include "LexicalUnit.hpp"

#include <climits>
#include <cstdlib>

#include <istream>
#include <sstream>
#include <string>

namespace Apertium {
ApertiumStream::ApertiumStream(std::wistream &CharacterStream_)
    : TheCharacterStream(CharacterStream_), TheLineNumber(1) {}

Optional<LexicalUnit> ApertiumStream::getTheNextLexicalUnit() {
  bool IsEscaped = false;
  LexicalUnit TheLexicalUnit;
  std::wstring Lemma;

  while (!TheCharacterStream.eof()) {
    const wchar_t Character_ = TheCharacterStream.get();
    TheLine += Character_;

    if (IsEscaped) {
      appendCharacter(TheLexicalUnit, Lemma, Character_);
      IsEscaped = false;
      continue;
    }

    switch (Character_) {
    case L'\\':
      IsEscaped = true;
      break;
    case L'[':
      if (ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        case L']':
          break;
        case L'$':
          break;
        default:
          std::wstringstream Message;
          Message << L"unexpected '" << Character_ << L"' following '"
                  << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                  << L"', '[' expected to follow ']' or '$'";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }
      }

      ThePreviousReservedCharacter.set(Character_);
      break;
    case L']':
      if (!ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_
                << L"', ']' expected to follow '['";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
      case L'[':
        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', ']' expected to follow '['";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      ThePreviousReservedCharacter.set(Character_);
      break;
    case L'^':
      if (ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        case L'[':
          continue;
        case L']':
          break;
        case L'$':
          break;
        default:
          std::wstringstream Message;
          Message << L"unexpected '" << Character_ << L"' following '"
                  << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                  << L"', '^' expected to follow '[', ']', or '$'";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }
      }

      ThePreviousReservedCharacter.set(Character_);
      break;
    case L'/':
      if (!ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_
                << L"', '/' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '^' or '#' not immediately";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
      case L'[':
        continue;
      case L'^':
        if (ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message << L"unexpected '" << Character_
                  << L"' immediately following '"
                  << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                  << L"', '/' expected to follow '[', to follow '>' "
                     L"immediately, or to follow '^' or '#' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      case L'>':
        if (!ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message << L"unexpected '" << Character_
                  << L"' not immediately following '"
                  << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                  << L"', '/' expected to follow '[', to follow '>' "
                     L"immediately, or to follow '^' or '#' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      case L'#':
        if (ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message << L"unexpected '" << Character_
                  << L"' immediately following '"
                  << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                  << L"', '/' expected to follow '[', to follow '>' "
                     L"immediately, or to follow '^' or '#' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', '/' expected to follow '[', to follow '>' "
                   L"immediately, or to follow '^' or '#' not immediately";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      TheLexicalUnit.TheAnalyses.push_back(Analysis());
      TheLexicalUnit.TheAnalyses.back().TheMorphemes.push_back(Morpheme());

      ThePreviousReservedCharacter.set(Character_);
      break;
    case L'*':
      if (ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        case L'[':
          continue;
        case L']':
          continue;
        case L'/':
          if (!ThePreviousReservedCharacter.isPreviousCharacter) {
            std::wstringstream Message;
            Message
                << L"unexpected '" << Character_
                << L"' not immediately following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', '*' expected to follow '[', ']', or '$' or to follow "
                   L"'/' immediately";
            throw UnexpectedReservedCharacter(getWhat(Message));
          }

          break;
        case L'$':
          continue;
        default:
          std::wstringstream Message;
          Message << L"unexpected '" << Character_ << L"' following '"
                  << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                  << L"', '*' expected to follow '[', ']', or '$' or to follow "
                     L"'/' immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        ThePreviousReservedCharacter.set(Character_);
      }

      break;
    case L'<':
      if (!ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_
                << L"', '<' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '/' or '+' not immediately";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
      case L'[':
        continue;
      case L'/':
        if (ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
              << L"', '<' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '/' or '+' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      case L'>':
        if (!ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_
              << L"' not immediately following '"
              << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
              << L"', '<' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '/' or '+' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      case L'+':
        if (ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
              << L"', '<' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '/' or '+' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', '<' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '/' or '+' not immediately";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      ThePreviousReservedCharacter.set(Character_);

      TheLexicalUnit.TheAnalyses.back().TheMorphemes.back().TheTags.push_back(
          Tag());
      break;
    case L'>':
      if (!ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"', '>' expected to "
                                                    L"follow '[' or to follow "
                                                    L"'<' not immediately";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
      case L'[':
        continue;
      case L'<':
        if (ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message << L"unexpected '" << Character_
                  << L"' immediately following '"
                  << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                  << L"', '>' expected to "
                     L"follow '[' or to follow "
                     L"'<' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', '>' expected to "
                   L"follow '[' or to follow "
                   L"'<' not immediately";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      ThePreviousReservedCharacter.set(Character_);
      break;
    case L'#':
      if (ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        case L'[':
          continue;
        case L']':
          continue;
        case L'>':
          if (!ThePreviousReservedCharacter.isPreviousCharacter) {
            std::wstringstream Message;
            Message
                << L"unexpected '" << Character_
                << L"' not immediately following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', '#' expected to follow '[', ']', or '$' or to follow "
                   L"'>' immediately";
            throw UnexpectedReservedCharacter(getWhat(Message));
          }

          break;
        case L'$':
          continue;
        default:
          std::wstringstream Message;
          Message << L"unexpected '" << Character_ << L"' following '"
                  << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                  << L"', '#' expected to follow '[', ']', or '$' or to follow "
                     L"'>' immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        ThePreviousReservedCharacter.set(Character_);
      }

      break;
    case L'+':
      if (ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        case L'[':
          continue;
        case L']':
          continue;
        case L'>':
          if (!ThePreviousReservedCharacter.isPreviousCharacter) {
            std::wstringstream Message;
            Message
                << L"unexpected '" << Character_
                << L"' not immediately following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', '+' expected to follow '[', ']', or '$', to follow '>' "
                   L"immediately, or to follow '#' not immediately";
            throw UnexpectedReservedCharacter(getWhat(Message));
          }

          break;
        case L'#':
          if (ThePreviousReservedCharacter.isPreviousCharacter) {
            std::wstringstream Message;
            Message
                << L"unexpected '" << Character_ << L"' immediately following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', '+' expected to follow '[', ']', or '$', to follow '>' "
                   L"immediately, or to follow '#' not immediately";
            throw UnexpectedReservedCharacter(getWhat(Message));
          }

          break;
        case L'$':
          continue;
        default:
          break;
        }

        ThePreviousReservedCharacter.set(Character_);

        TheLexicalUnit.TheAnalyses.back().TheMorphemes.push_back(Morpheme());
      }

      break;
    case L'$':
      if (!ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_
                << L"', '$' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '*' or '#' not immediately";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
      case L'[':
        continue;
      case L'*':
        if (ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
              << L"', '$' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '*' or '#' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        if (Lemma != TheLexicalUnit.TheSurfaceForm)
          throw;

        break;
      case L'>':
        if (!ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_
              << L"' not immediately following '"
              << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
              << L"', '$' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '*' or '#' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      case L'#':
        if (ThePreviousReservedCharacter.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
              << L"', '$' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '*' or '#' not immediately";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }

        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousReservedCharacter.ThePreviousReservedCharacter
                << L"', '$' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '*' or '#' not immediately";
        throw UnexpectedReservedCharacter(getWhat(Message));
      }

      ThePreviousReservedCharacter.set(Character_);

      return TheLexicalUnit;
      break;
    case L'\n':
      if (ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
        case L'[':
          break;
        case L']':
          break;
        case L'$':
          break;
        default:
          std::wstringstream Message;
          Message
              << L"unexpected '\\n', '\\n' expected to follow '[', ']', or '$'";
          throw UnexpectedReservedCharacter(getWhat(Message));
        }
      }

      ++TheLineNumber;
      TheLine.clear();
      break;
    default:
      appendCharacter(TheLexicalUnit, Lemma, Character_);
    }
  }

  if (ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
    switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
    case L']':
      break;
    case L'$':
      break;
    default:
      throw;
    }
  }

  return Optional<LexicalUnit>();
}

void ApertiumStream::appendCharacter(LexicalUnit &LexicalUnit_,
                                     std::wstring &Lemma,
                                     const wchar_t &Character_) {
  if (ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
    switch (*ThePreviousReservedCharacter.ThePreviousReservedCharacter) {
    case L'[':
      break;
    case L']':
      break;
    case L'^':
      LexicalUnit_.TheSurfaceForm.push_back(Character_);
      break;
    case L'/':
      LexicalUnit_.TheAnalyses.back().TheMorphemes.back().TheLemma.push_back(
          Character_);
      break;
    case L'*':
      Lemma.push_back(Character_);
      break;
    case L'<':
      static_cast<std::wstring>(
          LexicalUnit_.TheAnalyses.back().TheMorphemes.back().TheTags.back())
          .push_back(Character_);
      break;
    case L'>':
      throw;
    case L'#':
      LexicalUnit_.TheAnalyses.back().TheMorphemes.back().TheLemma.push_back(
          Character_);
      break;
    case L'+':
      LexicalUnit_.TheAnalyses.back().TheMorphemes.back().TheLemma.push_back(
          Character_);
      break;
    case L'$':
      break;
    default:
      throw;
    }
  }

  ThePreviousReservedCharacter.isPreviousCharacter = false;
}

std::wstring ApertiumStream::getWhat(const std::wstringstream &Message) {
  std::wstringstream What;
  What << TheLineNumber << L":" << TheLine.size() << L": " << Message.str()
       << L'\n' << TheLine << L'\n' << std::wstring(TheLine.size() - 1, L' ')
       << L'^';
  return What.str();
}

void ApertiumStream::PreviousReservedCharacter::set(const wchar_t &Character_) {
  ThePreviousReservedCharacter = Character_;
  isPreviousCharacter = true;
}

ApertiumStream::Exception::Exception(const std::wstring &What)
    : What(What) {}

ApertiumStream::Exception::~Exception() throw() {}

const char *ApertiumStream::Exception::what() const throw() { return What; }

#define APERTIUM_STREAM_EXCEPTION(APERTIUM_STREAM_EXCEPTION_HEAD_NAME)         \
  ApertiumStream::APERTIUM_STREAM_EXCEPTION_HEAD_NAME::                        \
      APERTIUM_STREAM_EXCEPTION_HEAD_NAME(const std::wstring &What)            \
      : ApertiumStream::Exception(What) {}                                     \
                                                                               \
  ApertiumStream::APERTIUM_STREAM_EXCEPTION_HEAD_NAME::                        \
      ~APERTIUM_STREAM_EXCEPTION_HEAD_NAME() throw() {}

APERTIUM_STREAM_EXCEPTION(UnexpectedEndOfFile)
APERTIUM_STREAM_EXCEPTION(UnexpectedReservedCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedLemma)
APERTIUM_STREAM_EXCEPTION(UnexpectedUnreservedCharacter)

#undef APERTIUM_STREAM_EXCEPTION
}