#include "Stream.hpp"

#include "Analysis.hpp"
#include "StreamType.hpp"

#include <climits>
#include <cstdlib>

#include <istream>
#include <sstream>
#include <string>

namespace Apertium {
Stream::Stream(std::wistream &CharacterStream_)
    : TheCharacterStream(CharacterStream_), TheLineNumber(1) {}

StreamType Stream::getTheNextStreamType() {
  StreamType TheStreamType;
  std::wstring Lemma;

  while (true) {
    const wchar_t Character_ = TheCharacterStream.get();

    if (TheCharacterStream.eof())
      break;

    TheLine += Character_;

    switch (Character_) {
    case L'\\': {
      appendCharacter(TheStreamType, Lemma, Character_);
      const wchar_t Character_ = TheCharacterStream.get();

      if (TheCharacterStream.eof()) {
        std::wstringstream Message;
        Message << L"unexpected end-of-file following '\\', end-of-file "
                   L"expected to follow ']' or '$'";
        throw UnexpectedEndOfFile(getWhat(Message));
      }

      TheLine += Character_;
      appendCharacter(TheStreamType, Lemma, Character_);
    } break;
    case L'[':
      if (ThePreviousCase.ThePreviousCase) {
        switch (*ThePreviousCase.ThePreviousCase) {
        case L']':
          break;
        case L'$':
          break;
        default:
          std::wstringstream Message;
          Message << L"unexpected '" << Character_ << L"' following '"
                  << *ThePreviousCase.ThePreviousCase
                  << L"', '[' expected to follow ']' or '$'";
          throw UnexpectedCase(getWhat(Message));
        }
      }

      appendCharacter(TheStreamType, Lemma, Character_);
      ThePreviousCase.set(Character_);
      break;
    case L']':
      if (!ThePreviousCase.ThePreviousCase) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_
                << L"', ']' expected to follow '['";
        throw UnexpectedCase(getWhat(Message));
      }

      switch (*ThePreviousCase.ThePreviousCase) {
      case L'[':
        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', ']' expected to follow '['";
        throw UnexpectedCase(getWhat(Message));
      }

      appendCharacter(TheStreamType, Lemma, Character_);
      ThePreviousCase.set(Character_);
      break;
    case L'^':
      if (ThePreviousCase.ThePreviousCase) {
        switch (*ThePreviousCase.ThePreviousCase) {
        case L'[':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        case L']':
          break;
        case L'$':
          break;
        default:
          std::wstringstream Message;
          Message << L"unexpected '" << Character_ << L"' following '"
                  << *ThePreviousCase.ThePreviousCase
                  << L"', '^' expected to follow '[', ']', or '$'";
          throw UnexpectedCase(getWhat(Message));
        }
      }

      TheStreamType.TheLexicalUnit = LexicalUnit();
      ThePreviousCase.set(Character_);
      break;
    case L'/':
      if (!ThePreviousCase.ThePreviousCase) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_
                << L"', '/' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '^' or '#' not immediately";
        throw UnexpectedCase(getWhat(Message));
      }

      switch (*ThePreviousCase.ThePreviousCase) {
      case L'[':
        appendCharacter(TheStreamType, Lemma, Character_);
        continue;
      case L'^':
        if (ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message << L"unexpected '" << Character_
                  << L"' immediately following '"
                  << *ThePreviousCase.ThePreviousCase
                  << L"', '/' expected to follow '[', to follow '>' "
                     L"immediately, or to follow '^' or '#' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      case L'>':
        if (!ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message << L"unexpected '" << Character_
                  << L"' not immediately following '"
                  << *ThePreviousCase.ThePreviousCase
                  << L"', '/' expected to follow '[', to follow '>' "
                     L"immediately, or to follow '^' or '#' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      case L'#':
        if (ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message << L"unexpected '" << Character_
                  << L"' immediately following '"
                  << *ThePreviousCase.ThePreviousCase
                  << L"', '/' expected to follow '[', to follow '>' "
                     L"immediately, or to follow '^' or '#' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '/' expected to follow '[', to follow '>' "
                   L"immediately, or to follow '^' or '#' not immediately";
        throw UnexpectedCase(getWhat(Message));
      }

      TheStreamType.TheLexicalUnit->TheAnalyses.push_back(Analysis());
      TheStreamType.TheLexicalUnit->TheAnalyses.back().TheMorphemes.push_back(Morpheme());
      ThePreviousCase.set(Character_);
      break;
    case L'*':
      if (ThePreviousCase.ThePreviousCase) {
        switch (*ThePreviousCase.ThePreviousCase) {
        case L'[':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        case L']':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        case L'/':
          if (!ThePreviousCase.isPreviousCharacter) {
            std::wstringstream Message;
            Message
                << L"unexpected '" << Character_
                << L"' not immediately following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '*' expected to follow '[', ']', or '$' or to follow "
                   L"'/' immediately";
            throw UnexpectedCase(getWhat(Message));
          }

          break;
        case L'$':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        default:
          std::wstringstream Message;
          Message << L"unexpected '" << Character_ << L"' following '"
                  << *ThePreviousCase.ThePreviousCase
                  << L"', '*' expected to follow '[', ']', or '$' or to follow "
                     L"'/' immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        ThePreviousCase.set(Character_);
        break;
      }

      appendCharacter(TheStreamType, Lemma, Character_);
      break;
    case L'<':
      if (!ThePreviousCase.ThePreviousCase) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_
                << L"', '<' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '/' or '+' not immediately";
        throw UnexpectedCase(getWhat(Message));
      }

      switch (*ThePreviousCase.ThePreviousCase) {
      case L'[':
        appendCharacter(TheStreamType, Lemma, Character_);
        continue;
      case L'/':
        if (ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousCase.ThePreviousCase
              << L"', '<' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '/' or '+' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      case L'>':
        if (!ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_
              << L"' not immediately following '"
              << *ThePreviousCase.ThePreviousCase
              << L"', '<' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '/' or '+' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      case L'+':
        if (ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousCase.ThePreviousCase
              << L"', '<' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '/' or '+' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '<' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '/' or '+' not immediately";
        throw UnexpectedCase(getWhat(Message));
      }

      TheStreamType.TheLexicalUnit->TheAnalyses.back().TheMorphemes.back().TheTags.push_back(
          Tag());
      ThePreviousCase.set(Character_);
      break;
    case L'>':
      if (!ThePreviousCase.ThePreviousCase) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"', '>' expected to "
                                                    L"follow '[' or to follow "
                                                    L"'<' not immediately";
        throw UnexpectedCase(getWhat(Message));
      }

      switch (*ThePreviousCase.ThePreviousCase) {
      case L'[':
        appendCharacter(TheStreamType, Lemma, Character_);
        continue;
      case L'<':
        if (ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message << L"unexpected '" << Character_
                  << L"' immediately following '"
                  << *ThePreviousCase.ThePreviousCase
                  << L"', '>' expected to "
                     L"follow '[' or to follow "
                     L"'<' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '>' expected to "
                   L"follow '[' or to follow "
                   L"'<' not immediately";
        throw UnexpectedCase(getWhat(Message));
      }

      ThePreviousCase.set(Character_);
      break;
    case L'#':
      if (ThePreviousCase.ThePreviousCase) {
        switch (*ThePreviousCase.ThePreviousCase) {
        case L'[':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        case L']':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        case L'>':
          if (!ThePreviousCase.isPreviousCharacter) {
            std::wstringstream Message;
            Message
                << L"unexpected '" << Character_
                << L"' not immediately following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '#' expected to follow '[', ']', or '$' or to follow "
                   L"'>' immediately";
            throw UnexpectedCase(getWhat(Message));
          }

          break;
        case L'$':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        default:
          std::wstringstream Message;
          Message << L"unexpected '" << Character_ << L"' following '"
                  << *ThePreviousCase.ThePreviousCase
                  << L"', '#' expected to follow '[', ']', or '$' or to follow "
                     L"'>' immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        ThePreviousCase.set(Character_);
        break;
      }

      appendCharacter(TheStreamType, Lemma, Character_);
      break;
    case L'+':
      if (ThePreviousCase.ThePreviousCase) {
        switch (*ThePreviousCase.ThePreviousCase) {
        case L'[':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        case L']':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        case L'>':
          if (!ThePreviousCase.isPreviousCharacter) {
            std::wstringstream Message;
            Message
                << L"unexpected '" << Character_
                << L"' not immediately following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '+' expected to follow '[', ']', or '$', to follow "
                   L"'>' "
                   L"immediately, or to follow '#' not immediately";
            throw UnexpectedCase(getWhat(Message));
          }

          break;
        case L'#':
          if (ThePreviousCase.isPreviousCharacter) {
            std::wstringstream Message;
            Message
                << L"unexpected '" << Character_ << L"' immediately following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '+' expected to follow '[', ']', or '$', to follow "
                   L"'>' "
                   L"immediately, or to follow '#' not immediately";
            throw UnexpectedCase(getWhat(Message));
          }

          break;
        case L'$':
          appendCharacter(TheStreamType, Lemma, Character_);
          continue;
        default:
          break;
        }

        TheStreamType.TheLexicalUnit->TheAnalyses.back().TheMorphemes.push_back(Morpheme());
        ThePreviousCase.set(Character_);
        break;
      }

      appendCharacter(TheStreamType, Lemma, Character_);
      break;
    case L'$':
      if (!ThePreviousCase.ThePreviousCase) {
        std::wstringstream Message;
        Message << L"unexpected '" << Character_
                << L"', '$' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '*' or '#' not immediately";
        throw UnexpectedCase(getWhat(Message));
      }

      switch (*ThePreviousCase.ThePreviousCase) {
      case L'[':
        appendCharacter(TheStreamType, Lemma, Character_);
        continue;
      case L'*':
        if (ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousCase.ThePreviousCase
              << L"', '$' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '*' or '#' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        if (Lemma != TheStreamType.TheLexicalUnit->TheSurfaceForm) {
          std::wstringstream Message;
          Message << L"unexpected lemma '" << Lemma << L"', expected '"
                  << TheStreamType.TheLexicalUnit->TheSurfaceForm << L"'";
          throw UnexpectedLemma(getWhat(Message));
        }

        break;
      case L'>':
        if (!ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_
              << L"' not immediately following '"
              << *ThePreviousCase.ThePreviousCase
              << L"', '$' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '*' or '#' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      case L'#':
        if (ThePreviousCase.isPreviousCharacter) {
          std::wstringstream Message;
          Message
              << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousCase.ThePreviousCase
              << L"', '$' expected to follow '[', to follow '>' immediately, "
                 L"or to follow '*' or '#' not immediately";
          throw UnexpectedCase(getWhat(Message));
        }

        break;
      default:
        std::wstringstream Message;
        Message << L"unexpected '" << Character_ << L"' following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '$' expected to follow '[', to follow '>' immediately, "
                   L"or to follow '*' or '#' not immediately";
        throw UnexpectedCase(getWhat(Message));
      }

      ThePreviousCase.set(Character_);
      return TheStreamType;
      break;
    case L'\n':
      if (!ThePreviousCase.ThePreviousCase)
        return getEndOfFile(TheStreamType, Lemma, Character_);

      switch (*ThePreviousCase.ThePreviousCase) {
      case L'[':
        break;
      case L']':
        return getEndOfFile(TheStreamType, Lemma, Character_);
      case L'$':
        return getEndOfFile(TheStreamType, Lemma, Character_);
      default:
        std::wstringstream Message;
        Message << L"unexpected '\\n' following '"
                << *ThePreviousCase.ThePreviousCase
                << L"', '\\n' expected to follow '[', ']', or '$'";
        throw UnexpectedCase(getWhat(Message));
      }

      appendCharacter(TheStreamType, Lemma, Character_);
      ++TheLineNumber;
      TheLine.clear();
      break;
    default:
      appendCharacter(TheStreamType, Lemma, Character_);
      break;
    }
  }

  if (ThePreviousCase.ThePreviousCase) {
    switch (*ThePreviousCase.ThePreviousCase) {
    case L']':
      break;
    case L'$':
      break;
    default:
      std::wstringstream Message;
      Message << L"unexpected end-of-file following '"
              << *ThePreviousCase.ThePreviousCase
              << L"', end-of-file expected to follow ']' "
                 L"or '$'";
      throw UnexpectedEndOfFile(getWhat(Message));
    }
  }

  return TheStreamType;
}

void Stream::appendCharacter(StreamType &StreamType_, std::wstring &Lemma,
                             const wchar_t &Character_) {
  if (ThePreviousCase.ThePreviousCase) {
    switch (*ThePreviousCase.ThePreviousCase) {
    case L'[':
      StreamType_.TheString += Character_;
      break;
    case L']':
      StreamType_.TheString += Character_;
      break;
    case L'^':
      StreamType_.TheLexicalUnit->TheSurfaceForm += Character_;
      break;
    case L'/':
      StreamType_.TheLexicalUnit->TheAnalyses.back().TheMorphemes.back().TheLemma.push_back(
          Character_);
      break;
    case L'*':
      Lemma += Character_;
      break;
    case L'<':
      StreamType_.TheLexicalUnit->TheAnalyses.back()
          .TheMorphemes.back()
          .TheTags.back()
          .TheTag += Character_;
      break;
    case L'>': {
      std::wstringstream Message;
      Message << L"unexpected '" << Character_ << L"' immediately following '"
              << *ThePreviousCase.ThePreviousCase
              << L"'";
      throw UnexpectedCharacter(getWhat(Message));
    }
    case L'#':
      StreamType_.TheLexicalUnit->TheAnalyses.back().TheMorphemes.back().TheLemma.push_back(
          Character_);
      break;
    case L'+':
      StreamType_.TheLexicalUnit->TheAnalyses.back().TheMorphemes.back().TheLemma.push_back(
          Character_);
      break;
    case L'$':
      StreamType_.TheString += Character_;
      break;
    default:
      std::wstringstream Message;
      Message << L"unexpected previous reserved or special character '"
              << *ThePreviousCase.ThePreviousCase
              << L"'";
      throw UnexpectedPreviousCase(getWhat(Message));
    }
  } else
    StreamType_.TheString += Character_;

  ThePreviousCase.isPreviousCharacter = false;
}

StreamType Stream::getEndOfFile(StreamType &StreamType_, std::wstring &Lemma,
                                const wchar_t &Character_) {
  appendCharacter(StreamType_, Lemma, Character_);

  {
    const wchar_t Character_ = TheCharacterStream.get();

    if (!TheCharacterStream.eof()) {
      TheLine += Character_;
      appendCharacter(StreamType_, Lemma, Character_);
      std::wstringstream Message;
      Message << L"unexpected '" << Character_
              << L"' following '\\n', end-of-file expected";
      throw UnexpectedCharacter(getWhat(Message));
    }

    return StreamType_;
  }
}

std::wstring Stream::getWhat(const std::wstringstream &Message) {
  std::wstringstream What;
  What << TheLineNumber << L":" << TheLine.size() << L": " << Message.str()
       << L'\n' << TheLine << L'\n' << std::wstring(TheLine.size() - 1, L' ')
       << L'^';
  return What.str();
}

void Stream::PreviousCase::set(const wchar_t &Character_) {
  ThePreviousCase = Character_;
  isPreviousCharacter = true;
}

Stream::Exception::Exception(const std::wstring &What) : What(What) {}

Stream::Exception::~Exception() throw() {}

const char *Stream::Exception::what() const throw() { return What; }

#define APERTIUM_STREAM_EXCEPTION(APERTIUM_STREAM_EXCEPTION_NAME)              \
  Stream::APERTIUM_STREAM_EXCEPTION_NAME::APERTIUM_STREAM_EXCEPTION_NAME(      \
      const std::wstring &What)                                                \
      : Stream::Exception(What) {}                                             \
                                                                               \
  Stream::APERTIUM_STREAM_EXCEPTION_NAME::                                     \
      ~APERTIUM_STREAM_EXCEPTION_NAME() throw() {}

APERTIUM_STREAM_EXCEPTION(UnexpectedEndOfFile)
APERTIUM_STREAM_EXCEPTION(UnexpectedPreviousCase)
APERTIUM_STREAM_EXCEPTION(UnexpectedCase)
APERTIUM_STREAM_EXCEPTION(UnexpectedCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedLemma)

#undef APERTIUM_STREAM_EXCEPTION
}
