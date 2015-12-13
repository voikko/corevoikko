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
    : TheCharacterStream(CharacterStream_) {}

Optional<LexicalUnit> ApertiumStream::getTheNextLexicalUnit() {
  LexicalUnit TheLexicalUnit;

  std::wstring Lemma;

  bool IsEscaped = false;

  while (!TheCharacterStream.eof()) {
    ++ColumnNumber;

    const wchar_t Character_ = TheCharacterStream.get();

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
      if (!ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        ThePreviousReservedCharacter.set(Character_);
        break;
      }

      switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
      case L']':
        ThePreviousReservedCharacter.set(Character_);
        break;
      case L'$':
        ThePreviousReservedCharacter.set(Character_);
        break;
      default:
        throw;
      }

      break;
    case L']':
      if (!ThePreviousReservedCharacter.PreviousReservedCharacter_)
        throw;

      switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
      case L'[':
        ThePreviousReservedCharacter.set(Character_);
        break;
      default:
        throw;
      }
      break;
    case L'^':
      if (!ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        ThePreviousReservedCharacter.set(Character_);
        break;
      }

      switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
      case L'[':
        break;
      case L']':
        ThePreviousReservedCharacter.set(Character_);
        break;
      case L'$':
        ThePreviousReservedCharacter.set(Character_);
      default:
        throw;
      }

      break;
    case L'/':
      if (!ThePreviousReservedCharacter.PreviousReservedCharacter_)
        throw;

      switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
      case L'[':
        break;
      case L'^':
        if (ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        ThePreviousReservedCharacter.set(Character_);
        break;
      case L'>':
        if (!ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        ThePreviousReservedCharacter.set(Character_);
        break;
      case L'#':
        if (ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        ThePreviousReservedCharacter.set(Character_);
        break;
      default:
        throw;
      }

      TheLexicalUnit.TheAnalyses.push_back(Analysis());
      TheLexicalUnit.TheAnalyses.back().TheMorphemes.push_back(Morpheme());
      break;
    case L'*':
      if (ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        case L'[':
          break;
        case L']':
          break;
        case L'/':
          if (!ThePreviousReservedCharacter.isPreviousCharacter)
            throw;

          ThePreviousReservedCharacter.set(Character_);
          break;
        case L'$':
          break;
        default:
          throw;
        }
      }

      break;
    case L'<':
      if (!ThePreviousReservedCharacter.PreviousReservedCharacter_)
        throw;

      switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
      case L'[':
        break;
      case L'/':
        if (ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        ThePreviousReservedCharacter.set(Character_);
        break;
      case L'>':
        if (!ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        ThePreviousReservedCharacter.set(Character_);
      case L'+':
        if (ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        ThePreviousReservedCharacter.set(Character_);
        break;
      default:
        throw;
      }

      TheLexicalUnit.TheAnalyses.back().TheMorphemes.back().TheTags.push_back(
          Tag());
      break;
    case L'>':
      if (!ThePreviousReservedCharacter.PreviousReservedCharacter_)
        throw;

      switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
      case L'[':
        break;
      case L'<':
        if (ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        ThePreviousReservedCharacter.set(Character_);
        break;
      default:
        throw;
      }

      break;
    case L'#':
      if (ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        case L'[':
          break;
        case L']':
          break;
        case L'>':
          if (!ThePreviousReservedCharacter.isPreviousCharacter)
            throw;

          ThePreviousReservedCharacter.set(Character_);
          break;
        case L'$':
          break;
        default:
          throw;
        }
      }

      break;
    case L'+':
      if (ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        case L'[':
          break;
        case L']':
          break;
        case L'>':
          if (!ThePreviousReservedCharacter.isPreviousCharacter)
            throw;

          ThePreviousReservedCharacter.set(Character_);
          break;
        case L'#':
          if (ThePreviousReservedCharacter.isPreviousCharacter)
            throw;

          ThePreviousReservedCharacter.set(Character_);
          break;
        case L'$':
          break;
        default:
          break;
        }
      }

      TheLexicalUnit.TheAnalyses.back().TheMorphemes.push_back(Morpheme());
      break;
    case L'$':
      if (!ThePreviousReservedCharacter.PreviousReservedCharacter_)
        throw;

      switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
      case L'[':
        break;
      case L'*':
        if (ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        if (Lemma != TheLexicalUnit.TheSurfaceForm)
          throw;

        return TheLexicalUnit;
      case L'>':
        if (!ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        return TheLexicalUnit;
      case L'#':
        if (ThePreviousReservedCharacter.isPreviousCharacter)
          throw;

        return TheLexicalUnit;
      default:
        throw;
      }

      break;
    case L'\n':
      if (ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
        case L'[':
          break;
        case L']':
          break;
        case L'$':
          break;
        default:
          throw;
        }
      }

      ++LineNumber;
      ColumnNumber = 0;
      break;
    default:
      appendCharacter(TheLexicalUnit, Lemma, Character_);
    }
  }

  if (ThePreviousReservedCharacter.PreviousReservedCharacter_) {
    switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
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
  if (ThePreviousReservedCharacter.PreviousReservedCharacter_) {
    switch (*ThePreviousReservedCharacter.PreviousReservedCharacter_) {
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

ApertiumStream::Exception::Exception(const std::wstringstream &What)
    : What(What) {}

ApertiumStream::Exception::~Exception() throw() {}

const char *ApertiumStream::Exception::what() const throw() { return What; }

#define APERTIUM_STREAM_EXCEPTION(APERTIUM_STREAM_EXCEPTION_HEAD_NAME)         \
  ApertiumStream::APERTIUM_STREAM_EXCEPTION_HEAD_NAME::                        \
      APERTIUM_STREAM_EXCEPTION_HEAD_NAME(                                     \
          const std::wstringstream &WhatReference)                             \
      : ApertiumStream::Exception(WhatReference) {}                            \
                                                                               \
  ApertiumStream::APERTIUM_STREAM_EXCEPTION_HEAD_NAME::                        \
      ~APERTIUM_STREAM_EXCEPTION_HEAD_NAME() throw() {}

APERTIUM_STREAM_EXCEPTION(UnexpectedEndOfFile)
APERTIUM_STREAM_EXCEPTION(UnexpectedPreviousReservedCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedPreviousCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedCharacter)
APERTIUM_STREAM_EXCEPTION(UnexpectedLemma)

#undef APERTIUM_STREAM_EXCEPTION
}
