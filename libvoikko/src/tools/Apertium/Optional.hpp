#ifndef OPTIONAL_HPP
#define OPTIONAL_HPP

#include "OptionalException.hpp"

#include <algorithm>
#include <cstddef>
#include <exception>
#include <new>

namespace Apertium {
template <typename OptionalType> class Optional;

template <typename OptionalType>
void swap(Optional<OptionalType> &A, Optional<OptionalType> &B);

template <typename OptionalType> class Optional {
public:
  friend void swap<OptionalType>(Optional &A, Optional &B);
  Optional();
  Optional(const OptionalType &OptionalType_);
  Optional(const Optional &Optional_);
  Optional &operator=(Optional Optional_);
  ~Optional();
  const OptionalType &operator*() const;
  OptionalType &operator*();
  const OptionalType *operator->() const;
  OptionalType *operator->();
  operator bool() const;

private:
  OptionalType *TheOptionalTypePointer;
};

template <typename OptionalType>
void swap(Optional<OptionalType> &A, Optional<OptionalType> &B) {
  using std::swap;
  swap(A.TheOptionalTypePointer, B.TheOptionalTypePointer);
}

template <typename OptionalType>
Optional<OptionalType>::Optional()
    : TheOptionalTypePointer(NULL) {}

template <typename OptionalType>
Optional<OptionalType>::Optional(const OptionalType &OptionalType_)
    : TheOptionalTypePointer(new OptionalType(OptionalType_)) {}

template <typename OptionalType>
Optional<OptionalType>::Optional(const Optional &Optional_) {
  if (Optional_.TheOptionalTypePointer == NULL) {
    TheOptionalTypePointer = NULL;
    return;
  }

  TheOptionalTypePointer =
      new OptionalType(*(Optional_.TheOptionalTypePointer));
}

template <typename OptionalType>
Optional<OptionalType> &Optional<OptionalType>::operator=(Optional Optional_) {
  swap(*this, Optional_);
  return *this;
}

template <typename OptionalType> Optional<OptionalType>::~Optional() {
  if (TheOptionalTypePointer == NULL)
    return;

  delete TheOptionalTypePointer;
}

template <typename OptionalType>
const OptionalType &Optional<OptionalType>::operator*() const {
  if (TheOptionalTypePointer == NULL)
    throw OptionalException();

  return *TheOptionalTypePointer;
}

template <typename OptionalType>
OptionalType &Optional<OptionalType>::operator*() {
  return const_cast<OptionalType &>(
      static_cast<const Optional &>(*this).operator*());
}

template <typename OptionalType>
const OptionalType *Optional<OptionalType>::operator->() const {
  if (TheOptionalTypePointer == NULL)
    throw OptionalException();

  return TheOptionalTypePointer;
}

template <typename OptionalType>
OptionalType *Optional<OptionalType>::operator->() {
  return const_cast<OptionalType *>(
      static_cast<const Optional &>(*this).operator->());
}

template <typename OptionalType> Optional<OptionalType>::operator bool() const {
  return TheOptionalTypePointer != NULL;
}
}

#endif
