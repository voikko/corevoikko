;;;; A Common Lisp interface for libvoikko
;;
;; Copyright (C) 2011-2012 Teemu Likonen <tlikonen@iki.fi>
;;
;; The contents of this file are subject to the Mozilla Public License Version
;; 1.1 (the "License"); you may not use this file except in compliance with
;; the License. You may obtain a copy of the License at
;; http://www.mozilla.org/MPL/
;;
;; Software distributed under the License is distributed on an "AS IS" basis,
;; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;; for the specific language governing rights and limitations under the
;; License.
;;
;; Alternatively, the contents of this file may be used under the terms of
;; either the GNU General Public License Version 2 or later (the "GPL"), or
;; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
;; in which case the provisions of the GPL or the LGPL are applicable instead
;; of those above. If you wish to allow use of your version of this file only
;; under the terms of either the GPL or the LGPL, and not to allow others to
;; use your version of this file under the terms of the MPL, indicate your
;; decision by deleting the provisions above and replace them with the notice
;; and other provisions required by the GPL or the LGPL. If you do not delete
;; the provisions above, a recipient may use your version of this file under
;; the terms of any one of the MPL, the GPL or the LGPL.

(in-package #:voikko)

(defclass option ()
  ((id :initarg :id)
   (value :initarg :value)))
(defclass boolean-option (option) nil)
(defclass integer-option (option) nil)

(define-condition unknown-option-key-error (voikko-error) nil)
(define-condition invalid-value-type-error (voikko-error) nil)

(defun make-option (key value)
  (let (data)
    (cond ((setf data (assoc key *voikko-boolean-options*))
           (make-instance 'boolean-option :id (cdr data) :value value))
          ((setf data (assoc key *voikko-integer-options*))
           (make-instance 'integer-option :id (cdr data) :value value))
          (t (error 'unknown-option-key-error
                    :string (format nil "Unknown option key: ~S" key))))))

(defgeneric set-option-caller (instance option))

(defmethod set-option-caller :before ((instance instance) option)
  (declare (ignore option))
  (error-if-not-active-instance instance))

(defmethod set-option-caller ((instance instance) (option boolean-option))
  (with-slots (id value) option
    (let ((success (foreign-funcall "voikkoSetBooleanOption"
                                    :pointer (address instance)
                                    :int id :int (if value 1 0) :int)))
      (if (zerop success) nil t))))

(defmethod set-option-caller ((instance instance) (option integer-option))
  (with-slots (id value) option
    (unless (integerp value)
      (error 'invalid-value-type-error
             :string "Invalid value type. The value must be an integer."))
    (let ((success (foreign-funcall "voikkoSetIntegerOption"
                                    :pointer (address instance)
                                    :int id :int value :int)))
      (if (zerop success) nil t))))

(defun set-option (instance key value)
  "Set option for a Voikko instance. KEY is a keyword specifying the
option and VALUE is the associated value. Possible keys and values as
well as the default values are described below. INSTANCE must be an
active Voikko instance, if not, a condition of type
NOT-ACTIVE-INSTANCE-ERROR is signaled. For invalid KEYs and VALUEs
conditions of type UNKNOWN-OPTION-KEY-ERROR and INVALID-VALUE-TYPE-ERROR
are signalled, respectively.

:IGNORE-DOT (boolean, nil)

    Ignore dot at the end of the word (needed for use in some word
    processors). If this option is set and input word ends with a dot,
    spell checking and hyphenation functions try to analyze the word
    without the dot if no results can be obtained for the original form.
    Also with this option, string tokenizer will consider trailing dot
    of a word to be a part of that word.

:IGNORE-NUMBERS (boolean, nil)

    (Spell checking only) Ignore words containing numbers.

:IGNORE-UPPERCASE (boolean, nil)

    Accept words that are written completely in uppercase letters
    without checking them at all.

:ACCEPT-FIRST-UPPERCASE (boolean, t)

    Accept words even when the first letter is in uppercase (start of
    sentence etc.).

:ACCEPT-ALL-UPPERCASE (boolean, t)

    Accept words even when all of the letters are in uppercase. Note
    that this is not the same as IGNORE_UPPERCASE: with this option the
    word is still checked, only case differences are ignored.

:NO-UGLY-HYPHENATION (boolean, nil)

    Do not insert hyphenation positions that are considered to be ugly
    but correct.

:OCR-SUGGESTIONS (boolean, nil)

    Use suggestions optimized for optical character recognition
    software. By default suggestions are optimized for typing errors.

:IGNORE-NONWORDS (boolean, t)

    (Spell checking only): Ignore non-words such as URLs and email
    addresses.

:ACCEPT-EXTRA-HYPHENS (boolean, nil)

    (Spell checking only): Allow some extra hyphens in words. This
    option relaxes hyphen checking rules to work around some unresolved
    issues in the underlying morphology, but it may cause some incorrect
    words to be accepted. The exact behavior (if any) of this option is
    not specified.

:ACCEPT-MISSING-HYPHENS (boolean, nil)

    (Spell checking only): Accept missing hyphens at the start and end
    of the word. Some application programs do not consider hyphens to be
    word characters. This is reasonable assumption for many languages
    but not for Finnish. If the application cannot be fixed to use
    proper tokenisation algorithm for Finnish, this option may be used
    to tell libvoikko to work around this defect.

:ACCEPT-TITLES-IN-GC (boolean, nil)

    (Grammar checking only): Accept incomplete sentences that could
    occur in titles or headings. Set this option to true if your
    application is not able to differentiate titles from normal text
    paragraphs, or if you know that you are checking title text.

:ACCEPT-UNFINISHED-PARAGRAPHS-IN-GC (boolean, nil)

    (Grammar checking only): Accept incomplete sentences at the end of
    the paragraph. These may exist when text is still being written.

:HYPHENATE-UNKNOWN-WORDS (boolean, t)

    (Hyphenation only): Hyphenate unknown words.

:ACCEPT-BULLETED-LISTS-IN-GC (boolean, nil)

    (Grammar checking only): Accept paragraphs if they would be valid
    within bulleted lists.

:MIN-HYPHENATED-WORD-LENGTH (integer, 2)

    The minimum length for words that may be hyphenated. This limit is
    also enforced on individual parts of compound words.

:SPELLER-CACHE-SIZE (integer, 0)

    Size of the spell checker cache. This can be -1 (no cache) or >= 0 (
    size in bytes = 2^cache_size * (6544*sizeof(wchar_t) + 1008) )."

  (set-option-caller instance (make-option key value)))
