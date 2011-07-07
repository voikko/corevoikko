;;;; A Common Lisp interface for libvoikko
;;
;; Copyright (C) 2011 Teemu Likonen <tlikonen@iki.fi>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; The license text: <http://www.gnu.org/licenses/gpl-2.0.html>

(in-package :voikko)

;; VOIKKO_SPELL_FAILED 0
;; VOIKKO_SPELL_OK 1
;; VOIKKO_INTERNAL_ERROR 2
;; VOIKKO_CHARSET_CONVERSION_FAILED 3

(defconstant +voikko-spell-failed+ 0)
(defconstant +voikko-spell-ok+ 1)
(defconstant +voikko-internal-error+ 2)
(defconstant +voikko-charset-conversion-failed+ 3)

;; VOIKKO_OPT_IGNORE_DOT 0
;; VOIKKO_OPT_IGNORE_NUMBERS 1
;; VOIKKO_OPT_IGNORE_UPPERCASE 3
;; VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE 6
;; VOIKKO_OPT_ACCEPT_ALL_UPPERCASE 7
;; VOIKKO_OPT_NO_UGLY_HYPHENATION 4
;; VOIKKO_OPT_OCR_SUGGESTIONS 8
;; VOIKKO_OPT_IGNORE_NONWORDS 10
;; VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS 11
;; VOIKKO_OPT_ACCEPT_MISSING_HYPHENS 12
;; VOIKKO_OPT_ACCEPT_TITLES_IN_GC 13
;; VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC 14
;; VOIKKO_OPT_HYPHENATE_UNKNOWN_WORDS 15
;; VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC 16

(defparameter *voikko-boolean-options*
  '((:ignore-dot . 0)
    (:ignore-numbers . 1)
    (:ignore-uppercase . 3)
    (:accept-first-uppercase . 6)
    (:accept-all-uppercase . 7)
    (:no-ugly-hyphenation . 4)
    (:ocr-suggestions . 8)
    (:ignore-nonwords . 10)
    (:accept-extra-hyphens . 11)
    (:accept-missing-hyphens . 12)
    (:accept-titles-in-gc . 13)
    (:accept-unfinished-paragraphs-in-gc . 14)
    (:hyphenate-unknown-words . 15)
    (:accept-bulleted-lists-in-gc . 16)))

;; VOIKKO_MIN_HYPHENATED_WORD_LENGTH 9
;; VOIKKO_SPELLER_CACHE_SIZE 17

(defparameter *voikko-integer-options*
  '((:min-hyphenated-word-length . 9)
    (:speller-cache-size . 17)))
