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

(in-package #:voikko)

(defconstant +voikko-spell-failed+ 0)
(defconstant +voikko-spell-ok+ 1)
(defconstant +voikko-internal-error+ 2)
(defconstant +voikko-charset-conversion-failed+ 3)

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

(defparameter *voikko-integer-options*
  '((:min-hyphenated-word-length . 9)
    (:speller-cache-size . 17)))
