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
