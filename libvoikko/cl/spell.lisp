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


(defun spell (instance string)
  (error-if-not-active-instance instance)
  (let ((value (foreign-funcall "voikkoSpellCstr"
                                :pointer (address instance)
                                :string string
                                :int)))

    (ecase value
      (#.+voikko-spell-failed+ nil)
      (#.+voikko-spell-ok+ string)
      (#.+voikko-internal-error+
       (error 'internal-error :string "Internal error."))
      (#.+voikko-charset-conversion-failed+
       (error 'charset-conversion-error
              :string "Charset conversion error.")))))

(defun suggest (instance string)
  (error-if-not-active-instance instance)
  (let ((suggestions (foreign-funcall "voikkoSuggestCstr"
                                      :pointer (address instance)
                                      :string string
                                      :pointer)))

    (when (proper-pointer-p suggestions)
      (unwind-protect
           (loop for i upfrom 0
                 for sug = (mem-aref suggestions :string i)
                 while (stringp sug)
                 collect sug)
        (foreign-funcall "voikkoFreeCstrArray" :pointer suggestions :void)))))

(defun hyphenate (instance string)
  (error-if-not-active-instance instance)
  (let ((hyphenation (foreign-funcall "voikkoHyphenateCstr"
                                      :pointer (address instance)
                                      :string string
                                      :pointer)))

    (if (proper-pointer-p hyphenation)
        (unwind-protect
             (loop with answer = (foreign-string-to-lisp hyphenation)
                   with hyph
                   with start = 0
                   for pos upfrom 0
                   for h across answer

                   if (char= h #\=) do
                   (when (< start pos)
                     (push (subseq string start pos) hyph))
                   ;; (push (aref string pos) hyph)
                   (setf start (1+ pos))

                   else if (char= h #\-) do
                   (when (< start pos)
                     (push (subseq string start pos) hyph)
                     ;; (push :h hyph)
                     )
                   (setf start pos)

                   finally
                   (when (< start (length string))
                     (push (subseq string start) hyph))
                   (setf hyph (nreverse hyph))
                   (return (values hyph answer)))

          (foreign-funcall "voikkoFreeCstr" :pointer hyphenation :void))

        (error 'hyphenation-error :string "Hyphenation error."))))

(defun split-word (instance width string)
  (if (>= width (length string))
      (cons string "")
      (loop with hyph = (nth-value 1 (hyphenate instance string))
            with end1 = 0
            with start2 = 0

            for i from 0 below (length string)
            for h = (aref hyph i)
            while (<= i width)

            if (char= #\- h) do (setf end1 i start2 i)
            else if (char= #\= h) do (setf end1 i start2 (1+ i))

            finally (return (cons (subseq string 0 end1)
                                  (subseq string start2))))))
