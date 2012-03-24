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

(defclass mor-analysis (foreign-object)
  ((list :initarg :list :reader mor-analysis-list)))

(defun mor-analysis-p (object)
  (typep object 'mor-analysis))

(defmethod free-foreign-resource ((object mor-analysis))
  (with-slots (address list) object
    (foreign-funcall "voikko_free_mor_analysis" :pointer address :void)
    (setf list nil)))

(defun analyze-word (instance word)
  (error-if-not-active-instance instance)
  (let ((address (foreign-funcall "voikkoAnalyzeWordCstr"
                                  :pointer (address instance)
                                  :string word
                                  :pointer)))

    (when (proper-pointer-p address)
      (make-instance 'mor-analysis :address address
                     :list (loop :for i :upfrom 0
                                 :for a := (mem-aref address :pointer i)
                                 :until (null-pointer-p a)
                                 :collect a)))))

(defun mor-analysis-key-values (mor-analysis)
  (let ((keys-ptr (foreign-funcall "voikko_mor_analysis_keys"
                                   :pointer mor-analysis
                                   :pointer)))

    (when (proper-pointer-p keys-ptr)
      (loop :for i :upfrom 0
            :for key := (mem-aref keys-ptr :string i)
            :while (stringp key)
            :for value-ptr := (foreign-funcall "voikko_mor_analysis_value_cstr"
                                               :pointer mor-analysis
                                               :string key :pointer)

            :collect (cons key (foreign-string-to-lisp value-ptr))
            :do (foreign-funcall "voikko_free_mor_analysis_value_cstr"
                                 :pointer value-ptr :void)))))

(defun analyze (instance word)
  "Return word analysis for WORD. The return value is a list of
different analysis for the word. Each analysis is a list of cons cells.
The car value of the cons cell is an analysis key (a string) and the cdr
value is the value for that key (a string).

INSTANCE must be an active Voikko instance, if not, a condition of type
NOT-ACTIVE-INSTANCE-ERROR is signaled."

  (let ((analysis (analyze-word instance word)))
    (when (and (mor-analysis-p analysis)
               (activep analysis))
      (unwind-protect (loop :for a :in (mor-analysis-list analysis)
                            :collect (mor-analysis-key-values a))
        (free-foreign-resource analysis)))))
