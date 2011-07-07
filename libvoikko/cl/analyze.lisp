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


(defclass mor-analysis (foreign-object)
  ((list :initarg :list :reader mor-analysis-list)))

(defmethod free-foreign-resource ((object mor-analysis))
  (with-slots (address list) object
    (foreign-funcall "voikko_free_mor_analysis" :pointer address :void)
    (setf list nil)))

(defun analyze-word (instance string)
  (error-if-not-active-instance instance)
  (let ((address (foreign-funcall "voikkoAnalyzeWordCstr"
                                  :pointer (address instance)
                                  :string string
                                  :pointer)))
    (when (proper-pointer-p address)
      (make-instance 'mor-analysis :address address
                     :list (loop for i upfrom 0
                                 for a = (mem-aref address :pointer i)
                                 until (null-pointer-p a)
                                 collect a)))))

(defun mor-analysis-key-values (mor-analysis)
  (let ((keys-ptr (foreign-funcall "voikko_mor_analysis_keys"
                                   :pointer mor-analysis
                                   :pointer)))

    (when (proper-pointer-p keys-ptr)
      (loop for i upfrom 0
            for key = (mem-aref keys-ptr :string i)
            while (stringp key)
            for value-ptr = (foreign-funcall "voikko_mor_analysis_value_cstr"
                                             :pointer mor-analysis
                                             :string key :pointer)

            collect (cons key (foreign-string-to-lisp value-ptr))
            do (foreign-funcall "voikko_free_mor_analysis_value_cstr"
                                :pointer value-ptr :void)))))

(defun analyze (instance string)
  (let ((analysis (analyze-word instance string)))
    (when (activep analysis)
      (unwind-protect (loop for a in (mor-analysis-list analysis)
                            collect (mor-analysis-key-values a))
        (free-foreign-resource analysis)))))
