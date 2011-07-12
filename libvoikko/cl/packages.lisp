;;;; A Common Lisp interface for libvoikko

(defpackage :voikko
  (:use :cl :cffi)
  (:export :instance :initialize :terminate
           :with-instance :spell :suggest :hyphenate :version
           :set-option :analyze :split-word

           :voikko-error :initialize-error :internal-error
           :charset-conversion-error :hyphenation-error
           :not-active-instance-error))
