(defpackage :voikko
  (:use :cl :cffi)
  (:export :instance :init :terminate
           :with-instance :spell :suggest :hyphenate :version
           :set-option :analyze

           :voikko-error :init-error :internal-error
           :charset-conversion-error :hyphenation-error
           :not-active-instance-error))
