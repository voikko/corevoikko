;;;; A Common Lisp interface for libvoikko

(defpackage #:voikko
  (:use #:cl #:cffi)
  (:export #:instance #:activep #:initialize #:terminate
           #:with-instance #:spell #:suggest #:hyphenate #:version
           #:set-option #:analyze #:split-word #:list-dicts
           #:list-supported-spelling-languages
           #:list-supported-hyphenation-languages
           #:list-supported-grammar-checking-languages

           #:error-string
           #:voikko-error #:initialize-error #:internal-error
           #:charset-conversion-error #:hyphenation-error
           #:not-active-instance-error #:unknown-option-key-error
           #:invalid-value-type-error))
