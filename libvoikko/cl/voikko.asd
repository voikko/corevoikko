;;;; A Common Lisp interface for libvoikko

(defsystem :voikko
  :description "Interface for Voikko library (libvoikko)"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "MPL 1.1 or GPL 2+ or LGPL 2.1+"
  :depends-on (:cffi)
  :serial t
  :components ((:file "packages")
               (:file "constants")
               (:file "general")
               (:file "options")
               (:file "spell")
               (:file "analyze")))
