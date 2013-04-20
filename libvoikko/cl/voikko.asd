;;;; A Common Lisp interface for libvoikko

(defsystem :voikko
  :description "Interface for Voikko library (libvoikko)"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "MPL 1.1 or GPL 2+ or LGPL 2.1+"
  :depends-on (:cffi)
  :components
  ((:file "packages")
   (:file "constants" :depends-on ("packages"))
   (:file "general" :depends-on ("packages"))
   (:file "options" :depends-on ("packages" "general"))
   (:file "spell" :depends-on ("packages" "general"))
   (:file "analyze" :depends-on ("packages" "general"))))
