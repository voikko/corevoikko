;;;; A Common Lisp interface for libvoikko

(defsystem :voikko
  :description "Interface for Voikko library (libvoikko)"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "The GNU General Public License version 2"
  :depends-on (:cffi)
  :serial t
  :components ((:file "packages")
               (:file "constants")
               (:file "general")
               (:file "options")
               (:file "spell")
               (:file "analyze")))
