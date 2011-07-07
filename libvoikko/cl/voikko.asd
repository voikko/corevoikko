(defsystem :voikko
  :depends-on (:cffi)
  :serial t
  :components ((:file "packages")
               (:file "constants")
               (:file "general")
               (:file "options")
               (:file "spell")
               (:file "analyze")))
