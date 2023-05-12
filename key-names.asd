(asdf:defsystem "key-names"
  :depends-on (:cl-svg)
  :serial t
  :components ((:file "key-names")
               (:file "database-handling")
               (:file "latex-backend")
               (:file "tuning-systems")
               (:file "main")))
