(asdf:defsystem "key-names"
  :depends-on (:cl-svg :vicentino-tunings :alexandria)
  :serial t
  :components ((:file "key-names")
               (:file "tuning-systems")
               (:file "database-handling")
               (:file "latex-backend")
               (:file "main")))
