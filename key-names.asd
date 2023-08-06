(asdf:defsystem "key-names"
  :depends-on (:cl-svg :vicentino-tunings :alexandria :local-time)
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "constant-strings")
               (:file "database-handling")
               (:file "latex-backend")
               (:file "tuning-systems")
               (:file "main")))
