(asdf:defsystem "vicentino-key-names"
  :depends-on (:cl-svg :vicentino-tunings :alexandria :local-time)
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "database-handling")
               (:file "constant-strings")
               (:file "latex-backend")
               (:file "tuning-systems")
               (:file "main")))
