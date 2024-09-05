(defpackage :vicentino-key-names
  (:use :cl :vicentino-tunings)
  (:export generate-tex))

(in-package :vicentino-key-names)

(defparameter *tex-output-path*
  (merge-pathnames (asdf/system:system-source-directory :vicentino-key-names) "tex-output/"))
