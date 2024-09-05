(defpackage :vicentino-key-names
  (:use :cl :vicentino-tunings)
  (:export generate-tex))

(in-package :vicentino-key-names)


(defparameter *tex-output-path* "~/common-lisp/vicentino-key-names/tex-output/")
