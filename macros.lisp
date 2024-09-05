(in-package :vicentino-key-names)

(defun make-comparison-expression (field value test)
  `(funcall ,test (getf item ,field) ,value))

(defun make-comparison-list (fields test)
  (loop while fields
        collecting (make-comparison-expression (pop fields) (pop fields) test)))
