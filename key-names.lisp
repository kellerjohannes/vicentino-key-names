(defpackage :key-names
  (:use :cl))

(in-package :key-names)


;;;; database handling

(defun make-key (libro chapter id folio original-name root ordine flag comment)
  (list :libro libro
        :chapter chapter
        :id id
        :folio folio
        :original-name original-name
        :root root
        :ordine ordine
        :flag flag
        :comment comment))

(defvar *keys* nil)

(defun add-key (key) (push key *keys*))

(defun dump-keys ()
  (format t "~&~{~{~a:~20t~a~%~}~%~}" *keys*))

(defun prompt-read-string (prompt)
  (format *query-io* "~&~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-integer (prompt)
  (or (parse-integer (prompt-read-string prompt) :junk-allowed t) 0))

(defun prompt-read-keyword (prompt)
  (intern (string-upcase (prompt-read-string prompt)) :keyword))

(defun prompt-for-key ()
  (make-key (prompt-read-integer "Libro [integer 0-5]")
            (prompt-read-integer "Capitolo [integer > 0]")
            (prompt-read-integer "Count / ID [integer > 0]")
            (let ((folio-number (prompt-read-integer "Folio number [integer > 0]"))
                  (suffix (prompt-read-string "Recto/verso [r/v]")))
              (cons folio-number (if (equal suffix "r") :recto :verso)))
            (prompt-read-string "Original name")
            (prompt-read-keyword "Root key [a-b]")
            (prompt-read-integer "Ordine [1-6]")
            (prompt-read-keyword "Flag [diplomatic strict relaxed]")
            (prompt-read-string "Comment")))

(defun input-keys ()
  (loop (add-key (prompt-for-key))
        (unless (y-or-n-p "Add another key? [y/n]: ") (return))))

(defun save-dbs ()
  (with-open-file (out "db-keys" :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *keys* out))))

(defun read-dbs ()
  (with-open-file (in "db-keys")
    (with-standard-io-syntax
      (setf *keys* (read in)))))

(defun make-comparison-expression (field value)
  `(equal (getf entry ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
        collecting (make-comparison-expression (pop fields) (pop fields))))

(defmacro where (&rest fields)
  `(lambda (entry) (and ,@(make-comparison-list fields))))



;;;; shorthand translation

(defparameter *shorthand-dict*
  '((:a 1 :A)
    (:a 2 :G♯)
    (:a 3 :A♭)
    (:a 4 :Ȧ)
    (:a 5 :Ȧ♭)
    (:a 6 :A’)
    (:b 1 :B♮)
    (:b 2 :B♭)
    (:b 3 :A♯)
    (:b 4 :Ḃ♮)
    (:b 5 :Ḃ♭)
    (:b 6 :B’)
    (:c 1 :C)
    (:c 2 :B♯)
    (:c 3 :Ċ)
    (:c 4 :C’)
    (:d 1 :D)
    (:d 2 :C♯)
    (:d 3 :D♭)
    (:d 4 :Ḋ)
    (:d 5 :Ḋ♭)
    (:d 6 :D’)
    (:e 1 :E)
    (:e 2 :E♭)
    (:e 3 :D♯)
    (:e 4 :Ė)
    (:e 5 :Ė♭)
    (:e 6 :E’)
    (:f 1 :F)
    (:f 2 :E♯)
    (:f 3 :Ḟ)
    (:f 6 :F’)
    (:g 1 :G)
    (:g 2 :F♯)
    (:g 3 :G♭)
    (:g 4 :Ġ)
    (:g 5 :Ġ♭)
    (:g 6 :G’)))

(defun shorthand (root ordine)
  (third (find (list root ordine) *shorthand-dict*
               :key (lambda (entry) (list (first entry) (second entry)))
               :test #'equal)))

(defun root-ordine (shorthand)
  (let ((result (find shorthand *shorthand-dict* :key #'third)))
    (values (first result) (second result))))
