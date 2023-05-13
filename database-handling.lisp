(in-package :key-names)


(defparameter *flag-hierarchy*
  '(:diplomatic 1 :obvious 2 :probable 3 :extended 4 :experimental 5))

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
            (let ((flag (prompt-read-string "Flag [diplomatic strict relaxed]")))
              (if (string= flag "") :diplomatic flag))
            (prompt-read-string "Comment")))

(defun input-keys ()
  (loop (add-key (prompt-for-key))
        (unless (y-or-n-p "Add another key? [y/n]: ") (return))))

(defun save-dbs ()
  (with-open-file (out "db-keys" :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *keys* out))))

(defun read-dbs ()
  (with-open-file (in "test-db.lisp")
    (with-standard-io-syntax
      (setf *keys* (read in)))))

(defun make-comparison-expression (field value)
  `(equal (getf entry ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
        collecting (make-comparison-expression (pop fields) (pop fields))))

(defmacro where (&rest fields)
  `(lambda (entry) (and ,@(make-comparison-list fields))))

(defun select (selector-fn)
  (remove-if-not selector-fn *keys*))

(defmacro pick (&rest fields)
  `(first (select (where ,@fields))))

(defun sort-by-id (data)
  (sort (copy-list data) #'< :key (lambda (item)
                                    (getf item :id))))

;; obsolete, because index was changed to id
(defun sort-by-b-c-i-f (data)
  (sort (copy-list data) (lambda (a b)
                           (let ((book-a (getf a :libro))
                                 (book-b (getf b :libro))
                                 (chapter-a (getf a :chapter))
                                 (chapter-b (getf b :chapter))
                                 (index-a (getf a :id))
                                 (index-b (getf b :id))
                                 (flag-a (getf *flag-hierarchy* (getf a :flag)))
                                 (flag-b (getf *flag-hierarchy* (getf b :flag))))
                             (cond ((< book-a book-b) t)
                                   ((> book-a book-b) nil)
                                   (t (cond ((< chapter-a chapter-b) t)
                                            ((> chapter-a chapter-b) nil)
                                            (t (cond ((< index-a index-b) t)
                                                     ((> index-a index-b) nil)
                                                     (t (< flag-a flag-b)))))))))))



(defun extract-note-name (entry &optional latex-shorthand)
  (cond ((eq (getf entry :category) :note)
         (alterations->shorthand (getf entry :root)
                                 (getf entry :chromatic-alteration)
                                 (getf entry :enharmonic-alteration)
                                 latex-shorthand))
        ((eq (getf entry :category) :key)
         (shorthand (getf entry :root) (getf entry :ordine) latex-shorthand))
        (t (format t "~&Category not known, no note name produced."))))

(defun get-note-name-from (interval-entry &optional latex-string)
  (extract-note-name (first (select (where :id (getf interval-entry :departure)))) latex-string))

(defun get-note-name-to (interval-entry &optional latex-string)
  (extract-note-name (first (select (where :id (getf interval-entry :destination)))) latex-string))

(defun get-direction (interval-entry &optional latex-string)
  (if latex-string
      (if (eq (getf interval-entry :direction) :up) "\\nearrow" "\\searrow")
      (if (eq (getf interval-entry :direction) :up) "➚" "➘")))

(defun generate-interval-string (interval-entry)
  (format nil "~a: ~a ~a ~a, »~a«"
          (getf interval-entry :id)
          (get-note-name-from interval-entry)
          (get-direction interval-entry)
          (get-note-name-to interval-entry)
          (getf interval-entry :original-name)))

(defun list-intervals ()
  (format t "~&Listing of all intervals in database:~%~%~{~a~%~}"
          (mapcar #'generate-interval-string (select (where :category :interval)))))




(defun create-list-of-unique-ids (data)
  (remove-duplicates (mapcar (lambda (entry) (getf entry :id)) data)))

(defun distill-reading (data reading-flags)
  (mapcar (lambda (id)
            (let ((candidates (select (where :id id))))
              (dolist (reading-flag reading-flags)
                (let ((hit (member reading-flag candidates :key (lambda (entry)
                                                                  (getf entry :flag)))))
                  (when hit
                    (return (first hit)))))))
          (create-list-of-unique-ids data)))


(defun create-full-interval-string (interval-entry)
  (let* ((name-from (get-note-name-from interval-entry))
         (name-to (get-note-name-to interval-entry))
         (direction (get-direction interval-entry))
         (size (calculate-interval-size name-from name-to direction *tuning-1*)))
    (format nil "#~a: ~a~a~a, ~a (~a SC, ~a ¢)"
            (getf interval-entry :id)
            name-from
            direction
            name-to
            size
            (ratio->length size :unit-interval 81/80)
            (ratio->length size))))

(defun list-intervals-size ()
  (format t "~&Listing of all intervals sorted by size:~%~%~{~a~%~}"
          (mapcar #'create-full-interval-string
                  (sort (distill-reading (select (where :category :interval :direction :up)) '(:diplomatic))
                        #'<
                        :key (lambda (interval-entry)
                               (calculate-interval-size (get-note-name-from interval-entry)
                                                        (get-note-name-to interval-entry)
                                                        (getf interval-entry :direction)
                                                        *tuning-1*))))))
