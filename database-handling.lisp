(in-package :key-names)

(defparameter *flag-hierarchy*
  '(:diplomatic 1 :obvious 2 :probable 3 :extended 4 :experimental 5))

(defvar *keys* nil)
(defvar *chapter-index* nil)

(defun read-db ()
  (with-open-file (in "~/common-lisp/prototypes/vicentino-tools/key-names/data/all-keys-db.lisp")
    (with-standard-io-syntax
      (setf *keys* (read in))))
  (with-open-file (in "~/common-lisp/prototypes/vicentino-tools/key-names/data/chapter-index.lisp")
    (with-standard-io-syntax
      (setf *chapter-index* (read in))))
  t)

(defun contains (lst item)
  (member item lst))

(defun make-comparison-expression (field value test)
  `(funcall ,test (getf item ,field) ,value))

(defun make-comparison-list (fields test)
  (loop while fields
        collecting (make-comparison-expression (pop fields) (pop fields) test)))

(defmacro where (test &rest fields)
  `(lambda (item) (and ,@(make-comparison-list fields test))))

(defun select (selector-fn)
  (remove-if-not selector-fn *keys*))

(defmacro pick (&rest fields)
  `(first (select (where ,@fields))))

(defun sort-by-id (data)
  (sort (copy-list data) #'< :key (lambda (item) (getf item :id))))


;; proofreading functions

(defun collect-values (field)
  (loop for item in *keys* collect (getf item field)))

(defun condense (field)
  (sort (remove nil (remove-duplicates (alexandria:flatten (collect-values field))))
        (lambda (a b)
          (string< (symbol-name a) (symbol-name b)))))

(defun extract-mappings ()
  (sort (remove-duplicates (remove nil
                                   (loop for item in *keys* collect (when (eq (getf item :item-type) :key)
                                                                      (list (getf item :note-name)
                                                                            (getf item :root-letter)
                                                                            (getf item :ordine)))))
                           :test #'equal)
        (lambda (a b)
          (string< (symbol-name a) (symbol-name b)))
        :key #'second))


;; list generating functions

;; (defun filter-reading (tag-list)
;;   (select (where #'intersection :tag-list tag-list)))

(defun create-list-of-unique-ids (data)
  (remove-duplicates (mapcar (lambda (item) (getf item :id)) data)))

(defun distill-reading (data tags)
  (remove nil (mapcar (lambda (id)
                        (block loops
                          (dolist (tag tags)
                            (dolist (candidate (select (where #'equal :id id)))
                              (when (member tag (getf candidate :tag-list))
                                (return-from loops candidate))))))
                      (create-list-of-unique-ids data))))



;; possibly obsolete

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





(defparameter *tuning* *tuning-1*)

(defun create-full-interval-string (interval-entry)
  (let* ((name-from (get-note-name-from interval-entry))
         (name-to (get-note-name-to interval-entry))
         (direction (get-direction interval-entry))
         (size (interval-size interval-entry)))
    (format nil "#~a: ~a~a~a, ~a (~a SC, ~a ¢)"
            (getf interval-entry :id)
            name-from
            direction
            name-to
            size
            (ratio->length size :unit-interval 81/80)
            (ratio->length size))))


(defun interval-size (interval-entry)
  (calculate-interval-size (get-note-name-from interval-entry)
                           (get-note-name-to interval-entry)
                           (getf interval-entry :direction)
                           *tuning*))

(defun list-intervals-size ()
  (format t "~&Listing of all intervals sorted by size:~%~%~{~a~%~}"
          (mapcar #'create-full-interval-string
           (sort (distill-reading (select (where :category :interval))
                                  (list :diplomatic))
                 #'<
                 :key #'interval-size))))
