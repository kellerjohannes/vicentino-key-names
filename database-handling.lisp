(in-package :vicentino-key-names)

(defvar *keys* nil)
(defvar *chapter-index* nil)

(defun read-db ()
  (with-open-file (in (merge-pathnames "data/all-keys-db.lisp"
                                       (asdf/system:system-source-directory :vicentino-key-names)))
    (with-standard-io-syntax
      (setf *keys* (read in))))
  (with-open-file (in (merge-pathnames "data/chapter-index.lisp"
                                       (asdf/system:system-source-directory :vicentino-key-names)))
    (with-standard-io-syntax
      (setf *chapter-index* (read in))))
  t)

(defun contains (lst item)
  (member item lst))


(defmacro where (test &rest fields)
  `(lambda (item) (and ,@(make-comparison-list fields test))))

(defun select (data selector-fn)
  (remove-if-not selector-fn data))

(defun unselect (data selector-fn)
  (remove-if selector-fn data))

(defmacro pick (data &rest fields)
  `(first (select ,data (where #'equal ,@fields))))

(defun sort-by-id (data)
  (sort (copy-list data) #'< :key (lambda (item) (getf item :id))))



;; proofreading functions

(defun collect-values (field)
  (loop for item in *keys* collect (getf item field)))

(defun collect-values-1 (data field)
  (loop for item in data collect (getf item field)))

(defun condense (field)
  (sort (remove nil (remove-duplicates (alexandria:flatten (collect-values field))))
        (lambda (a b)
          (string< (symbol-name a) (symbol-name b)))))

(defun condense-1 (data field)
  (sort (remove nil (remove-duplicates (alexandria:flatten (collect-values-1 data field))))
        (lambda (a b)
          (string< (symbol-name a) (symbol-name b)))))

(defun extract-mappings ()
  (sort (remove-duplicates
         (remove nil (loop for item in *keys* collect (when (eq (getf item :item-type) :key)
                                                        (list (getf item :note-name)
                                                              (getf item :root-letter)
                                                              (getf item :ordine)))))
         :test #'equal)
        (lambda (a b)
          (string< (symbol-name a) (symbol-name b)))
        :key #'second))

(defun spot-redundant-ids (data)
  (let ((conclusion nil))
    (dolist (item data conclusion)
      (let ((current-id (getf item :id)))
        (when (> (length (select data (where #'eq :id current-id))) 1)
          (push (getf item :id) conclusion))))))

(defun check-type-conformity-of-intervals (data)
  (let ((conclusion nil))
    (dolist (item data conclusion)
      (when (eq (getf item :item-type) :interval)
        (let ((departure-id (getf item :departure))
              (destination-id (getf item :destination)))
          (unless (eq (getf (pick data :id departure-id) :item-type)
                      (getf (pick data :id destination-id) :item-type))
            (push (getf item :id) conclusion)))))))

;; list generating functions

(defun create-list-of-unique-ids (data)
  (remove-duplicates (mapcar (lambda (item) (getf item :id)) data)))

(defun distill-reading (data tags)
  (remove nil (mapcar (lambda (id)
                        (block loops
                          (dolist (tag tags)
                            (dolist (candidate (select data (where #'eq :id id)))
                              (when (member tag (getf candidate :tag-list))
                                (return-from loops candidate))))))
                      (create-list-of-unique-ids data))))

(defun remove-unique-items (data)
  (remove-if (lambda (item)
               (let ((current-id (getf item :id)))
                 (= 1 (length (select data (where #'eq :id current-id))))))
             data))
