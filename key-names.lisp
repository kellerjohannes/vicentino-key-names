(defpackage :key-names
  (:use :cl))

(in-package :key-names)

(defclass location ()
  ((book :initform nil
         :initarg :book
         :accessor book)
   (chapter :initform nil
            :initarg :chapter
            :accessor chapter)
   (folio-number :initform nil
                 :initarg :folio-number
                 :accessor folio-number)
   (folio-suffix :initform nil
                 :initarg :folio-suffix
                 :accessor folio-suffix)))

(defmethod print-location-short ((location location))
  (format nil "c~a.~2,'0d (fol. ~a~a)"
          (book location)
          (chapter location)
          (folio-number location)
          (if (eq (folio-suffix location) :recto) "r" "v")))

(defclass critical-entity (location)
  ((error-tags :initform nil
               :accessor error-tags
               :documentation "Obvious, propinqua-related, regular short-spelling, additional key, ...")
   (explanation :initform ""
                :accessor explanation
                :documentation "Equivalent of an entry in the critical apparatus.")))



(defclass key-name ()
  ((original-spelling :initform ""
                      :accessor original-spelling)
   (root-letter :initform nil
                :accessor root-letter)
   (ordine :initform 0
           :accessor ordine)
   (shorthand :initform nil
              :accessor shorthand)))

(defmethod set-key-name ((key key-name) original root ordine shorthand)
  (setf (original-spelling key) original
        (root-letter key) root
        (ordine key) ordine
        (shorthand key) shorthand))


(defclass key-instance (critical-entity)
  ((diplomatic :initform (make-instance 'key-name)
               :accessor diplomatic)
   (idealised :initform (make-instance 'key-name)
              :accessor idealised
              :documentation "In case no idealisation is needed, this will be identical to 'diplomatic.")))

(defparameter *keys* (make-hash-table))

(defun add-key (hash id original root ordine shorthand flag apparatus
                &optional (i-original original) (i-root root) (i-ordine ordine) (i-shorthand shorthand))
  (if (gethash id hash)
      (format t "~&Error: key already exists.")
      (let ((new-key (make-instance 'key-instance)))
        (set-key-name (diplomatic new-key) original root ordine shorthand)
        (set-key-name (idealised new-key) i-original i-root i-ordine i-shorthand)
        (setf (error-tags new-key) flag)
        (setf (explanation new-key) apparatus)
        (setf (gethash id hash) new-key))))


(defclass interval ()
  ((departure-key :initform nil
                  :initarg :departure
                  :accessor departure-key
                  :documentation "ID of a key.")
   (destination-key :initform nil
                    :initarg :destination
                    :accessor destination-key
                    :documentation "ID of a key.")
   (direction :initform nil
              :initarg :direction
              :accessor direction
              :documentation ":ascendente or :discendente")
   (name :initform nil
         :initarg :name
         :accessor name)))

(defclass interval-instance (critical-entity)
  ((diplomatic :initform nil
               :initarg :diplomatic
               :accessor diplomatic
               :documentation "Instance of 'interval.")
   (idealised :initform nil
              :initarg :idealised
              :accessor idealised
              :documentation "Instance of 'interval.")))


(defparameter *intervals*)
