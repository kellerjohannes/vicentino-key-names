;; Obsolete code, not in use anymore


(defparameter *shorthand-dict*
  '((:a 1 :a nil nil :A "A")
    (:a 2 :g :sharp nil :G♯ "\\nsharp{G}")
    (:a 3 :a :flat nil :A♭ "\\nflat{A}")
    (:a 4 :a nil :dot :Ȧ "\\ndot{A}")
    (:a 5 :a :flat :dot :Ȧ♭ "\\nflatdot{A}")
    (:a 6 :a nil :comma :A’ "\\ncomma{A}")
    (:b 1 :b nil nil :B♮ "\\nnatural{B}")
    (:b 2 :b :flat nil :B♭ "\\nflat{B}")
    (:b 3 :a :sharp nil :A♯ "\\nsharp{A}")
    (:b 4 :b nil :dot :Ḃ♮ "\\nnaturaldot{B}")
    (:b 5 :b :flat :dot :Ḃ♭ "\\nflatdot{B}")
    (:b 6 :b nil :comma :B♮’ "\\nnaturalcomma{B}")
    (:c 1 :c nil nil :C "C")
    (:c 3 :b :sharp nil :B♯ "\\nsharp{B}")
    (:c 4 :c nil :dot :Ċ "\\ndot{C}")
    (:c 6 :c nil :comma :C’ "\\ncomma{C}")
    (:d 1 :d nil nil :D "D")
    (:d 2 :c :sharp nil :C♯ "\\nsharp{C}")
    (:d 3 :d :flat nil :D♭ "\\nflat{D}")
    (:d 4 :d nil :dot :Ḋ "\\ndot{D}")
    (:d 5 :d :flat :dot :Ḋ♭ "\\nflatdot{D}")
    (:d 6 :d nil :comma :D’ "\\ncomma{D}")
    (:e 1 :e nil nil :E "E")
    (:e 2 :e :flat nil :E♭ "\\nflat{E}")
    (:e 3 :d :sharp nil :D♯ "\\nsharp{D}")
    (:e 4 :e nil :dot :Ė "\\ndot{E}")
    (:e 5 :e :flat :dot :Ė♭ "\\nflatdot{E}")
    (:e 6 :e nil :comma :E’ "\\ncomma{E}")
    (:f 1 :f nil nil :F "F")
    (:f 3 :e :sharp nil :E♯ "\\nsharp{E}")
    (:f 4 :f nil :dot :Ḟ "\\ndot{F}")
    (:f 6 :f nil :comma :F’ "\\ncomma{F}")
    (:g 1 :g nil nil :G "G")
    (:g 2 :f :sharp nil :F♯ "\\nsharp{F}")
    (:g 3 :g :flat nil :G♭ "\\nflat{G}")
    (:g 4 :g nil :dot :Ġ "\\ndot{G}")
    (:g 5 :g :flat :dot :Ġ♭ "\\nflatdot{G}")
    (:g 6 :g nil :comma :G’ "\\ncomma{G}")))



(defun shorthand (root ordine &optional (tex-string nil))
  (funcall (if tex-string #'seventh #'sixth)
           (find (list root ordine) *shorthand-dict*
                 :key (lambda (entry) (list (first entry) (second entry)))
                 :test #'equal)))

(defun root-ordine (shorthand)
  (let ((result (find shorthand *shorthand-dict* :key #'sixth)))
    (values (first result) (second result))))

(defun alterations->shorthand (root chromatic-alteration enharmonic-alteration &optional (tex-string nil))
  (funcall (if tex-string #'seventh #'sixth)
           (find (list root chromatic-alteration enharmonic-alteration) *shorthand-dict*
                 :key (lambda (entry) (list (third entry) (fourth entry) (fifth entry)))
                 :test #'equal)))

(defun alteration-list->shorthand (lst)
  (alterations->shorthand (first lst) (second lst) (third lst)))







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
