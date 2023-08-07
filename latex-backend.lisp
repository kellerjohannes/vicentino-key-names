(in-package :key-names)

(defparameter *latex-text-replacements*
  '(("&" . "\\&")
    ("_" . "\\_")
    ("#" . "\\#")
    ;; ("{" . "\\{")
    ;; ("}" . "\\}")
    ))

(defun replace-substring (new old source &optional (start-position 0))
  (let ((occurrence (search old source :start2 start-position)))
    (if occurrence
        (replace-substring new old (concatenate 'string
                                                (subseq source 0 occurrence)
                                                new
                                                (subseq source (+ occurrence (length old))))
                           (+ occurrence (length new)))
        source)))

(defun make-string-latex-friendly (str)
  (let ((result (format nil "~a" str)))
    (dolist (candidate *latex-text-replacements* result)
      (setf result (replace-substring (cdr candidate) (car candidate) result)))))

(defparameter *latex-format-triggers*
  '(("/" . "\\emph{")
    ("*" . "\\textbf{")))

(defparameter *bold-trigger* "*")
(defparameter *italics-trigger* "/")

;; TODO: this is not ideal, because it only replaces the triggers literally, without checking
;; whether they are attached to words or within words.
(defun replace-formatting (str trigger command &optional (start-position 0))
  (let ((occurrence-1 (search trigger str :start2 start-position)))
    (if occurrence-1
        (let ((occurrence-2 (search trigger str :start2 (1+ occurrence-1))))
          (if occurrence-2
              (replace-formatting
               (concatenate 'string
                            (subseq str 0 occurrence-1)
                            command
                            (subseq str (+ occurrence-1 (length *italics-trigger*)) occurrence-2)
                            "}"
                            (subseq str (+ occurrence-2 (length *italics-trigger*))))
               trigger command occurrence-2)
              str))
        str)))

(defun generate-latex-formatting (str)
  (let ((result str))
    (dolist (candidate *latex-format-triggers* (make-string-latex-friendly result))
      (setf result (replace-formatting result (car candidate) (cdr candidate))))))


(defun lookup-location (id)
  (dolist (chapter *chapter-index*)
    (when (and (<= id (getf chapter :last-id))
               (>= id (getf chapter :first-id)))
      (return (cons (getf chapter :book) (getf chapter :chapter))))))

;; Only to be used in TeX generating contexts.
(defmacro access (field)
  `(if (getf item ,field)
       (getf item ,field)
       "--"))

;; Only to be used in TeX generating contexts.
(defmacro type-select (expr-key expr-interval expr-note)
  `(case (getf item :item-type)
     (:key ,expr-key)
     (:interval ,expr-interval)
     (:note ,expr-note)))

;;;; TEX output

(defparameter *item-type-symbols*
  '(
    ;;(:key . "$\\hspace{1pt}\\square$")
    (:key . "$\\Square$")
    (:interval . "$\\Leftrightarrow$")
    (:note . "$\\CIRCLE$")))

(defun get-item-type-symbol (type-kwd)
  (cdr (assoc type-kwd *item-type-symbols*)))

(defparameter *dict-tags*
  '((:avoid-exotic . "$\\neg$ex")
    (:avoid-inverse-propinqua . "$\\neg$ip")
    (:diplomatic . "D")
    (:exotic . "ex")
    (:extended-key . "extd")
    (:propinqua . "p")
    (:propinquissima . "pp")
    (:inverse-propinqua . "ip")
    (:inverse-propinquissima . "ipp")
    (:obvious-correction . "C")
    (:omitted-text . "om")
    (:quintenschaukel . "qs")
    (:regular-shorthand . "sh")
    (:septimal . "{\\small\\fbox{7}}")))

(defun replace-tag (tag-kwd)
  (cdr (assoc tag-kwd *dict-tags*)))

(defparameter *line-counter* 0)

(defun generate-table-line (item background-data resolve-intervals-p)
  (let ((location (lookup-location (getf item :id))))
    (format nil "~a & ~a & ~a & ~a & ~a & ~a & ~a & ~a & ~a \\\\"
            (format nil "\\typesetLinecounter{~a}" (incf *line-counter*))
            (get-item-type-symbol (getf item :item-type))
            (access :id)
            (car location)
            (cdr location)
            (type-select (make-string-latex-friendly (access :key-name))
                         (if resolve-intervals-p
                             (format nil "~a \\typesetInterval{~a}{~a}{~a}"
                                     (make-string-latex-friendly (access :interval-name))
                                     (access :departure)
                                     (symbol-name (getf item :direction))
                                     (access :destination))
                             (make-string-latex-friendly (access :interval-name)))
                         "--")
            (type-select (format nil "~a \\typesetKey{~a}{~a}"
                                 (access :note-name)
                                 (access :root-letter)
                                 (access :ordine))
                         (if resolve-intervals-p
                             (let ((departure (getf item :departure))
                                   (destination (getf item :destination)))
                               (format nil "\\typesetInterval{~a}{~a}{~a}"
                                       (getf (pick background-data :id departure) :note-name)
                                       (symbol-name (getf item :direction))
                                       (getf (pick background-data :id destination) :note-name)))
                             (format nil "\\typesetInterval{~a}{~a}{~a}"
                                     (getf item :departure)
                                     (symbol-name (getf item :direction))
                                     (getf item :destination)))
                         (access :note-name))
            (format nil "{\\footnotesize~{\\texttt{~a} ~}}" (mapcar #'replace-tag (access :tag-list)))
            (generate-latex-formatting (access :comment)))))

(defun generate-list-tex-code (document-title table-title display-data background-data
                               resolve-intervals-p)
  (setf *line-counter* 0)
  (concatenate 'string
               +latex-header+
               +latex-titelage+
               (format nil "
\\title{~a}


\\begin{document}

\\maketitle

\\begin{center}

\\vspace{3ex}

{\\large{~a}}

\\vspace{2ex}
"
                       document-title
                       table-title)
               +latex-legende+
               (format nil "
\\begin{longtable}{p{1.5mm}C{1.5mm}p{4.5mm}p{1mm}p{2mm}p{6.5cm}p{15mm}p{1cm}p{11cm}}

\\toprule
\\# &
\\emph{T} &
\\emph{I} &
\\emph{B} &
\\emph{C} &
\\emph{Name (normalisierte Orthographie)} &
&
\\emph{Tags} &
\\emph{Kommentar}\\\\
\\midrule
\\endhead

~{~%~a~}

\\bottomrule
\\end{longtable}
\\end{center}
\\end{document}"
                       (mapcar (lambda (line)
                                 (generate-table-line line background-data resolve-intervals-p))
                               display-data))))

(defun write-list (filename document-title table-title display-data background-data
                   &key resolve-intervals)
  (with-open-file (out (merge-pathnames *tex-output-path* filename)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out "%% Auto-generated file: ~a~&~a"
            (local-time:universal-to-timestamp (get-universal-time))
            (generate-list-tex-code document-title
                                    table-title
                                    display-data
                                    background-data
                                    resolve-intervals))))
