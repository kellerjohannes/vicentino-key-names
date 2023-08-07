(in-package :key-names)

(defun translate-direction (item)
  (case (getf item :direction)
    (:➚ :up)
    (:➘ :down)
    (otherwise nil)))

(defun scale-diagram (len)
  (* len 0.009))

(defun generate-tikz-line (interval-size)
  (format nil "
\\begin{tikzpicture}
\\draw[fill=black] (0,0) -- (0,0.1) -- (~a,0.1) -- (~a,0) -- (0,0) ;
\\draw[thin] (0,0) -- (~a,0) ; % bottom to 2:1
\\draw[thin] (0,0.2) -- (~a,0.2) ; % top to 2:1
\\draw[thin] (0,0) -- (0,0.2) ; % tick on 1:1
\\draw[very thin] (~a,0) -- (~a,0.2) ; % tick on 6:5
\\draw[very thin] (~a,0) -- (~a,0.2) ; % tick on 5:4
\\draw[very thin] (~a,0) -- (~a,0.2) ; % tick on 3:2
\\draw[very thin] (~a,0) -- (~a,0.2) ; % tick on 8:5
\\draw[very thin] (~a,0) -- (~a,0.2) ; % tick on 5:3
\\draw[very thin] (~a,0) -- (~a,0.2) ; % tick on 2:1
\\end{tikzpicture}"
          (scale-diagram interval-size)
          (scale-diagram interval-size)
          (scale-diagram (ratio->length 2/1))
          (scale-diagram (ratio->length 2/1))
          (scale-diagram (ratio->length 6/5))
          (scale-diagram (ratio->length 6/5))
          (scale-diagram (ratio->length 5/4))
          (scale-diagram (ratio->length 5/4))
          (scale-diagram (ratio->length 3/2))
          (scale-diagram (ratio->length 3/2))
          (scale-diagram (ratio->length 8/5))
          (scale-diagram (ratio->length 8/5))
          (scale-diagram (ratio->length 5/3))
          (scale-diagram (ratio->length 5/3))
          (scale-diagram (ratio->length 2/1))
          (scale-diagram (ratio->length 2/1))))

(defun get-interval-size (item tuning-id data)
  (let* ((departure (getf item :departure))
         (destination (getf item :destination))
         (departure-name (getf (pick data :id departure) :note-name))
         (destination-name (getf (pick data :id destination) :note-name)))

    ;; only for debug
    (unless (and departure-name destination-name)
      (format t "~&~a: ~a,~a | ~a,~a" (getf item :id) departure-name destination-name departure destination))

    (interval-size tuning-id departure-name (translate-direction item) destination-name)))

(defun generate-spreadsheet-line (item tuning-id background-data)
  (let* ((location (lookup-location (getf item :id)))
         (departure (getf item :departure))
         (destination (getf item :destination))
         (departure-name (getf (pick background-data :id departure) :note-name))
         (destination-name (getf (pick background-data :id destination) :note-name)))
    (format nil "~a & ~a & ~a & ~a & ~a & ~a & ~a & ~a & ~a \\\\"
            (format nil "\\typesetLinecounter{~a}" (incf *line-counter*))
            (format nil "{\\tiny~a~a}"
                    (get-item-type-symbol (getf (pick background-data :id departure) :item-type))
                    (get-item-type-symbol (getf (pick background-data :id destination) :item-type)))
            (access :id)
            (car location)
            (cdr location)
            (type-select (make-string-latex-friendly (access :key-name))
                         (format nil "~a \\typesetInterval{~a}{~a}{~a}"
                                 (make-string-latex-friendly (access :interval-name))
                                 (access :departure)
                                 (symbol-name (getf item :direction))
                                 (access :destination))
                         "--")
            (type-select (format nil "~a \\typesetKey{~a}{~a}"
                                 (access :note-name)
                                 (access :root-letter)
                                 (access :ordine))
                         (format nil "\\typesetInterval{~a}{~a}{~a}"
                                 departure-name
                                 (symbol-name (getf item :direction))
                                 destination-name)
                         (access :note-name))
            (format nil "~{\\texttt{~a} ~}" (mapcar #'replace-tag (access :tag-list)))
            (generate-tikz-line (get-interval-size item tuning-id background-data)))))


(defun generate-spreadsheet-tex-code (document-title table-title tuning-id display-data background-data)
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
\\begin{longtable}{p{1.5mm}C{3mm}p{4.5mm}p{1mm}p{2mm}p{6.5cm}p{15mm}p{1.2cm}p{10.6cm}}

\\toprule
\\# &
\\emph{T} &
\\emph{I} &
\\emph{B} &
\\emph{C} &
\\emph{Name (normalisierte Orthographie)} &
&
\\emph{Tags} &
\\emph{Intervallgrösse}\\\\
\\midrule
\\endhead

~{~%~a~}

\\bottomrule
\\end{longtable}
\\end{center}
\\end{document}"
                       (mapcar (lambda (line)
                                 (generate-spreadsheet-line line tuning-id background-data))
                               display-data))))



(defun write-spreadsheet (filename document-title table-title tuning-id display-data background-data)
  (with-open-file (out (merge-pathnames *tex-output-path* filename)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out "%% Auto-generated file: ~a~&~a"
            (local-time:universal-to-timestamp (get-universal-time))
            (generate-spreadsheet-tex-code document-title table-title tuning-id display-data
                                           background-data))))
