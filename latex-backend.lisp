(in-package :key-names)

(defparameter *shorthand-dict*
  '((:a 1 :A "A")
    (:a 2 :G♯ "\\nsharp{G}")
    (:a 3 :A♭ "\\nflat{A}")
    (:a 4 :Ȧ "\\ndot{A}")
    (:a 5 :Ȧ♭ "\\nflatdot{A}")
    (:a 6 :A’ "\\ncomma{A}")
    (:b 1 :B♮ "\\nnatural{B}")
    (:b 2 :B♭ "\\nflat{B}")
    (:b 3 :A♯ "\\nsharp{A}")
    (:b 4 :Ḃ♮ "\\nnaturaldot{B}")
    (:b 5 :Ḃ♭ "\\nflatdot{B}")
    (:b 6 :B’ "\\nnaturalcomma{B}")
    (:c 1 :C "C")
    (:c 2 :B♯ "\\nsharp{B}")
    (:c 3 :Ċ "\\ndot{C}")
    (:c 4 :C’ "\\ncomma{C}")
    (:d 1 :D "D")
    (:d 2 :C♯ "\\nsharp{C}")
    (:d 3 :D♭ "\\nflat{D}")
    (:d 4 :Ḋ "\\ndot{D}")
    (:d 5 :Ḋ♭ "\\nflatdot{D}")
    (:d 6 :D’ "\\ncomma{D}")
    (:e 1 :E "E")
    (:e 2 :E♭ "\\nflat{E}")
    (:e 3 :D♯ "\\nsharp{D}")
    (:e 4 :Ė "\\ndot{E}")
    (:e 5 :Ė♭ "\\nflatdot{E}")
    (:e 6 :E’ "\\ncomma{E}")
    (:f 1 :F "F")
    (:f 2 :E♯ "\\nsharp{E}")
    (:f 3 :Ḟ "\\ndot{F}")
    (:f 6 :F’ "\\ncomma{F}")
    (:g 1 :G "G")
    (:g 2 :F♯ "\\nsharp{F}")
    (:g 3 :G♭ "\\nflat{G}")
    (:g 4 :Ġ "\\ndot{G}")
    (:g 5 :Ġ♭ "\\nflatdot{G}")
    (:g 6 :G’ "\\ncomma{G}")))

(defun shorthand (root ordine &optional (tex-string nil))
  (funcall (if tex-string #'fourth #'third)
           (find (list root ordine) *shorthand-dict*
                 :key (lambda (entry) (list (first entry) (second entry)))
                 :test #'equal)))

(defun root-ordine (shorthand)
  (let ((result (find shorthand *shorthand-dict* :key #'third)))
    (values (first result) (second result))))




;;;; TEX output

(defparameter *flag-tex-translation*
  '(:diplomatic "diplomatisch"
    :obvious "offensichtlich"
    :probable "naheliegend"
    :extended "erweitert"
    :experimental "experimentell"))

(defun generate-longtable-row-keys (key)
  (let ((folio-cons (getf key :folio)))
    (format nil "~a & ~a & ~a & ~a~a & ~a & ~a~a & ~a & ~a & ~a \\\\"
            (getf key :libro)
            (getf key :chapter)
            (getf key :id)
            (car folio-cons)
            (if (eq (cdr folio-cons) :recto) "r" "v")
            (getf key :original-name)
            (getf key :root)
            (getf key :ordine)
            (shorthand (getf key :root) (getf key :ordine) t)
            (getf *flag-tex-translation* (getf key :flag))
            (getf key :comment))))

(defun generate-tex-code-keys (data)
  (format nil "%% Auto-generated file
\\documentclass[10pt,landscape,DIV=13]{scrartcl}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[ngerman]{babel}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{tipa}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{soul}
\\setcounter{secnumdepth}{0}
\\author{Johannes Keller}
\\date{\\today}
\\title{Inventar sämtlicher Tastenbezeichnungen im \\emph{Libro V}}


\\def\\nsharp#1{$\\sharp$#1}
\\def\\nflat#1{$\\flat$#1}
\\def\\nnatural#1{$\\natural$#1}
\\def\\ndot#1{\\.{#1}}
\\def\\nnaturaldot#1{$\\natural$\\.{#1}}
\\def\\ncomma#1{\\'{#1}}
\\def\\nnaturalcomma#1{$\\natural$\\'{#1}}
\\def\\nflatdot#1{$\\flat$\\.{#1}}
\\def\\nsharpdot#1{$\\sharp$\\.{#1}}

\\begin{document}

\\maketitle


\\begin{center}
\\caps{Auflistung aller Tastenbezeichnungen}
\\begin{longtable}{p{2mm}p{2mm}p{2mm}p{6mm}p{5.5cm}p{5mm}p{5mm}p{2cm}p{10cm}}

\\toprule
\\emph{B} &
\\emph{C} &
\\emph{I} &
\\emph{fol.} &
\\emph{Name (normalisierte Orthographie)} &
\\emph{K1} &
\\emph{K2} &
\\emph{Lesart} &
\\emph{Kommentar}\\\\
\\midrule
\\endhead

~{~%~a~}

\\bottomrule
\\end{longtable}
\\end{center}
\\end{document}"
          (mapcar #'generate-longtable-row-keys data)))


(defun write-tex-file-keys (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out "~a" (generate-tex-code-keys (sort-by-b-c-i-f *keys*)))))
