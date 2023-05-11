(in-package :key-names)

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


;;;; TEX output

(defparameter *flag-tex-translation*
  '(:diplomatic "diplomatisch"
    :obvious "offensichtlich"
    :anti-septimal "anti-septimal"
    :in-discussion "unklar"))

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


\\def\\nsharp#1{#1$\\sharp$}
\\def\\nflat#1{#1$\\flat$}
\\def\\nnatural#1{#1$\\natural$}
\\def\\ndot#1{\\.{#1}}
\\def\\nnaturaldot#1{\\.{#1}$\\natural$}
\\def\\ncomma#1{\\'{#1}}
\\def\\nnaturalcomma#1{\\'{#1}$\\natural$}
\\def\\nflatdot#1{\\.{#1}$\\flat$}
\\def\\nsharpdot#1{\\.{#1}$\\sharp$}

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
          (remove-if #'null (mapcar #'generate-longtable-row-keys data))))


(defun write-tex-file-keys (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out "~a" (generate-tex-code-keys (sort-by-id (select (where :category :key)))))))
