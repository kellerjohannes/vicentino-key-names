(in-package :key-names)

(defparameter *latex-text-replacements*
  '(("&" . "\\&")
    ("_" . "\\_")
    ("#" . "\\#")))

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


(defun lookup-location (id)
  (dolist (chapter *chapter-index*)
    (when (and (<= id (getf chapter :last-id))
               (>= id (getf chapter :first-id)))
      (return (cons (getf chapter :book) (getf chapter :chapter))))))

(defmacro access (field)
  `(if (getf item ,field)
       (getf item ,field)
       "--"))

(defmacro type-select (expr-key expr-interval expr-note)
  `(case (getf item :item-type)
     (:key ,expr-key)
     (:interval ,expr-interval)
     (:note ,expr-note)))

;;;; TEX output

(defparameter *item-type-symbols*
  '((:key . "$\\square$")
    (:interval . "$\\Leftrightarrow$")
    (:note . "$\\bigcirc$")))

(defun get-item-type-symbol (type-kwd)
  (cdr (assoc type-kwd *item-type-symbols*)))

(defparameter *dict-tags*
  '((:AVOID-EXOTIC . "$\\neg$ex")
    (:AVOID-INVERSE-PROPINQUA . "$\\neg$ip")
    (:DIPLOMATIC . "d")
    (:EXOTIC . "ex")
    (:EXTENDED-KEY . "extd")
    (:INVERSE-PROPINQUA . "ip")
    (:INVERSE-PROPINQUISSIMA . "ipp")
    (:OBVIOUS-CORRECTION . "ob")
    (:OMITTED-TEXT . "om")
    (:PROPINQUA-PROPINQUISSIMA . "p-pp")
    (:QUINTENSCHAUKEL . "qs")
    (:REGULAR-SHORTHAND . "sh")))

(defun replace-tag (tag-kwd)
  (cdr (assoc tag-kwd *dict-tags*)))

(defun generate-table-line (item data resolve-intervals-p)
  (let ((location (lookup-location (getf item :id))))
    (format nil "~a & ~a & ~a & ~a & ~a & ~a & ~a & ~a & ~a & ~a \\\\"
            (get-item-type-symbol (getf item :item-type))
            (access :id)
            (car location)
            (cdr location)
            (type-select (access :key-name) (access :interval-name) "--")
            (type-select (format nil "~a~a" (access :root-letter) (access :ordine))
                         (if resolve-intervals-p
                             (getf (pick (where #'equal :id (getf item :departure))) :note-name)
                             (getf item :departure))
                         (access :note-name))
            (type-select "" (symbol-name (getf item :direction)) "")
            (type-select (access :note-name) (access :destination) "--")
            (format nil "~{\\sffamily{~a} ~}" (mapcar #'replace-tag (access :tag-list)))
            (generate-latex-formatting (access :comment)))))

(defun generate-tex-code (document-title table-title data resolve-intervals-p)
  (format nil "\\documentclass[10pt,landscape,DIV=17,a4paper]{scrartcl}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[ngerman]{babel}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{tipa}
\\usepackage{array}
\\usepackage{amssymb}
\\usepackage{booktabs}
\\usepackage{soul}
\\setcounter{secnumdepth}{0}
\\author{Johannes Keller}
\\date{\\today}
\\title{~a}
\\subtitle{Berücksichtigt sämtliche Tastennamen, Intervalle und Noten der Kapitel b5-c8 bis b5-c38.}


\\usepackage{newunicodechar}
\\newunicodechar{♮}{$\\natural$}
\\newunicodechar{♭}{$\\flat$}
\\newunicodechar{♯}{$\\sharp$}
\\newunicodechar{➚}{$\\nearrow$}
\\newunicodechar{➘}{$\\searrow$}
\\newunicodechar{Ȧ}{\\.A}
\\newunicodechar{Ḃ}{\\.B}
\\newunicodechar{Ċ}{\\.C}
\\newunicodechar{Ḋ}{\\.D}
\\newunicodechar{Ė}{\\.E}
\\newunicodechar{Ḟ}{\\.F}
\\newunicodechar{Ġ}{\\.G}
\\newunicodechar{ʼ}{'}

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
\\caps{~a}
\\begin{longtable}{p{1.5mm}p{4.5mm}p{1mm}p{2mm}p{7.5cm}p{5mm}p{2mm}p{5mm}p{1cm}p{11cm}}

\\toprule
\\emph{T} &
\\emph{I} &
\\emph{B} &
\\emph{C} &
\\emph{Name (normalisierte Orthographie)} &
&
&
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
          document-title
          table-title
          (mapcar (lambda (line)
                    (generate-table-line line resolve-intervals-p))
                  data)))



(defparameter *tex-output-path* "~/common-lisp/prototypes/vicentino-tools/key-names/tex-output/")

(defun write-tex-file (filename document-title table-title data &key resolve-intervals)
  (with-open-file (out (merge-pathnames *tex-output-path* filename)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out "%% Auto-generated file: ~a~&~a"
            (local-time:universal-to-timestamp (get-universal-time))
            (generate-tex-code document-title table-title data resolve-intervals))))

(defun generate-tex ()
  (write-tex-file "komplett.tex"
                  "Komplettes Inventar"
                  "S\\\"amtliche Tasten, Intervalle und Noten in allen Lesarten"
                  *keys*)
  (write-tex-file "kritisch.tex"
                  "Kritisches Inventar"
                  "S\\\"amtliche Tasten, Intervalle und Noten in kritischer Lesart"
                  (distill-reading *keys* '(:obvious-correction :diplomatic))
                  :resolve-intervals t))
