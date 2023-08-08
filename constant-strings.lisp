(in-package :key-names)


(defparameter *item-type-symbols*
  '((:key . "$\\Square$")
    (:interval . "$\\Leftrightarrow$")
    (:note . "$\\CIRCLE$")))

(defun lookup-type-symbol (type-kwd)
  (cdr (assoc type-kwd *item-type-symbols*)))

(defun get-item-type-symbol (item background-data)
  (let ((type (getf item :item-type)))
    (if (eq type :interval)
        (let ((departure (getf item :departure))
              (destination (getf item :destination)))
          (if (and departure destination)
              (let ((departure-symbol (lookup-type-symbol (getf (pick background-data :id departure) :item-type)))
                    (destination-symbol (lookup-type-symbol (getf (pick background-data :id destination) :item-type))))
                (if (and departure-symbol destination-symbol)
                    (format nil "{\\tiny~a~a}" departure-symbol destination-symbol)
                    (lookup-type-symbol :interval)))))
        (lookup-type-symbol type))))

(defparameter *dict-tags*
  '((:avoid-exotic . "$\\neg$ex")
    (:avoid-inverse-propinqua . "$\\neg$ip")
    (:avoid-inverse-propinquissima . "$\\neg$ipp")
    (:diplomatic . "D")
    (:recommended-correction . "R")
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


(defparameter +latex-titelage+
  "
\\author{Johannes Keller}
\\date{\\today}
\\subtitle{Berücksichtigt sämtliche Tastennamen, Intervalle und Noten der Kapitel b5-c8 bis b5-c38.}
")


(defparameter +latex-legende+
  (format nil "
{\\footnotesize{Legende:
\\#~~Zeilennummerierung,
T~~Objekttyp (~a~~Taste, ~a~~Note, ~a~~Intervall zwischen Tasten, ~a~~Intervall zwischen Noten),
I~~Objekt-ID,
B~~\\emph{libro},
C~~\\emph{capitolo},
»\\texttt{~a}«~~\\texttt{:diplomatic},
»\\texttt{~a}«~~\\texttt{:regular-shorthand},
»\\texttt{~a}«~~\\texttt{:obvious-correction},
»\\texttt{~a}«~~\\texttt{:recommended-correction},
»\\texttt{~a}«~~\\texttt{:omitted-text},
»\\texttt{~a}«~~\\texttt{:extended-key},
»\\texttt{~a}«~~\\texttt{:quintenschaukel},
»\\texttt{~a}«~~\\texttt{:propinqua},
»\\texttt{~a}«~~\\texttt{:inverse-propinqua},
»\\texttt{~a}«~~\\texttt{:avoid-inverse-propinqua},
»\\texttt{~a}«~~\\texttt{:propinquissima},
»\\texttt{~a}«~~\\texttt{:inverse-propinquissima},
»\\texttt{~a}«~~\\texttt{:avoid-inverse-propinquissima},
»\\texttt{~a}«~~\\texttt{:exotic},
»\\texttt{~a}«~~\\texttt{:avoid-exotic},
»\\texttt{~a}«~~\\texttt{:septimal},
Skala der Intervallgrössen: Markierungen für 1:1 81:80, 128:125, 6:5, 5:4, 3:2, 8:5, 5:3 und 2:1.
}}
"
          (get-item-type-symbol '(:item-type :key) nil)
          (get-item-type-symbol '(:item-type :note) nil)
          (get-item-type-symbol '(:item-type :interval :departure 1 :destination 3)
                                '((:id 1 :item-type :key)
                                  (:id 3 :item-type :key)))
          (get-item-type-symbol '(:item-type :interval :departure 1 :destination 3)
                                '((:id 1 :item-type :note)
                                  (:id 3 :item-type :note)))
          (replace-tag :diplomatic)
          (replace-tag :regular-shorthand)
          (replace-tag :obvious-correction)
          (replace-tag :recommended-correction)
          (replace-tag :omitted-text)
          (replace-tag :extended-key)
          (replace-tag :quintenschaukel)
          (replace-tag :propinqua)
          (replace-tag :inverse-propinqua)
          (replace-tag :avoid-inverse-propinqua)
          (replace-tag :propinquissima)
          (replace-tag :inverse-propinquissima)
          (replace-tag :avoid-inverse-propinquissima)
          (replace-tag :exotic)
          (replace-tag :avoid-exotic)
          (replace-tag :septimal)
          ))

(defparameter +latex-header+
"\\documentclass[10pt,landscape,DIV=17,a4paper]{scrartcl}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[ngerman]{babel}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{tipa}
\\usepackage{array}
\\usepackage{amssymb}
\\usepackage{mathtools}
\\usepackage{wasysym}
\\usepackage{booktabs}
\\usepackage{soul}
\\usepackage{titling}
\\usepackage{tikz}
\\setcounter{secnumdepth}{0}

\\usepackage{newunicodechar}
\\newunicodechar{♮}{$\\natural$}
\\newunicodechar{♭}{$\\flat$}
\\newunicodechar{♯}{$\\sharp$}
\\newunicodechar{➚}{{\\small$\\nearrow$}}
\\newunicodechar{➘}{{\\small$\\searrow$}}
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


%% This is used for a thighter box around key names
\\setlength\\fboxsep{1.2pt}

\\def\\typesetInterval#1#2#3{\\small{$\\lvert$#1#2#3$\\rvert$}}
\\def\\typesetKey#1#2{\\fbox{\\footnotesize{\\textsc{#1#2}}}}
\\def\\typesetLinecounter#1{\\tiny{\\textsc{#1}}}
\\def\\typesetTag#1{\\texttt{#1}}

\\renewcommand*{\\maketitle}{\\noindent%
\\parbox{\\dimexpr\\linewidth-2\\fboxsep}{\\centering%
\\fontsize{20}{24}\\selectfont\\sffamily\\bfseries\\thetitle\\\\[1ex]%
\\fontsize{12}{14}\\selectfont\\centering\\today\\hspace{1cm}\\theauthor}}


\\newcolumntype{C}[1]{>{\\centering\\arraybackslash}p{#1}}

\\renewcommand{\\arraystretch}{1.3}")
