(in-package :key-names)

(defparameter +latex-titelage+
"
\\author{Johannes Keller}
\\date{\\today}
\\subtitle{Berücksichtigt sämtliche Tastennamen, Intervalle und Noten der Kapitel b5-c8 bis b5-c38.}
")

(defparameter +latex-legende+
"
{\\footnotesize{Legende:
»\\texttt{D}«~\\texttt{:diplomatic},
»\\texttt{sh}«~\\texttt{:regular-shorthand},
»\\texttt{C}«~\\texttt{:obvious-correction},
»\\texttt{R}«~\\texttt{:recommended-correction},
»\\texttt{om}«~\\texttt{:omitted-text},
»\\texttt{extd}«~\\texttt{:extended-key},
»\\texttt{qs}«~\\texttt{:quintenschaukel},
»\\texttt{p}«~\\texttt{:propinqua},
»\\texttt{ip}«~\\texttt{:inverse-propinqua},
»\\texttt{$\\neg$ip}«~\\texttt{:avoid-inverse-propinqua},
»\\texttt{p}«~\\texttt{:propinquissima},
»\\texttt{ipp}«~\\texttt{:inverse-propinquissima},
»\\texttt{$\\neg$ipp}«~\\texttt{:avoid-inverse-propinquissima},
»\\texttt{ex}«~\\texttt{:exotic},
»\\texttt{$\\neg$ex}«~\\texttt{:avoid-exotic},
»\\texttt{{\\small\\fbox{7}}}«~\\texttt{:septimal}.
}}
")

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
