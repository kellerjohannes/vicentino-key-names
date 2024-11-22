#!/bin/sh

pdflatex liste-komplett.tex
pdflatex liste-eingriffe.tex
pdflatex liste-kritisch.tex
pdflatex liste-kritische-eingriffe.tex
pdflatex liste-verkuerzungen.tex
pdflatex liste-verkuerzungen-vergleich.tex

pdflatex tab-tuning1-alle-intervalle-nach-groesse.tex
pdflatex tab-tuning1-alle-terzen.tex
pdflatex tab-tuning1-alle-quinten.tex
pdflatex tab-tuning1-alle-sexten.tex
pdflatex tab-tuning1-alle-oktaven.tex
pdflatex tab-tuning1-alle-semitoni-und-kleiner.tex

pdflatex tab-tuning3-alle-intervalle-nach-groesse.tex
pdflatex tab-tuning3-alle-terzen.tex
pdflatex tab-tuning3-alle-quinten.tex
pdflatex tab-tuning3-alle-sexten.tex
pdflatex tab-tuning3-alle-septimal.tex
pdflatex tab-tuning1-alle-septimal.tex
pdflatex tab-tuning1-alle-propinqui-und-propinquissimi.tex
pdflatex tab-tuning3-alle-propinqui-und-propinquissimi.tex

mv *.pdf pdf/
