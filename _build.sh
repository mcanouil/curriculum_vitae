#!/bin/sh

pdflatex curriculum_vitae
bibtex curriculum_vitae
pdflatex curriculum_vitae 
pdflatex curriculum_vitae 


rm *.aux *.bbl *.log *.blg *.out

