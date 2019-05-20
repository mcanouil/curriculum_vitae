#!/bin/sh

pdflatex curriculum_vitae
bibtex curriculum_vitae
pdflatex curriculum_vitae 
pdflatex curriculum_vitae 


rm *.aux *.bbl *.log *.blg *.out


Rscript -e 'rmarkdown::render(input = "curriculum_vitae.Rmd", output_file = "index.html", encoding = "UTF-8")'
