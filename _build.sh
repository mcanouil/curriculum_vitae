#!/bin/sh

cd latex/

pdflatex curriculum_vitae
bibtex curriculum_vitae
pdflatex curriculum_vitae 
pdflatex curriculum_vitae 
rm *.aux *.bbl *.log *.blg *.out

cd ../

rm *.html


Rscript -e 'rmarkdown::render(input = "curriculum_vitae.Rmd", output_file = "index.html", encoding = "UTF-8")'

Rscript -e 'rmarkdown::render_site(input = "www")'
