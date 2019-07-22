#!/bin/sh

# cd latex/
# pdflatex curriculum_vitae
# bibtex curriculum_vitae
# pdflatex curriculum_vitae
# pdflatex curriculum_vitae
# rm *.aux *.bbl *.log *.blg *.out
# cd ../

# rm curriculum_vitae/curriculum_vitae.html

Rscript -e 'rmarkdown::render(input = "curriculum_vitae/curriculum_vitae.Rmd", output_dir = ".", output_file = "cv.html", encoding = "UTF-8")'

Rscript -e 'rmarkdown::render_site()'


# git -c "user.name=Mickaël Canouil" -c "mickael.canouil@cnrs.fr" commit -am "Build update"
