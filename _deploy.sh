#!/bin/bash

Rscript -e 'rmarkdown::render(input = "curriculum_vitae/curriculum_vitae.Rmd", output_dir = ".", output_file = "cv.html", encoding = "UTF-8")' ;

echo "curl -u $1:$2 -T cv.html ${3}index.html ;";

ping ${3}


# Rscript -e 'rmarkdown::render_site()' ;

# find -name '*.html' -type f -exec curl -u $1:$2 -T {} ${3}dev/ \;
