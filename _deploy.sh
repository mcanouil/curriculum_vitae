#!/bin/bash

Rscript -e 'rmarkdown::render(input = "curriculum_vitae/curriculum_vitae.Rmd", output_dir = ".", output_file = "cv.html", encoding = "UTF-8")' ;

curl -u $1:$2 -T cv.html $3/index.html ;

# Rscript -e 'rmarkdown::render_site()' ;
# 
# find -name '*.html' -type f -exec curl -u $1:$2 -T {} $4 \;
