#!/bin/bash

Rscript -e 'rmarkdown::render(input = "curriculum_vitae/curriculum_vitae.Rmd", encoding = "UTF-8")' ;

Rscript -e 'rmarkdown::render_site()' ;

echo "curl -u $1:$2 -T curriculum_vitae/curriculum_vitae.html ${3}index.html ;";
ping ${3}index.html

# find -name '*.html' -type f -exec curl -u $1:$2 -T {} ${3}dev/ \;
