#!/bin/bash

Rscript -e 'rmarkdown::render_site()' ;

curl -u $1:$2 -T docs/curriculum_vitae.html $3/index.html ;

# find -name '*.html' -type f -exec curl -u $1:$2 -T {} $4 \;
