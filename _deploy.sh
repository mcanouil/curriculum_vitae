#!/bin/bash

Rscript -e 'rmarkdown::render_site(); unlink(c("_site/README.html", "_site/DESCRIPTION"))' ;

curl -u $1:$2 -T _site/curriculum_vitae.html $3/index.html ;

find -name '*.html' -type f -exec curl -u $1:$2 -T {} $4 \;
