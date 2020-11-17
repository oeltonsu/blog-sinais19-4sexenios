# -*- coding: utf-8 -*-
#
# Author: OE
# Maintainer(s): OE, CT 
# License: (c) Data Cívica 2020, GPL v2	or newer
# -----------------------------------------------------------
# blog-sianis19-armada/write/Makefile
#

.PHONY: all

all: output/blog-4sexenios.docx

clean:
	rm -r output/*

R_OPTS=--no-save --no-restore --no-init-file --no-site-file
KNIT="rmarkdown::render(input='$<', output_dir='output/')"

output/blog-4sexenios.docx: \
		src/blog-4sexenios.Rmd
	Rscript $(R_OPTS) -e $(KNIT)

# done.