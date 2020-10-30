# -*- coding: utf-8 -*-
#
# Author: OE
# Maintainer(s): OE, CT 
# License: (c) Data Cívica 2020, GPL v2	or newer
# -----------------------------------------------------------
# blog-sianis19-armada/import_clean/Makefile
#

.PHONY: all clean

all: output/sinais.rds \
	output/pob.rds 

clean: 
	rm output/*

output/sinais.rds output/pob.rds: \
		src/import-clean.R	
	Rscript --vanilla $<

# done .

