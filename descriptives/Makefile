# -*- coding: utf-8 -*-
#
# Author: OE
# Maintainer(s): OE, CT 
# License: (c) Data Cívica 2020, GPL v2	or newer
# -----------------------------------------------------------
# blog-sianis19-armada/descriptives/Makefile
#

.PHONY: all clean

all: output/fiebre-formas.png \
	output/fiebre-edades.png \
	output/bump-edos.png \
	output/baseline-tasa.png

clean: 
	rm output/*

GRAPHS := output/fiebre-formas.png \
	output/fiebre-edades.png \
	output/bump-edos.png \
	output/baseline-tasa.png

$(GRAPHS): src/descriptives.R	
	Rscript --vanilla $<
	rm Rplots.pdf
# done .
