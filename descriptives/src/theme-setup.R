#
# Author: Oscar Elton
# Maintainers: OE, CT
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# --------------------------------------------------
# blog-mortalidad-hospitales/grafs/src/theme-setup.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, extrafont, ggrepel, treemapify, viridis, scales, svglite)
extrafont::loadfonts(quiet = T)

tema <- theme_minimal() +
  theme(plot.title = element_text(size = 16, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 10, family = "Barlow Condensed", face = "bold"),
        axis.title = element_text(size = 12, family = "Barlow Condensed"),
        legend.text = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        legend.title = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"),
        text = element_text(family = "Barlow Condensed"),
        legend.position="top")


pal_7 <-  c("#00FAE1","#4FAAAC","#5104BC","#78069E","#9F0780","#C50961","#EC0B43")
pal_2 <-  c("#525252","#cccccc")

devices <- c("png", "svg")

# done.
