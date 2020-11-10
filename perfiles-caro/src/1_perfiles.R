rm(list = ls())

pacman::p_load(tidyverse,
               purrr, 
               here) 

# ugh wey no me sirve here en esta compu cuando es git por alguna razón
# entonces tú disculparas mis directorios puercos

main <- "~/Documents/proyectos/blog-sinais19-armas"
 inp <- paste(main, "import-clean/output", sep = "/")
 out <-  paste(main, "perfiles-caro/output", sep = "/")
 src <-  paste(main, "perfiles-caro/src", sep = "/")
 
 # Mi tema
 source(paste0(src, "/0_tema.R"))
 
 # 1. Read
 full_sin <- read_rds(paste(inp, "sinais90-19.rds", sep = "/"))
 full_pop <- read_rds(paste(inp, "pob.rds", sep = "/"))
 
 # 2. Make sexenios
 