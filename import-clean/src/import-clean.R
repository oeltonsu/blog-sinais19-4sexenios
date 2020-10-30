#
# Author: OE
# Maintainers: OE, CT
# Copyright © Data Cívica 2020
# ====================================================
# blog-sinais19-armada/import-clean/src/import-clean.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, foreign, R.utils, data.table, janitor, stringi, here)

files <- list(input = here("import-clean/input/"),
              sinais = here("import-clean/output/sinais90-19.rds"),
              pob = here("import-clean/output/pob.rds")
              )

clean_text <- function(s){
  str_squish(toupper(stri_trans_general(s, "latin-ascii")))
}

clean_acentos <- function(x) {
  x <- gsub("\xa0", "á", x)
  x <- gsub("\x82", "é", x)
  x <- gsub("\xa1", "í", x)
  x <- gsub("\xa2", "ó", x)
  x <- gsub("\xa4", "ñ", x)
  x <- gsub("\xa3", "ú", x)
}

# ==== Patrones de lugar y causa
patP_viv <- "EN VIVIENDA|INSTITUCION RESIDENCIAL"
patP_via <- "CALLES Y CARRETERAS|AREA INDUSTRIAL Y DE LA CONSTRUCCION|AREAS DE DEPORTE Y ATLETISMO|COMERCIO Y AREA DE SERVICIOS|EN ESCUELAS"
patP_otr <- "OTRO LUGAR ESPECIFICADO|GRANJA"
patP_ne <- "LUGAR NO ESPECIFICADO"

patC_af <- "ARMA DE FUEGO|ARMAS DE FUEGO|ARMA LARGA|ARMA CORTA"
patC_ab <- "OBJETO CORTANTE|OBJETO ROMO O SIN FILO|CUCHILLO"
patC_ahr <- "AHORCAMIENTO|AHOGAMIENTO"
patC_env <- "DROGAS, MEDICAMENTOS Y SUSTANCIAS BIOLOGICAS|PRODUCTOS QUIMICOS Y SUSTANCIAS NOCIVAS|PLAGUICIDAS|SUSTANCIA CORROSIVA"
patC_fc <- "FUERZA CORPORAL|GOLPE CONTRA|APORREO"
patC_ne <- "MEDIOS NO ESPECIFICADOS"

keep_vars <- c("cve_ent", "sexo", "edad", "rango_edad", "anio_ocur", "lugar", "causa_hom")

# ==== Catalogo de causas
causas <- read.dbf(gunzip(paste0(files$input, "/catminde.dbf.gz"), overwrite=T, remove=F), as.is=T) %>% 
  clean_names() %>% 
  mutate(descrip = clean_acentos(descrip),
         descrip = clean_text(descrip)) %>% 
  rename(causa_def = cve)

file.remove(paste0(files$input, "/catminde.dbf"))
  
# ==== SINAIS 
defun <- dir(files$input, pattern = "defun")

sinais <- data.frame()  

pb <- txtProgressBar(min=1, max=length(defun), style=3)
for (i in 1:length(defun)){
  tempo <- read.dbf(gunzip(paste0(files$input, defun[i]), overwrite=T, remove=F), as.is = T) %>% 
    clean_names() %>% 
    filter(presunto==2) %>% 
    left_join(causas) %>%
    mutate(lugar = case_when(descrip %like% patP_viv ~ "Vivienda",
                             descrip %like% patP_via ~ "Vía pública",
                             descrip %like% patP_otr ~ "Otro",
                             descrip %like% patP_ne ~ "No especificado",
                             T ~ NA_character_),
           causa_hom = case_when(descrip %like% patC_af ~ "Arma de fuego",
                                 descrip %like% patC_ab ~ "Arma blanca",
                                 descrip %like% patC_ahr ~ "Ahorcamiento o ahogamiento",
                                 descrip %like% patC_env ~ "Envenenamiento o sustancia corrosiva",
                                 descrip %like% patC_fc ~ "Fuerza corporal",
                                 descrip %like% patC_ne ~ "No especificado",
                                 T ~ "Otro"),
           cve_ent = str_pad(ent_ocurr, width = 2, side = "left", pad = "0"),
           sexo = case_when(sexo==1 ~ "Hombres",
                            sexo==2 ~ "Mujeres",
                            T ~ NA_character_),
           edad = as.integer(edad),
           edad = case_when(substr(edad, 1, 2) %in% 40:41 ~ as.integer(substr(edad, 2, 4)),
                            substr(edad, 1, 2) < 40 ~ as.integer(0),
                            T ~ NA_integer_),
           rango_edad = case_when(edad %in% 1:11 ~ "1 - 11 años",
                                  edad %in% 12:17 ~ "12 - 17 años",
                                  edad %in% 18:24 ~ "18 - 24 años",
                                  edad %in% 25:34 ~ "25 - 34 años",
                                  edad %in% 35:44 ~ "35 - 44 años",
                                  edad %in% 45:64 ~ "45 - 64 años",
                                  edad >= 65 ~ "65 años o más",
                                  T ~ NA_character_)) %>% 
    select(one_of(keep_vars))
  sinais <- bind_rows(sinais, tempo)
  file.remove(paste0(files$input, str_replace(defun[i], ".gz", "")))
  rm(tempo)
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb, i)

sinais <- filter(sinais, anio_ocur %in% 1990:2019) %>% 
  arrange(cve_ent, anio_ocur, sexo, edad, lugar, causa_hom) %>% 
  rename(year = anio_ocur)

saveRDS(sinais, files$sinais)

# ==== Población

pob <- read_csv(paste0(files$input, "pob_mit_proyecciones.csv.gz")) %>% 
  rename(year = ano,
         cve_ent = cve_geo) %>% 
  mutate(cve_ent = str_pad(cve_ent, width = 2, side = "left", pad = "0"),
         rango_edad = case_when(edad %in% 1:11 ~ "1 - 11 años",
                                edad %in% 12:17 ~ "12 - 17 años",
                                edad %in% 18:24 ~ "18 - 24 años",
                                edad %in% 25:34 ~ "25 - 34 años",
                                edad %in% 35:44 ~ "35 - 44 años",
                                edad %in% 45:64 ~ "45 - 64 años",
                                edad >= 65 ~ "65 años o más",
                                T ~ NA_character_)) %>% 
  filter(cve_ent!="00" & year %in% 1990:2019)

saveRDS(pob, files$pob)

# done.
