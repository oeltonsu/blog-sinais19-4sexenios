#
# Author: CT, OE
# Mainteiner(s): CT, OE
# Copyright: Data Cívica © 2020
# ===================================================
# blog-sinais19-armas/descriptives/src/descriptives.R

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, ggalluvial, here)

files <- list(input = here("import-clean/output/sinais90-19.rds"),
              sankey = here("descriptives/output/sankey-edos.")
              )

sinais <- readRDS(files$input) %>% 
  filter(year>=2001) %>% 
  mutate(sexenio = case_when(year %in% 2001:2006 ~ "Fox",
                             year %in% 2007:2012 ~ "Calderón",
                             year %in% 2013:2018 ~ "EPN",
                             T ~ "AMLO"),
         cve_ent = case_when(as.numeric(cve_ent) > 32 ~ "99", T ~ cve_ent)) %>% 
  group_by(cve_ent, sexenio) %>% 
  summarise(tot_ent = n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = cve_ent, names_from = sexenio, values_from = tot_ent)

ggplot(as.data.frame(sinais), aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = Admit), width = 1/12)
