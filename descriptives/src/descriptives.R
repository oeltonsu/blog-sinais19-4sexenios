#
# Author: CT, OE
# Mainteiner(s): CT, OE
# Copyright: Data Cívica © 2020
# ===================================================
# blog-sinais19-armas/descriptives/src/descriptives.R

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, ggalluvial, here)

files <- list(sinais = here("import-clean/output/sinais90-19.rds"),
              pob = here("import-clean/output/pob.rds"),
              bump = here("descriptives/output/bump-edos.png"),
              theme = here("descriptives/src/theme-setup.R")
              )

source(files$theme)

sinais <- readRDS(files$sinais) %>% 
  filter(year>=2001 & as.numeric(cve_ent) <= 32) %>% 
  mutate(sexenio = case_when(year %in% 2001:2006 ~ "Fox",
                             year %in% 2007:2012 ~ "Calderón",
                             year %in% 2013:2018 ~ "EPN",
                             T ~ "AMLO"),
         sexenio = factor(sexenio, levels = c("Fox", "Calderón", "EPN", "AMLO"))) %>% 
  group_by(cve_ent, year, sexenio) %>% 
  summarise(tot_hom = n()) %>% 
  ungroup() %>% 
  left_join(readRDS(files$pob) %>% 
              filter(year >= 2001) %>% 
              group_by(cve_ent, entidad, year) %>% 
              summarise(pob = sum(poblacion)) %>% 
              ungroup()) %>% 
  mutate(tasa = tot_hom*(100000/pob)) %>% 
  group_by(cve_ent, entidad, sexenio) %>% 
  summarise(tot_hom = sum(tot_hom),
            tasa = weighted.mean(tasa, pob)) %>% 
  ungroup() %>% 
  group_by(sexenio) %>% 
  mutate(ranking = rank(-tasa)) %>% 
  ungroup()
  
ggplot(sinais, aes(x = sexenio, y = ranking, group = entidad)) +
  geom_line(aes(color = entidad, alpha = 1), size = 2) +
  geom_point(aes(color = entidad, alpha = 1), size = 3) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:max(sinais$ranking)) + 
  geom_text(data = sinais %>% filter(sexenio == "Fox"), aes(label = entidad, x = "Fox"), hjust = 1.1, size = 4) +
  geom_text(data = sinais %>% filter(sexenio == "AMLO"), aes(label = entidad, x = "AMLO"), hjust = -0.1, size = 4) +
  labs(title = "Entidades rankeadas por tasa de homicidio promedio",
       subtitle = "Por sexenio",
       x = "Sexenio", y = "Ranking por tasa de homicidio promedio", 
       caption = "FUENTE: Elaboración propia con datos de Mortalidad General, INEGI 2001-2019 y CONAPO.\nPara el sexenio de AMLO únicamente se cuenta 2019.") +
  tema +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14))

ggsave(files$bump, width = 12, height = 7)
