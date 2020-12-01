#
# Author: CT, OE
# Mainteiner(s): CT, OE
# Copyright: Data Cívica © 2020
# ===================================================
# blog-sinais19-armas/descriptives/src/descriptives.R

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here)

files <- list(sinais = here("import-clean/output/sinais90-19.rds"),
              pob = here("import-clean/output/pob.rds"),
              bump = here("descriptives/output/bump-edos.png"),
              baseline = here("descriptives/output/baseline-tasa.png"),
              edades = here("descriptives/output/fiebre-edades.png"),
              formas = here("descriptives/output/fiebre-formas.png"),
              theme = here("descriptives/src/theme-setup.R")
              )

source(files$theme)

sinais <- readRDS(files$sinais) %>% 
  filter(year>=2001) %>% 
  mutate(sexenio = case_when(year %in% 2001:2006 ~ "Fox",
                             year %in% 2007:2012 ~ "Calderón",
                             year %in% 2013:2018 ~ "EPN",
                             T ~ "AMLO"),
         sexenio = factor(sexenio, levels = c("Fox", "Calderón", "EPN", "AMLO")))

# ==== ¿Cómo se ha movido geográficamente la violencia?
sinais %>% 
  filter(as.numeric(cve_ent) <= 32) %>% 
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
  ungroup() %>% 
  {
    ggplot(data = ., aes(x = sexenio, y = ranking, group = entidad)) +
      geom_line(aes(color = entidad, alpha = 1), size = 2) +
      geom_point(aes(color = entidad, alpha = 1), size = 3) +
      geom_point(color = "#FFFFFF", size = 1) +
      scale_y_reverse(breaks = 1:max(.$ranking)) + 
      scale_color_manual(values = c("#99d8c9", "#2ca25f", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15", "#bdbdbd")) +
      geom_text(data = . %>% filter(sexenio == "Fox"), aes(label = entidad, x = "Fox"), hjust = 1.1, size = 4) +
      geom_text(data = . %>% filter(sexenio == "AMLO"), aes(label = entidad, x = "AMLO"), hjust = -0.1, size = 4) +
      labs(title = "Entidades rankeadas por tasa de homicidio promedio",
           subtitle = "Por sexenio",
           x = "Sexenio", y = "Ranking por tasa de homicidio promedio", 
           caption = "FUENTE: Elaboración propia con datos de Mortalidad General, INEGI 2001-2019 y CONAPO.\nPara el sexenio de AMLO únicamente se cuenta 2019.") +
      tema +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 14)) +
      scale_color_manual(values = c("#a1d99b", "#fcbba1", "#fc9272", "#bdbdbd", "#bdbdbd",
                                    "#bdbdbd", "#bdbdbd", "#fb6a4a", "#bdbdbd", "#bdbdbd",
                                    "#a50f15", "#de2d26", "#bdbdbd", "#bdbdbd", "#bdbdbd",
                                    "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd",
                                    "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd",
                                    "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd",
                                    "#31a354", "#bdbdbd"))
  }

ggsave(files$bump, width = 12, height = 7)

# ==== Cambio porcentual en la tasa de homicidios
sinais %>% 
  filter(!is.na(sexo)) %>% 
  group_by(year, sexo) %>% 
  summarize(total = n()) %>% 
  ungroup() %>% 
  left_join(readRDS(files$pob) %>%
              group_by(year, sexo) %>%
              summarise(pob = sum(poblacion)) %>% 
              ungroup()) %>% 
  group_by(sexo) %>% 
  mutate(tasa = total*(100000/pob),
         acumulado = cumsum(total),
         lag = lag(tasa),
         cambio = ((tasa - lag)/lag)*100) %>% 
  {
    ggplot() +
      geom_vline(aes(xintercept = seq(2006, 2019, 6)), color = "gray45", linetype = "dashed", size= 1.5) +
      geom_hline(data = ., aes(yintercept = 0), color = "maroon", size= 1.5) +
      geom_line(data = ., aes(x = year, y = cambio, color = sexo), size = 2) +
      geom_point(data = ., aes(x = year, y = cambio, group = sexo), size = 3) + 
      geom_text_repel(data = ., aes(x = year, y = cambio, group = sexo, label = paste0(round(cambio, 1), "%")),
                               size = 5, hjust = 1) + 
      tema +
      labs(fill = "", 
           title = "Cambio porcentual en tasa de personas asesinadas",
           subtitle = "Acumulado por sexo",
           y = "Cambio % en la tasa", x = "", color = "",
           caption = "FUENTE: Elaboración propia con datos de Mortalidad General, INEGI 2001-2019 y CONAPO.") +
      scale_color_manual(values = c(pal_2)) +
      scale_x_continuous(breaks = seq(from = 2001, to = 2019, by = 1))
  }

ggsave(files$baseline, width = 14, height = 10)


# ==== Fiebre de totales por grupo de edad
sinais %>%
  filter(is.na(rango_edad)==F) %>% 
  group_by(rango_edad, sexenio, year) %>%
  summarise(total = n()) %>%
  ungroup() %>% 
  group_by(year) %>%
  mutate(total_year = sum(total),
         porcent = round(total / total_year * 100, 2)) %>% 
  {
    ggplot() +
      geom_area(data = ., aes(x = year, y = total, fill = rango_edad)) + 
      geom_vline(aes(xintercept = seq(2006, 2019, 6)), color = "gray45", linetype = "dashed", size= 1.5) +
      geom_point(data = filter(., rango_edad=="35 - 44 años"),
                 aes(x = year, y = total_year, group = rango_edad),position = "stack") + 
      geom_text_repel(data = filter(., rango_edad=="35 - 44 años"),
                               aes(x = year, y = total_year, group = rango_edad, label = total_year),
                               position = "stack", size = 5) + 
      tema +
      labs(fill = "Rango de edad", 
           title = "¿Cuántas personas han muerto en los últimos cuatro sexenios?",
           subtitle = "Por rango de edad",
           y = "", x = "",
           caption = "FUENTE: Elaboración propia con datos de Mortalidad General, INEGI 2001-2019.") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_fill_manual(values = pal_7) +
      scale_x_continuous(breaks = seq(from = 2001, to = 2019, by = 1))
  }
  
ggsave(files$edades, width = 14, height = 10)

# ==== Fiebre de totales por forma de asesinato
sinais %>%
  mutate(causa_hom = ifelse(causa_hom %in% c("Fuerza corporal","Envenenamiento o sustancia corrosiva"), 
                                             "Otro", causa_hom)) %>% 
  group_by(causa_hom, sexenio, year) %>%
  filter(is.na(causa_hom)==F) %>% 
  summarise(total = n()) %>%
  ungroup() %>% 
  group_by(year) %>%
  mutate(total_year = sum(total),
         porcent = round(total / total_year * 100, 2),
         causa_hom = factor(causa_hom,
                            levels = c("Arma de fuego", "Arma blanca",
                                       "No especificado", 
                                       "Ahorcamiento o ahogamiento",
                                       "Otro"))) %>% 
  {
    ggplot() +
      geom_area(data = ., aes(x = year, y = total, fill = causa_hom)) + 
      geom_vline(aes(xintercept = seq(2006, 2019, 6)), color = "gray45", linetype = "dashed", size= 1.5) +
      geom_point(data = filter(., causa_hom=="Arma blanca"), 
                 aes(x = year, y = total_year, group = causa_hom), position = "stack") + 
      geom_text_repel(data = filter(., causa_hom=="Arma blanca"),
                               aes(x = year, y = total_year, group = causa_hom, label = total_year),
                               position = "stack", size = 5) + 
      tema +
      labs(fill = "", 
           title = "¿Cuántas personas han muerto en los últimos cuatro sexenios?",
           subtitle = "Por causa de muerte registrada",
           y = "", x = "",
           caption = "FUENTE: Elaboración propia con datos de Mortalidad General, INEGI 2001-2019.") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_fill_manual(values = pal_7) +
      scale_x_continuous(breaks = seq(from = 2001, to = 2019, by = 1))
  }

ggsave(files$formas, width = 14, height = 10)

# done.
