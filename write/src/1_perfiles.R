rm(list = ls())

pacman::p_load(tidyverse,
               purrr, 
               here) 

# ugh wey no me sirve here en esta compu cuando es git por alguna razón
# entonces tú disculparas mis directorios puercos

main <- "~/Documents/proyectos/blog-sinais19-armas"

inp <.
 inp <- paste(main, "import-clean/output", sep = "/")
 out <-  paste(main, "perfiles-caro/output", sep = "/")
 src <-  paste(main, "perfiles-caro/src", sep = "/")
 
 # Mi tema
 source(paste0(src, "/0_tema.R"))
 
 # 1. Read
 full_sin <- read_rds("/Users/oscarelton/Documents/blog-sinais19-armas/import-clean/output/sinais90-19.rds")
 full_pop <- read_rds("/Users/oscarelton/Documents/blog-sinais19-armas/import-clean/output/pob.rds")
 
 # 2. Make sexenios
 s_c <- full_sin %>%
   filter(year > 2000) %>%
   mutate(
     tot = 1,
     sexenio = case_when(
       year > 2000 & year < 2007 ~ "Fox",
       year >= 2007 & year < 2013 ~ "Calderón",
       year >= 2013 & year < 2019 ~ "EPN",
       year == 2019 ~ "AMLO"
     )
   )
   
# ----- Gráficas ------ #
 
 
 sex_list <- seq(from = 2006, to = 2018, by = 6)
 
 data_graf <- s_c %>%
   ungroup() %>%
   group_by(year) %>%
   summarize(tot = sum(tot)) %>%
   filter(year %in% sex_list)
 
 pob_porsex <- full_pop %>%
   group_by(year, sexo) %>%
   summarize(pob = sum(poblacion))
 
# sobreponer el acumulado
 
 cumsum <- s_c %>% 
   ungroup() %>% 
   group_by(year, sexo) %>% 
   summarize(total = sum(tot, na.rm = T)) %>% 
   left_join(pob_porsex,
             by = c("year", "sexo")) %>% 
   ungroup() %>% 
   group_by(sexo) %>% 
   mutate(tasa = total / pob * 100000,
      acumulado = cumsum(total),
          lag = lag(tasa),
          cambio = (tasa - lag) / lag * 100) 

 
 # Acumulado por sexo
 cumsum %>% 
   filter(is.na(sexo)==F) %>% 
   ggplot() +
   geom_vline(data = data_graf, 
              aes(xintercept = year),
              color = "gray45", 
              linetype = "dashed",
              size= 1.5) +
   geom_line(aes(x = year, 
                 y = acumulado,
                 color = sexo),
             size = 2) +
   geom_point(aes(x = year, 
                  y = acumulado,
                  group = sexo),
              size = 3) + 
   ggrepel::geom_text_repel(aes(x = year, 
                 y = acumulado,
                 group = sexo,
                 label = round(acumulado / 1000, 1)),
                 size = 5,
                 family = "Lato", hjust = .7) + 
   tema +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) +
   labs(fill = "", 
        title = "¿Cuántas personas han sido asesinadas en los últimos cuatro sexenios?",
        subtitle = "Acumulado por sexo",
        y = "Homicidios acumulados (miles)", x = "", color = "") +
   scale_color_manual(values = c(fill_dos)) +
   scale_x_continuous(breaks = seq(from = 2001, to = 2019, by = 1))  
   ggsave(paste(out, "0_baseline.png", sep="/"), width=18, height=15) 
 # Cambio porcentual en totales
   
   cumsum %>% 
     filter(is.na(sexo)==F & is.na(cambio)==F) %>% 
     ggplot() +
     geom_vline(data = data_graf, 
                aes(xintercept = year),
                color = "gray45", 
                linetype = "dashed",
                size= 1.5) +
     geom_hline(data = data_graf, 
                aes(yintercept = 0),
                color = "maroon", 
                size= 1.5) +
     geom_line(aes(x = year, 
                   y = cambio,
                   color = sexo),
               size = 2) +
     geom_point(aes(x = year, 
                    y = cambio,
                    group = sexo),
                size = 3) + 
     ggrepel::geom_text_repel(aes(x = year, 
                                  y = cambio,
                                  group = sexo,
                                  label = paste0(round(cambio, 1), "%")),
                              size = 5,
                              family = "Lato", hjust = 1) + 
     tema +
     labs(fill = "", 
          title = "Cambio porcentual en tasa de personas asesinadas",
          subtitle = "Acumulado por sexo",
          y = "Cambio % en la tasa", x = "", color = "") +
     scale_color_manual(values = c(fill_dos)) +
     scale_x_continuous(breaks = seq(from = 2001, to = 2019, by = 1))  
   ggsave(paste(out, "0_2_baseline_tasa.png", sep="/"), width=18, height=15) 

# 0. Totales por grupo 
 g0 <- s_c %>%
   group_by(rango_edad,
            sexenio, 
            year) %>%
   filter(is.na(rango_edad)==F) %>% 
   summarize(total = sum(tot, na.rm = T)) %>%
   ungroup() %>% 
   group_by(year) %>%
   mutate(total_year = sum(total),
          porcent = round(total / total_year * 100, 2),
          sexenio = factor(sexenio,
                           levels = c("Fox", "Calderón", 
                                      "EPN", "AMLO")))

 g0 %>% 
   ggplot() +
   geom_area(aes(x = year, 
                 y = total,
                 fill = rango_edad)) + 
   geom_vline(data = data_graf, 
              aes(xintercept = year),
              color = "gray45", linetype = "dashed",
              size= 2) +
   geom_point(data = filter(g0,
                            rango_edad=="35 - 44 años"),
              aes(x = year, 
                  y = total_year,
                  group = rango_edad),
              position = "stack") + 
   ggrepel::geom_text_repel(data = filter(g0,
                            rango_edad=="35 - 44 años"),
              aes(x = year, 
                  y = total_year,
                  group = rango_edad,
                  label = total_year),
              position = "stack",
              size = 5,
              family = "Lato") + 
   tema +
   labs(fill = "Rango de edad", 
        title = "¿Cuántas personas han muerto en los últimos cuatro sexenios?",
        subtitle = "Por rango de edad",
        y = "", x = "") +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) +
   scale_fill_manual(values = fill_base ) +
   scale_x_continuous(breaks = seq(from = 2001, to = 2019, by = 1))
   ggsave(paste(out, "1_tot-edades.png", sep="/"), width=18, height=15) 
   
  
# Por tipo 
   
   g0 <- s_c %>%
     mutate(causa_hom = ifelse(causa_hom == "Fuerza corporal" |
                               causa_hom == "Envenenamiento o sustancia corrosiva", "Otro", 
                               causa_hom)) %>% 
     group_by(causa_hom,
              sexenio, 
              year) %>%
     filter(is.na(causa_hom)==F) %>% 
     summarize(total = sum(tot, na.rm = T)) %>%
     ungroup() %>% 
     group_by(year) %>%
     mutate(total_year = sum(total),
            porcent = round(total / total_year * 100, 2),
            sexenio = factor(sexenio,
                             levels = c("Fox", "Calderón", 
                                        "EPN", "AMLO")),
            causa_hom = factor(causa_hom,
                               levels = c("Arma de fuego", "Arma blanca",
                                          "No especificado", 
                                          "Ahorcamiento o ahogamiento",
                                          "Otro"))) 
   
 
 g0 %>% 
   ggplot() +
   geom_area(aes(x = year, 
                 y = total,
                 fill = causa_hom)) + 
   geom_vline(data = data_graf, 
              aes(xintercept = year),
              color = "gray45", linetype = "dashed",
              size= 2) +
   geom_point(data = filter(g0,
                            causa_hom=="Arma blanca"),
              aes(x = year, 
                  y = total_year,
                  group = causa_hom),
              position = "stack") + 
   ggrepel::geom_text_repel(data = filter(g0,
                                          causa_hom=="Arma blanca"),
                            aes(x = year, 
                                y = total_year,
                                group = causa_hom,
                                label = total_year),
                            position = "stack",
                            size = 5,
                            family = "Lato") + 
   tema +
   labs(fill = "", 
        title = "¿Cuántas personas han muerto en los últimos cuatro sexenios?",
        subtitle = "Por causa de muerte registrada",
        y = "", x = "") +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) +
   scale_fill_manual(values = fill_base ) +
   scale_x_continuous(breaks = seq(from = 2001, to = 2019, by = 1))
 ggsave(paste(out, "2_tot-metodos.png", sep="/"), width=18, height=15) 
 
 rm(g0, pob_porsex, data_graf, cumsum)

# 1. Perfiles más comunes
 
 pob_poredad <- full_pop %>% 
   group_by(year,
            sexo,
            rango_edad) %>% 
   summarize(pob = sum(poblacion)) %>% 
   ungroup() %>% 
   mutate(
     sexenio = case_when(
                   year > 2000 & year < 2007 ~ "Fox",
                   year >= 2007 & year < 2013 ~ "Calderón",
                   year >= 2013 & year < 2019 ~ "EPN",
                   year == 2019 ~ "AMLO"
                 )
   ) %>% 
   filter(is.na(sexenio)==F) %>% 
   group_by(sexenio,
            sexo,
            rango_edad) %>% 
   summarize(pob_prom  = mean(pob))

 g1 <- s_c %>% 
   group_by(sexo,
            rango_edad, 
            lugar,
            causa_hom,
            sexenio) %>% 
   summarize(total = sum(tot, na.rm = T)) %>% 
   ungroup() %>% 
   left_join(pob_poredad,
             by = c("rango_edad",
                    "sexo", 
                    "sexenio")) %>% 
   group_by(sexenio) %>% 
   mutate(denomin = sum(total),
          porcent = round(total / denomin * 100, 2),
          tasa = total / pob_prom * 10000) %>% 
   slice_max(order_by = total,
             n = 1)
 
# 2. Perfiles más comunes por sexo
 
 g2 <- s_c %>% 
   group_by(sexo,
            rango_edad, 
            lugar,
            causa_hom,
            sexenio) %>% 
   summarize(total = sum(tot, na.rm = T)) %>% 
   ungroup() %>% 
   left_join(pob_poredad,
             by = c("rango_edad",
                    "sexo", 
                    "sexenio")) %>% 
   group_by(sexenio, sexo) %>% 
   mutate(denomin = sum(total),
          porcent = round(total / denomin * 100, 2),
          tasa = total / pob_prom * 10000) %>% 
   slice_max(order_by = total,
             n = 1) %>% 
   filter(is.na(sexo)==F)
 
 
# 3. Perfiles más comunes por edo y sexo

 g3 <- s_c %>% 
   group_by(sexo,
            rango_edad, 
            lugar,
            causa_hom,
            sexenio,
            cve_ent) %>% 
   summarize(total = sum(tot, na.rm = T)) %>% 
   ungroup() %>% 
   group_by(sexenio, 
            sexo) %>% 
   mutate(denomin = sum(total),
          porcent = round(total / denomin * 100, 2)) %>% 
   slice_max(order_by = total,
             n = 1) %>% 
   filter(is.na(sexo)==F)
 
 
 
 
 
 