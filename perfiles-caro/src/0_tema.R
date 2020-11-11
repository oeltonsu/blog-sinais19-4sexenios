options(scipen=999)


fill_base <-  c("#00FAE1","#4FAAAC","#5104BC","#78069E","#9F0780","#C50961","#EC0B43")
 fill_dos <-  c("#525252","#cccccc")


 tema <-  theme_linedraw() +
   theme(
     text = element_text(color = "grey35", family = "Lato"),
     plot.title = element_text(
       size = 30,
       face = "bold",
       margin = margin(10, 1, 5, 1),
       color = "black"
     ),
     plot.subtitle = element_text(
       size = 22,
       face = "bold",
       color = "#666666",
       margin = margin(1, 2, 5, 1)
     ),
     plot.caption = element_text(hjust = 0, size = 17),
     panel.grid = element_line(linetype = 2),
     legend.position = "top",
     panel.grid.minor = element_blank(),
     legend.title = element_text(
       size = 18,
       face = "bold",
       family = "Lato Bold"
     ),
     legend.text = element_text(size = 18),
     legend.title.align = 0.5,
     axis.title = element_text(
       size = 16,
       hjust = .25,
       face = "bold",
       family = "Lato"
     ),
     axis.text = element_text(
       size = 18,
       face = "bold",
       family = "Lato"
     ),
     strip.background = element_rect(fill = "#525252"),
     strip.text.x = element_text(
       size = 18,
       face = "bold",
       family = "Lato"
     ),
     strip.text.y = element_text(
       size = 18,
       face = "bold",
       family = "Lato"
     )
   )

