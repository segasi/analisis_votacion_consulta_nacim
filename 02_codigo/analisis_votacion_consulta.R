### Paquetes ----
library(pacman)
p_load(scales, tidyverse)

### Cargar datos ----
bd <- read_csv("01_datos/resultado_mesas20181029-1830.csv")

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))


### Calcular diversas métricas ----

## Votación total por casilla ----
voto_por_casilla <- bd %>% 
  group_by(idcasilla) %>% 
  summarise(estado = last(estado),
            municipio = last(municipio),
            casilla = last(casilla),
            suma_sta_lucia = sum(Opcion1_Actual_mas_Toluca_y_StLucia),
            suma_texcoco = sum(Opcion2_continuar_construccion_en_texcoco),
            suma_nulos = sum(nulos),
            suma_total = sum(total)) %>% 
  ungroup() %>% 
  mutate(gano_texcoco = ifelse(suma_texcoco >= suma_sta_lucia, "Opción de continuar la construcción del NAICM en Texcoco", "Opción AICM + Toluca + Santa Lucía")) 