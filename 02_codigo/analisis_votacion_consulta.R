### Paquetes ----
library(pacman)
p_load(scales, tidyverse, treemapify)

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
            suma_sta_lucia = sum(Opcion1_Actual_mas_Toluca_y_StLucia, na.rm = T),
            suma_texcoco = sum(Opcion2_continuar_construccion_en_texcoco, na.rm = T),
            suma_nulos = sum(nulos, na.rm = T),
            suma_total = sum(total, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(por_sta_lucia = round((suma_sta_lucia/suma_total)*100, 1),
         por_texcoco = round((suma_texcoco/suma_total)*100, 1),
         opcion_ganadora = ifelse(suma_texcoco >= suma_sta_lucia, "Opción de continuar la construcción del NAICM en Texcoco", "Opción AICM + Toluca + Santa Lucía"))

## Votación total por municipio ----
voto_por_mpo <- bd %>% 
  group_by(municipio) %>% 
  summarise(estado = last(estado),
            suma_sta_lucia = sum(Opcion1_Actual_mas_Toluca_y_StLucia, na.rm = T),
            suma_texcoco = sum(Opcion2_continuar_construccion_en_texcoco, na.rm = T),
            suma_nulos = sum(nulos, na.rm = T),
            suma_total = sum(total, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(por_sta_lucia = round((suma_sta_lucia/suma_total)*100, 1),
         por_texcoco = round((suma_texcoco/suma_total)*100, 1),
         opcion_ganadora = ifelse(suma_texcoco >= suma_sta_lucia, "Opción de continuar la construcción del NAICM en Texcoco", "Opción AICM + Toluca + Santa Lucía"))

## Votación total por estado ----
voto_por_edo <- bd %>% 
  group_by(estado) %>% 
  summarise(suma_sta_lucia = sum(Opcion1_Actual_mas_Toluca_y_StLucia, na.rm = T),
            suma_texcoco = sum(Opcion2_continuar_construccion_en_texcoco, na.rm = T),
            suma_nulos = sum(nulos, na.rm = T),
            suma_total = sum(total, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(por_sta_lucia = round((suma_sta_lucia/suma_total)*100, 1),
         por_texcoco = round((suma_texcoco/suma_total)*100, 1),
         opcion_ganadora = ifelse(suma_texcoco >= suma_sta_lucia, "Opción de continuar la construcción del NAICM en Texcoco", "Opción AICM + Toluca + Santa Lucía"))

## Votación acumulada por casilla ----
voto_acumulado_por_casilla <- 
  bd %>% 
  arrange(idcasilla, dia) %>% 
  group_by(idcasilla) %>% 
  mutate(acumulado_sta_lucia = cumsum(Opcion1_Actual_mas_Toluca_y_StLucia),
         acumulado_texcoco = cumsum(Opcion2_continuar_construccion_en_texcoco),
         acumulado_nulos =cumsum(nulos),
         acumulado_total = cumsum(total),
         opcion_ganadora = ifelse(acumulado_texcoco >= acumulado_sta_lucia, "Opción de continuar la construcción del NAICM en Texcoco", "Opción AICM + Toluca + Santa Lucía")) 
  
# Gráfica: Votos acumulados diariamente a favor de AICM, Toluca y Santa Lucía ----
voto_acumulado_por_casilla %>% 
  ggplot() +
  geom_line(aes(dia, acumulado_sta_lucia, group = idcasilla),
            color = "salmon", alpha = 0.5) +
  scale_y_continuous(limits = c(0, 5000), label = comma) +
  facet_wrap(~ estado, ncol = 8) +
  labs(title = str_wrap("NÚMERO ACUMULADO DE VOTOS A FAVOR DE LA OPCIÓN AICM + TOLUCA + SANTA LUCÍA, POR CASILLA", width = 70), 
       x = "\nDía de votación",
       y = "Número\n", 
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema +
  theme(strip.background =  element_rect(fill = "grey80", color = "grey80"),
        strip.text = element_text(color = "white"),
        axis.text.x = element_text(size = 14))

ggsave(filename = "votos_acumulados_por_casilla_aicm_toluca_sta_lucia.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)

  