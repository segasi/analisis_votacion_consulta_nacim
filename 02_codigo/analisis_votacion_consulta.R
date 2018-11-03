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
         opcion_ganadora = ifelse(acumulado_texcoco >= acumulado_sta_lucia, "Opción de continuar la construcción del NAICM en Texcoco", "Opción AICM + Toluca + Santa Lucía")) %>% 
  ungroup()

## Votación acumulada por estado y día ----
voto_acumulado_por_edo <- 
  bd %>% 
  group_by(estado, dia) %>% 
  summarise(suma_diaria_sta_lucia = sum(Opcion1_Actual_mas_Toluca_y_StLucia, na.rm = T),
            suma_diaria_texcoco = sum(Opcion2_continuar_construccion_en_texcoco, na.rm = T),
            suma_diaria_nulos = sum(nulos, na.rm = T),
            suma_diaria_total = sum(total, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(estado) %>% 
  mutate(acumulado_sta_lucia = cumsum(suma_diaria_sta_lucia),
         acumulado_texcoco = cumsum(suma_diaria_texcoco),
         acumulado_nulos =cumsum(suma_diaria_nulos),
         acumulado_total = cumsum(suma_diaria_total),
         opcion_ganadora = ifelse(acumulado_texcoco >= acumulado_sta_lucia, "Opción de continuar la construcción del NAICM en Texcoco", "Opción AICM + Toluca + Santa Lucía")) %>% 
  ungroup() 

### Gráficas ----
## Gráfica: Número de casillas en las que ganó una u otra opción ----
voto_por_casilla %>% 
  ggplot(aes(str_wrap(opcion_ganadora, width = 40), fill = opcion_ganadora)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = comma(..count..), y = ..count..), vjust = -1.1, fontface = "bold", size = 8) +
  scale_y_continuous(limits = c(0, 1050)) +
  scale_fill_manual(values = c("salmon", "steelblue")) +
  labs(title = str_wrap("NÚMERO DE CASILLAS EN LAS QUE CADA OPCIÓN DE LA CONSULTA OBTUVO EL MAYOR NÚMERO DE VOTOS", width = 80),
       x = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuentes: Mexico Decide") +
  tema +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none")

ggsave(filename = "casillas_gano_cada_opcion.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


## Gráfica: Número de municipios en los que ganó una u otra opción ----
voto_por_mpo %>% 
  filter(suma_total != 0) %>% # Eliminar municipios en los que no hay votos
  ggplot(aes(str_wrap(opcion_ganadora, width = 40), fill = opcion_ganadora)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = comma(..count..), y = ..count..), vjust = -1.1, fontface = "bold", size = 8) +
  scale_y_continuous(limits = c(0, 514)) +
  scale_fill_manual(values = c("salmon", "steelblue")) +
  labs(title = str_wrap("NÚMERO DE MUNICIPIOS EN LOS QUE CADA OPCIÓN DE LA CONSULTA OBTUVO LA MAYORÍA DE LOS VOTOS", width = 80),
       subtitle = str_wrap("A pesar de que el comité organizador de la consulta anunció que instalarían casillas en 538 municipios, los datos publicados solo incluyen información de 524 municipios. En tres de ellos el número de votos es cero para las dos opciones.", width = 150),
       x = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuentes: Mexico Decide") +
  tema +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none")

ggsave(filename = "mpos_gano_cada_opcion.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)



## Gráfica: % de votos a favor de cada opción, por estado ----
voto_por_edo %>% 
  select(estado, por_texcoco, por_sta_lucia) %>% 
  mutate(ranking_sta_lucia = rank(por_sta_lucia, ties.method = "first")) %>% 
  gather(key = opcion,
         value = porcentaje,
         -estado, -ranking_sta_lucia) %>% 
  ggplot() +
  geom_col(aes(fct_reorder(estado, ranking_sta_lucia), porcentaje, fill = opcion)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10), limits = c(0, 102)) +
  scale_fill_manual(values = c("salmon", "steelblue"), labels = c("AICM + Toluca + Santa Lucía    ", "Continuar NAICM en Texcoco")) +
  coord_flip() +
  labs(title = "PORCENTAJE DE VOTOS A FAVOR DE CADA OPCIÓN, POR ESTADO",
       subtitle = "Las barras no suman 100% porque no incluyo el porcentaje de votos nulos",
       x = NULL, 
       y = "\nPorcentaje    ",
       fill = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema +
  theme(strip.background =  element_rect(fill = "grey80", color = "grey80"),
        strip.text = element_text(color = "white"),
        axis.text.x = element_text(size = 14),
        legend.position = c(0.22, -0.08),
        legend.direction = "horizontal")

ggsave(filename = "por_votos_por_estado.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)
  

## Gráfica: Votos acumulados diariamente a favor de cada opción de la consulta, por estado ---- 

voto_acumulado_por_edo %>% 
  ggplot() +
  geom_line(aes(dia, acumulado_sta_lucia, group = estado), col = "salmon") +
  geom_line(aes(dia, acumulado_texcoco, group = estado), col = "steelblue") +
  scale_y_continuous(label = comma) +
  facet_wrap(~ estado, ncol = 8) +
  labs(title = str_wrap("NÚMERO ACUMULADO DE VOTOS A FAVOR DE CADA OPCIÓN DE LA CONSULTA, POR ESTADO", width = 70), 
       x = "\nDía de votación",
       y = "Número\n", 
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema + 
  theme(strip.background =  element_rect(fill = "grey80", color = "grey80"),
        strip.text = element_text(color = "white"),
        axis.text.x = element_text(size = 12))

ggsave(filename = "votos_acumulados_por_cada_opcion_por_estado.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


## Gráfica: Votos acumulados diariamente a favor de AICM, Toluca y Santa Lucía ----
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


## Gráfica: Votos acumulados diariamente a favor de NAICM por casilla ----
voto_acumulado_por_casilla %>% 
  ggplot() +
  geom_line(aes(dia, acumulado_texcoco, group = idcasilla),
            color = "steelblue", alpha = 0.5) +
  scale_y_continuous(limits = c(0, 5000), label = comma) +
  facet_wrap(~ estado, ncol = 8) +
  labs(title = str_wrap("NÚMERO ACUMULADO DE VOTOS A FAVOR DE LA OPCIÓN DE CONTINUAR EL NAICM EN TEXCOCO, POR CASILLA", width = 70),
       x = "\nDía de votación",
       y = "Número\n", 
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema +
  theme(strip.background =  element_rect(fill = "grey80", color = "grey80"),
        strip.text = element_text(color = "white"),
        axis.text.x = element_text(size = 14))

ggsave(filename = "votos_acumulados_por_casilla_texcoco.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)  



## Gráfica: Aportación de votos a favor de AICM, Toluca y Santa Lucía, por estado ----
voto_por_edo %>% 
  mutate(por_edo_sta_lucia = round((suma_sta_lucia/sum(suma_sta_lucia))*100, 1)) %>% 
  ggplot() +
  geom_treemap(aes(area = suma_sta_lucia, fill = suma_sta_lucia), col = "white") +
  geom_treemap_text(aes(area = suma_sta_lucia, label = estado), fontface = "bold", color = "white") +
  geom_treemap_text(aes(area = suma_sta_lucia, label = comma(suma_sta_lucia)), color = "white", padding.y = unit(8, "mm"), size = 16) +
  geom_treemap_text(aes(area = suma_sta_lucia, label = paste(por_edo_sta_lucia, "% del total", sep = "")), color = "white", padding.y = unit(14.5, "mm"), size = 15) +
  scale_fill_gradient(low = "grey80", high = "salmon", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = str_wrap("APORTACIÓN DE VOTOS A FAVOR DE LA OPCIÓN AICM + TOLUCA + SANTA LUCÍA, POR ESTADO", width = 90),
       subtitle = str_wrap("El tamaño de cada rectángulo es proporcional al número de votos a favor de la opción AICM + Toluca + Santa Lucía. Mientras más grande y rojo el recuadro, mayor el número de votos aportados por dicha entidad.", width = 150),
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema +
  theme(legend.position = "none")

ggsave(filename = "treemap_sta_lucia_por_edo.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)

### Gráfica: Aportación de votos a favor de NAICM por estado ----
voto_por_edo %>% 
  mutate(por_edo_texcoco = round((suma_texcoco/sum(suma_texcoco))*100, 1)) %>% 
  ggplot() +
  geom_treemap(aes(area = suma_texcoco, fill = suma_texcoco), col = "white") +
  geom_treemap_text(aes(area = suma_texcoco, label = estado), fontface = "bold", color = "white") +
  geom_treemap_text(aes(area = suma_texcoco, label = comma(suma_texcoco)), color = "white", padding.y = unit(8, "mm"), size = 16) +
  geom_treemap_text(aes(area = suma_texcoco, label = paste(por_edo_texcoco, "% del total", sep = "")), color = "white", padding.y = unit(14.5, "mm"), size = 15) +
  scale_fill_gradient(low = "grey80", high = "steelblue", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = str_wrap("APORTACIÓN DE VOTOS A FAVOR DE LA OPCIÓN DE CONTINUAR CONSTRUYENDO EL NAICM EN TEXCOCO, POR ESTADO", width = 80),
       subtitle = str_wrap("El tamaño de cada rectángulo es proporcional al número de votos a favor de la opción de continuar el NAICM en Texcoco. Mientras más grande y azul el recuadro, mayor el número de votos aportados por dicha entidad.", width = 150),
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema +
  theme(legend.position = "none")

ggsave(filename = "treemap_texcoco_por_edo.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


## Gráfica: Aportación de votos a favor de AICM, Toluca y Santa Lucía, por municipio ----
voto_por_mpo %>% 
  mutate(por_edo_sta_lucia = round((suma_sta_lucia/sum(suma_sta_lucia))*100, 1)) %>% 
  ggplot() +
  geom_treemap(aes(area = suma_sta_lucia, fill = suma_sta_lucia), col = "white") +
  geom_treemap_text(aes(area = suma_sta_lucia, label = municipio), fontface = "bold", color = "white") +
  geom_treemap_text(aes(area = suma_sta_lucia, label = comma(suma_sta_lucia)), color = "white", padding.y = unit(8, "mm"), size = 16) +
  geom_treemap_text(aes(area = suma_sta_lucia, label = paste(por_edo_sta_lucia, "% del total", sep = "")), color = "white", padding.y = unit(14.5, "mm"), size = 15) +
  scale_fill_gradient(low = "grey80", high = "salmon", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = str_wrap("APORTACIÓN DE VOTOS A FAVOR DE LA OPCIÓN AICM + TOLUCA + SANTA LUCÍA, POR MUNICIPIO", width = 90),
       subtitle = str_wrap("El tamaño de cada rectángulo es proporcional al número de votos a favor de la opción AICM + Toluca + Santa Lucía. Mientras más grande y rojo el recuadro, mayor el número de votos aportados por dicho municipio.", width = 150),
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema +
  theme(legend.position = "none")

ggsave(filename = "treemap_sta_lucia_por_mpo.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


## Gráfica: Aportación de votos a favor de NAICM por municipio ----
voto_por_mpo %>% 
  mutate(por_edo_texcoco = round((suma_texcoco/sum(suma_texcoco))*100, 1)) %>% 
  ggplot() +
  geom_treemap(aes(area = suma_texcoco, fill = suma_texcoco), col = "white") +
  geom_treemap_text(aes(area = suma_texcoco, label = municipio), fontface = "bold", color = "white") +
  geom_treemap_text(aes(area = suma_texcoco, label = comma(suma_texcoco)), color = "white", padding.y = unit(8, "mm"), size = 16) +
  geom_treemap_text(aes(area = suma_texcoco, label = paste(por_edo_texcoco, "% del total", sep = "")), color = "white", padding.y = unit(14.5, "mm"), size = 15) +
  scale_fill_gradient(low = "grey80", high = "steelblue", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = str_wrap("APORTACIÓN DE VOTOS A FAVOR DE LA OPCIÓN DE CONTINUAR CONSTRUYENDO EL NAICM EN TEXCOCO, POR MUNICIPIO", width = 80),
       subtitle = str_wrap("El tamaño de cada rectángulo es proporcional al número de votos a favor de la opción de continuar el NAICM en Texcoco. Mientras más grande y azul el recuadro, mayor el número de votos aportados por dicho municipio.", width = 150),
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema +
  theme(legend.position = "none")

ggsave(filename = "treemap_texcoco_por_mpo.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)



## Gráfica: % de votos a favor de una y otra opción en los 18 municipios en donde ganó continuar el aeropuerto en Texcoco ----
voto_por_mpo %>% 
  filter(str_detect(opcion_ganadora, "Texcoco"), suma_total != 0) %>% 
  select(municipio, por_sta_lucia, por_texcoco) %>%
  mutate(ranking = rank(-por_texcoco)) %>%
  gather(key = opcion,
         value = porcentaje,
         -municipio, -ranking) %>% 
  ggplot() +
  geom_point(aes(porcentaje, fct_rev(fct_reorder(municipio, ranking)), color = opcion), size = 3) +
  scale_x_continuous(limits = c(18, 82), breaks = seq(20, 80, 10)) +
  scale_color_manual(values = c("salmon", "steelblue"), labels = c("AICM + Toluca + Santa Lucía    ", "Continuar NAICM en Texcoco")) +
  labs(title = str_wrap("PORCENTAJE DE VOTOS A FAVOR DE CADA OPCIÓN EN LOS 18 MUNICIPIOS DONDE GANÓ CONTINUAR EL NAICM EN TEXCOCO", width = 65),
       subtitle = "En Metepec la diferencia (24.2%) corresponde a los votos nulos",
       x = "\nPorcentaje",
       y = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuentes: Mexico Decide",
       col = NULL) +
  tema +
  theme(legend.direction = "vertical",
        legend.position = c(0.85, 0.1),
        legend.box.background = element_rect(fill = "#66666610", 
                                             color = "transparent"))

ggsave(filename = "por_18_mpos_gano_texcoco.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


## Gráfica: % de votos a favor de una y otra opción en los 503 municipios en donde ganó la opción AICM + Toluca + Santa Lucía ----
voto_por_mpo %>% 
  filter(str_detect(opcion_ganadora, "Lucía"), suma_total != 0) %>% 
  select(municipio, por_sta_lucia, por_texcoco) %>%
  mutate(ranking = rank(-por_sta_lucia)) %>%
  gather(key = opcion,
         value = porcentaje,
         -municipio, -ranking) %>% 
  ggplot() +
  geom_point(aes(porcentaje, fct_rev(fct_reorder(municipio, ranking)), color = opcion), size = 3) +
  scale_x_continuous(limits = c(0, 101), breaks = seq(0, 100, 10)) +
  scale_y_discrete(expand = c(0.01, 0.01)) +
  scale_color_manual(values = c("salmon", "steelblue"), labels = c("AICM + Toluca + Santa Lucía    ", "Continuar NAICM en Texcoco")) +
  labs(title = str_wrap("PORCENTAJE DE VOTOS A FAVOR DE CADA OPCIÓN EN LOS 503 MUNICIPIOS DONDE GANÓ LA OPCIÓN AICM + TOLUCA + SANTA LUCÍA", width = 85),
       subtitle = "En diversos municipios la diferencia corresponde a los votos nulos",
       x = "\nPorcentaje   ",
       y = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuentes: Mexico Decide",
       col = NULL) +
  tema +
  theme(legend.direction = "vertical",
        legend.position = c(0.85, 0.05),
        legend.box.background = element_rect(fill = "#66666610", 
                                             color = "transparent"),
        legend.text = element_text(size = 28),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 44),
        plot.subtitle = element_text(size = 34),
        plot.caption = element_text(size = 32),
        axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 32),
        panel.grid.major.x = element_line(color = "grey20"))

ggsave(filename = "por_503_mpos_gano_sta_lucia.png", path = "03_graficas/", width = 30, height = 20, dpi = 100)
x


## Gráfica: distribución de votos por minuto en casillas ----
voto_por_casilla %>% 
  mutate(votos_por_minuto = round(suma_total/(60*10*4), 1)) %>% 
  ggplot() +
  geom_histogram(aes(votos_por_minuto), breaks = seq(0, 3.5, 0.1667), fill = "steelblue", color = "white") +
  labs(title = str_wrap("DISTRIBUCIÓN DE LOS VOTOS POR MINUTO RECIBIDOS EN LAS CASILLAS DE LA CONSULTA DEL NAICM", width = 80),
       subtitle = str_wrap("Para este cálculo consideré que las casillas estuvieron en funcionamiento 2,400 minutos,* equivalentes a 60 minutos x 10 horas x 4 días", width = 150),
       caption = "Nota: La versión original de esta gráfica consideraba, por error, que las casillas funcionaron 8 horas diarias, cuando lo hicieron por 10.",
       x = "\nVotos por minuto\n",
       y = "Número de casillas\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema

ggsave(filename = "histograma_votos_por_minuto_por_casilla.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)  

## Gráfica: casillas que recibieron más de 1.5 votos por minuto ----
voto_por_casilla %>% 
  mutate(votos_por_minuto = round(suma_total/(60*10*4), 1),
         ranking = rank(-votos_por_minuto, ties.method = "first")) %>% 
  filter(ranking < 25) %>%
  ggplot() +
  geom_col(aes(fct_reorder(casilla, votos_por_minuto), votos_por_minuto), fill = "steelblue") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.3)) +
  labs(title = str_wrap("CASILLAS QUE RECIBIERON MÁS DE 1.5 VOTOS POR MINUTO EN LA CONSULTA DEL NAICM", width = 65),
       subtitle = str_wrap("Para este cálculo consideré que las casillas estuvieron en funcionamiento 2,400 minutos,* equivalentes a 60 minutos x 10 horas x 4 días", width = 120),
       caption = "Nota: La versión original de esta gráfica consideraba, por error, que las casillas funcionaron 8 horas diarias, cuando lo hicieron por 10.",
       x = NULL,
       y = "\nVotos por minuto  \n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  coord_flip() +
  tema

ggsave(filename = "casillas_mas_de_2_votos_por_minuto.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)  


