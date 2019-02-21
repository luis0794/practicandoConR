# setwd("D:/Luis/git/practicandoConR/analisisSentimientoTwitter")

#### Preparación ####

# install.packages(c("tidyverse","tidytext","tm","lubridate","zoo","scales"))

# tidyverse, permite importar multiples paquetes que nos facilitarán el análisis y manipulación de datos
library(tidyverse)
# tidytext, contiene las herramientas para manipular texto
library(tidytext)
# tm, contiene herramientas de mineria de textos
library(tm)
# lubridate, para fechas de manera consistente
library(lubridate)
# zoo y scales, contienen funciones para realizar tareas comunes de análisis y presentación de datos
library(zoo)
library(scales)

# Se define un tema para facilitar la visualización de los resultados.
tema_graf <-
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "serif"),
        panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.background = ggplot2::element_rect(fill = "#EBEBEB", colour = NA))



#### Importando los datos ####

# Tweets de los candidatos a la presidencia
#  El argumento fileEncoding = "latin1" es importante para mostrar correctamente las vocales con tildes, la ñ y otro caracteres especiales.
tweets <- read.csv("data/tweets_candidatos.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% tbl_df()

# Traducción automática, de inglés a español, de la versión del léxico presente en el conjunto de datos "sentiments" de tidytext, con algunas correcciones manuales
afinn <- read.csv("data/lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% tbl_df()


#### Peparando los datos ####

# Filtrando por año
tweets <- 
  tweets %>%
  tidyr::separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  tidyr::separate(Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",
           remove = FALSE) %>%
  dplyr::mutate(Fecha = dmy(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  dplyr::filter(Periodo == 2018)

# Convirtiendo tweets en palabras 
tweets_afinn <- 
  tweets %>%
  tidytext::unnest_tokens(input = "text", output = "Palabra") %>%
  dplyr::inner_join(afinn, ., by = "Palabra") %>%
  dplyr::mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = screen_name)

# Asignando una puntucacion a cada tweet
tweets <-
  tweets_afinn %>%
  dplyr::group_by(status_id) %>%
  dplyr::summarise(Puntuacion_tuit = mean(Puntuacion)) %>%
  dplyr::left_join(tweets, ., by = "status_id") %>% 
  dplyr::mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit), 0, Puntuacion_tuit)) %>% 
  rename("Candidato" = screen_name)


##### Explorando los datos, medias por día #####

# Total
tweets_afinn %>% dplyr::count(Candidato)

# Únicas
tweets_afinn %>% dplyr::group_by(Candidato) %>% dplyr::distinct(Palabra) %>% dplyr::count()

purrr::map(c("Positiva", "Negativa"), function(sentimiento) {
  tweets_afinn %>%
    dplyr::filter(Tipo ==  sentimiento) %>%
    dplyr::group_by(Candidato) %>%
    dplyr::count(Palabra, sort = T) %>%
    dplyr::top_n(n = 10, wt = n) %>%
    ggplot2::ggplot() +
    ggplot2::aes(Palabra, n, fill = Candidato) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap("Candidato", scales = "free") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = sentimiento) +
    tema_graf
})

# Quitando palabra "no", es muy usuada en oraciones pero no necesariamente significa algo negativo
tweets_afinn <-
  tweets_afinn %>%
  dplyr::filter(Palabra != "no") 

# Obteniendo la media de sentimientos por día
tweets_afinn_fecha <-
  tweets_afinn %>%
  dplyr::group_by(status_id) %>%
  dplyr::mutate(Suma = mean(Puntuacion)) %>%
  dpylr::group_by(Candidato, Fecha) %>%
  dppylr::summarise(Media = mean(Puntuacion))

tweets_afinn_fecha %>%
  ggplot2::ggplot() +
  ggplot2::aes(Fecha, Media, color = Candidato) +
  ggplot2::geom_line() +
  tema_graf +
  ggplot2::theme(legend.position = "top")

# Separando tendencias por candidatos
tweets_afinn_fecha %>%
  ggplot2::ggplot() +
  ggplot2::aes(Fecha, Media, color = Candidato) +
  ggplot2::geom_hline(yintercept = 0, alpha = .35) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(Candidato~.) +
  tema_graf +
  ggplot2::theme(legend.position = "none")


#### Usando LOESS (regression local) ####
### Una manera en que podemos extraer tendencias es usar el algoritmo de regresión local LOESS
tweets_afinn_fecha %>%
  ggplot2::ggplot() +
  ggplot2::aes(Fecha, Media, color = Candidato) +
  ggplot2::geom_smooth(method = "loess", fill = NA) +
  tema_graf

tweets_afinn %>%
  ggplot2::ggplot() +
  ggplot2::aes(Fecha, Puntuacion, color = Candidato) +
  ggplot2::geom_smooth(method = "loess", fill = NA) +
  tema_graf

tweets_afinn %>%
  ggplot2::ggplot() +
  ggplot2::aes(Fecha, Puntuacion, color = Candidato) +
  ggplot2::geom_point(color = "#E5E5E5") + 
  ggplot2::geom_smooth(method = "loess", fill = NA) +
  ggplot2::facet_wrap(~Candidato) +
  tema_graf

tweets_afinn_fecha %>%
  ggplot2::ggplot() +
  ggplot2::aes(Fecha, Media, color = Candidato) +
  ggplot2::geom_point(color = "#E5E5E5") + 
  ggplot2::geom_smooth(method = "lm", fill = NA) +
  ggplot2::facet_wrap(~Candidato) +
  tema_graf


#### Usando la media móvil ####
# Se crea medias móviles usando rollmean() de zoo. Con esta función se calcula la media de cada tres días y se la grafica con ggplot.
tweets_afinn_fecha %>%
  dplyr::group_by(Candidato) %>%
  dplyr::mutate(MediaR = zoo::rollmean(Media, k = 3, align = "right", na.pad = TRUE)) %>%
  ggplot2::ggplot() +
  ggplot2::aes(Fecha, MediaR, color = Candidato) +
  ggplot2::geom_hline(yintercept = 0, alpha = .35) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(Candidato~.) +
  tema_graf

