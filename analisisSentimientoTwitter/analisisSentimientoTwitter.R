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
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.background = element_rect(fill = "#EBEBEB", colour = NA))



#### Importando los datos ####

# Tweets de los candidatos a la presidencia
#  El argumento fileEncoding = "latin1" es importante para mostrar correctamente las vocales con tildes, la ñ y otro caracteres especiales.
tweets <- read.csv("data/tweets_candidatos.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% tbl_df()

# Traducción automática, de inglés a español, de la versión del léxico presente en el conjunto de datos "sentiments" de tidytext, con algunas correcciones manuales
afinn <- read.csv("data/lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% tbl_df()
