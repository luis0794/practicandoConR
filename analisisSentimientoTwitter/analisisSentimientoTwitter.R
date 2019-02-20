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


#### Peparando los datos ####

# Filtrando por año
tweets <- 
  tweets %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",
           remove = FALSE) %>%
  mutate(Fecha = dmy(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2018)

# Convirtiendo tweets en palabras 
tweets_afinn <- 
  tweets %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = screen_name)

# Asignando una puntucacion a cada tweet
tweets <-
  tweets_afinn %>%
  group_by(status_id) %>%
  summarise(Puntuacion_tuit = mean(Puntuacion)) %>%
  left_join(tweets, ., by = "status_id") %>% 
  mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit), 0, Puntuacion_tuit)) %>% 
  rename("Candidato" = screen_name)


##### Explorando los datos, medias por día #####

# Total
tweets_afinn %>% count(Candidato)

# Únicas
tweets_afinn %>% group_by(Candidato) %>% distinct(Palabra) %>% count()

map(c("Positiva", "Negativa"), function(sentimiento) {
  tweets_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(Candidato) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = Candidato) +
    geom_col() +
    facet_wrap("Candidato", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})

