######################     PoS Tagging Mensajes de Navidad (2)    ########################
#             Este fichero contiene el script para el etiquetado morfológico             #
#          dentro del curso Análisis Automático de Textos y Estilomtería con R           #
#          del Laboratorio de Innovación en Humanidades Digitales (LinhdUNED)            #
#                                       edición 2019                                     #

# Proyecto 7PartidasDigital "Edición crítica digital de las Siete Partidas de Alfonso X" #
#       Proyecto financiado por el MINECO, referencia FFI2016-75014-P AEI-FEDER, EU      #
#              Universidad de Valladolid -- IP José Manuel Fradejas Rueda                #
#                            https://7partidas.hypotheses.org/                           #
#                            https://github.com/7PartidasDigital                         #
#                        Este material se distribuye con una licencia                    #
#                                            MIT                                         #
#                                         v. 1.0.0                                       #

# Establece el directorio. No lo olvides.
# Tiene que ser la carpeta AnaText

library(udpipe)
library(tidyverse)
library(tidytext)

# Ancora
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.3-181115.udpipe')

Pazos_Ulloa <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/pazos_ulloa.txt",
                           skip = 3,
                           locale = default_locale())
Pazos_Ulloa <- gsub("[-–—]", " — ", Pazos_Ulloa)
Pazos_Ulloa <- gsub(" ([\\.,;:])", "\\1", Pazos_Ulloa)
Pazos_Ulloa <- gsub("  ", " ", Pazos_Ulloa)
Pazos_Ulloa <- gsub("^ ", "", Pazos_Ulloa)

PU_analisis <- udpipe_annotate(modelo_ancora, Pazos_Ulloa)
PU_analisis <- as_tibble(PU_analisis)

PU_analisis %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

PU_analisis %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

PU_analisis %>%
  filter(upos == "NOUN") %>%
  count(token, sort = T) %>%
  mutate(token = reorder(token, n)) %>%
  top_n(30) %>%
  ggplot(aes(token, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

PU_analisis %>%
  filter(upos == "VERB") %>%
  count(token, sort = T) %>%
  mutate(token = reorder(token, n)) %>%
  top_n(30) %>%
  ggplot(aes(token, n)) +
  geom_col(fill = "maroon") +
  coord_flip()

PU_analisis %>%
  filter(upos == "VERB") %>%
  count(lemma, sort = T) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  top_n(30) %>%
  ggplot(aes(lemma, n)) +
  geom_col(fill = "orange") +
  coord_flip()

PU_analisis %>%
  filter(upos == "AUX" | upos == "VERB") %>%
  count(lemma, sort = T) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  top_n(30) %>%
  ggplot(aes(lemma, n)) +
  geom_col(fill = "orange") +
  coord_flip()

