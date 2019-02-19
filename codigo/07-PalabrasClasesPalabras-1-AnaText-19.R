######################     PoS Tagging Mensajes de Navidad (3)    ########################
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
# Descarga los modelos y los guarda en el directorio de trabajo
# Una vez descargados, no hace falta volver a descargarlos.
udpipe_download_model(language = "spanish-ancora")
udpipe_download_model(language = "spanish-gsd")
# Se pueden cargar los dos modelos, o tan solo uno
# Ancora
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.3-181115.udpipe')
texto <- "Se me permitirá que antes de referir el gran suceso de que fui testigo, diga algunas palabras sobre mi infancia, explicando por qué extraña manera me llevaron los azares de la vida a presenciar la terrible catástrofe de nuestra marina."
# La línea siguiente es solo ordenadores Windows
texto <- iconv(texto, from = "Latin1", to = "UTF-8")

analisis <- udpipe_annotate(modelo_ancora, texto)
analisis <- as_tibble(analisis)

analisis %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col() #+
  coord_flip()
  
analisis %>%
  filter(upos == "ADJ") %>%
  count(token, sort = T) #%>%
  ggplot(aes(token, n)) +
  geom_col() +
  coord_flip()

  ###
  
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.3-181115.udpipe')

Pazos_Ulloa <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/pazos_ulloa.txt",
                           skip = 3,
                           locale = default_locale())
Pazos_Ulloa <- gsub("—", " — ", Pazos_Ulloa)
Pazos_Ulloa <- gsub(" ([\\.,;:])", "\\1", Pazos_Ulloa)
Pazos_Ulloa <- gsub("  ", " ", Pazos_Ulloa)
Pazos_Ulloa <- gsub("^ ", "", Pazos_Ulloa)

PU_analisis <- udpipe_annotate(modelo_ancora, Pazos_Ulloa)
PU_analisis <- as_tibble(PU_analisis)


PU_analisis %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
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
  filter(upos == "AUX" | upos == "VERB") %>%
  count(lemma, sort = T) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  top_n(30) %>%
  ggplot(aes(lemma, n)) +
  geom_col(fill = "orange") +
  coord_flip()

