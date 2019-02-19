#######################             Nubes básicas              ###########################
#             Este fichero contiene el script para dibujar una nuebe de                  #
#            como las que hay al comienzo del capítulo de Topic Modeling                 #
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

# Establece AnaText como el directorio de trabajo.
# No lo olivides


library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer) # Puede que lo carge el anterior
novela <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/textos/pazos_ulloa.txt", locale = default_locale())
vacias <- as_tibble(read_delim("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt",
                               delim = "\t",
                               col_names = TRUE,
                               quote = "\"",
                               locale = default_locale()))
texto <- tibble(texto = novela)
palabras <- texto %>%
  unnest_tokens(palabra, texto) %>%
  anti_join(vacias) %>%
  count(palabra, sort = T) %>%
  with(wordcloud(palabra, n,
                 max.words = 100, color = brewer.pal(8,"Dark2")))
