##################     Primeros pasos en el Análisis de textos (1)     ###################
#                                                                                        #
#                  Análisis Automático de Textos y Estilomtería con R                    #
#             Laboratorio de Innovación en Humanidades Digitales (LinhdUNED)             #
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

discurso <- readLines("datos/mensajes/1975.txt")

install.packages("tidyverse", "tidytext")  # Solo la primera vez que uses este script
library(tidyverse)
library(tidytext) 
mensaje <- data_frame(parrafo = seq_along(discurso), texto = discurso)
mensaje_palabras <- mensaje %>%
  unnest_tokens(palabra, texto)
mensaje %>%
  unnest_tokens(oraciones, texto, token = "sentences", to_lower = FALSE)
mensaje_palabras %>%
  count(palabra, sort = T)
mensaje_frecuencias <- mensaje_palabras %>% 
  count(palabra, sort = T) %>% 
  mutate(relativa = n / sum(n))
mensaje_enunciados <- mensaje %>%
  unnest_tokens(oraciones,
                texto, token = "sentences",
                to_lower = FALSE)
mensaje_enunciados <- mensaje %>%
  unnest_tokens(oracion,
                texto,
                token = "sentences") %>%
  mutate(NumPal = str_count(oracion, "\\w+"))

# Dibuja el gráfico mínimo
ggplot(mensaje_enunciados, aes(1:nrow(mensaje_enunciados), NumPal)) + 
  geom_bar(stat = 'identity')

# Lo diibuja cambiando la etiqueta del eje horizntal
ggplot(mensaje_enunciados, aes(1:nrow(mensaje_enunciados), NumPal)) + 
  geom_bar(stat = 'identity') +
  labs(x = "Número de oración")

# Añade al título y subtítulo del gráfico
ggplot(mensaje_enunciados, aes(1:nrow(mensaje_enunciados), NumPal)) + 
  geom_bar(stat = 'identity') +
  labs(x = "Número de oración") +
  ggtitle("Número de palabras por oración", subtitle = "Mensaje de Navidad de 1975")

# Añade la línea de la media
ggplot(mensaje_enunciados, aes(1:nrow(mensaje_enunciados), NumPal)) + 
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = mean(mensaje_enunciados$NumPal),
             linetype = "dashed",
             colour = "red",
             size = 0.4) +
  labs(x = "Número de oración") +
  ggtitle("Número de palabras por oración", subtitle = "Mensaje de Navidad de 1975")

# Añade la línea de la mediana
ggplot(mensaje_enunciados, aes(1:nrow(mensaje_enunciados), NumPal)) + 
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = mean(mensaje_enunciados$NumPal),
             linetype = "dashed",
             colour = "red",
             size = 0.4) +
  geom_hline(yintercept = median(mensaje_enunciados$NumPal),
             linetype = "longdash",
             colour = "blue",
             size = 0.4) +
  labs(x = "Número de oración") +
  ggtitle("Número de palabras por oración", subtitle = "Mensaje de Navidad de 1975")
