##################     Primeros pasos en el Análisis de textos (2)     ###################
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


library(tidyverse)
library(tidytext)
ficheros <- list.files(path = "datos/mensajes", pattern = "*.txt")
anno <- gsub("\\.txt", "", ficheros, perl = T)
mensajes <- data_frame(anno = character(),
                       parrafo = numeric(),
                       texto = character())
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes", ficheros[i], sep = "/"))
  temporal <- data_frame(anno = anno[i], parrafo = seq_along(discurso), texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}
mensajes_palabras <- mensajes %>%
  unnest_tokens(palabra, texto)
mensajes_frecuencias <- mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))
frecuencias_anno <- mensajes_palabras %>%
  group_by(anno) %>%
  count(palabra, sort =T) %>%
  mutate(relativa = n / sum(n)) %>%
  ungroup()
palabras_1992 <- frecuencias_anno %>%
  filter(anno == "1992")

# Gráficos
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(anno, n), stat = 'identity')

# Lo de menor a mayor
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(x = reorder(anno, n), y = n), stat = 'identity')

# Lo roerdena de mayor a menor
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(x = reorder(anno, -n), y = n), stat = 'identity')

# Lo mejoras
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(anno, n),
           stat = 'identity',
           fill = "lightblue") +
  theme(legend.position='none',
        axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = "Año", y = "Número de palabras") +
  ggtitle("Mensajes de Navidad 1975-2017",
          subtitle = "Número de palabras en cada mensaje")
