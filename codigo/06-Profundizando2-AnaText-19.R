##################     Profundizando en el Análisis de textos (2)     ####################
#                                                                                        #
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

library(tidyverse)
library(tidytext)
ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")
anno <- gsub("\\.txt", "", ficheros, perl = T)
JCI <- rep("Juan Carlos I", 39)
FVI <- rep("Felipe VI", 5)
rey <- c(JCI, FVI)
mensajes <- tibble(anno = character(),
                       rey = character(),
                       parrafo = numeric(),
                       texto = character())
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes",
                              ficheros[i],
                              sep = "/"))
  temporal <- tibble(anno = anno[i],
                         rey = rey[i],
                         parrafo = seq_along(discurso),
                         texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}

FVI <- mensajes %>%
  filter(rey == "Felipe VI") %>%
  select(texto)

JCI <- mensajes %>%
  filter(rey == "Juan Carlos I") %>%
  select(texto)
rm(temporal, anno, discurso, ficheros, i, rey)
vacias <- as_tibble(read.delim("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/vacias.txt",
                               header= TRUE,
                               quote = '"',
                               encoding = "UTF-8",
                               stringsAsFactors = F))

FVI_palabras <- FVI %>%
  unnest_tokens(palabra, texto) %>%
  anti_join(vacias)

JCI_palabras <- JCI %>%
  unnest_tokens(palabra, texto) %>%
  anti_join(vacias)

FVI_porcentaje <- FVI_palabras %>%
  mutate(palabra = str_extract(palabra, "\\D+")) %>%
  count(palabra) %>%
  transmute(palabra, Felipe = n / sum(n), rey = "Felipe VI")

JCI_porcentaje <- JCI_palabras %>%
  mutate(palabra = str_extract(palabra, "\\D+")) %>%
  count(palabra) %>%
  transmute(palabra, JuanCarlos = n / sum(n), rey = "Juan Carlos I")

reyes_frecuencias <- FVI_porcentaje %>%
  left_join(JCI_porcentaje, by = "palabra")

install.packages("scales") # Solo la primera vez
library(scales)
ggplot(reyes_frecuencias,
       aes(x = Felipe, y = JuanCarlos,
           color = abs(JuanCarlos - Felipe))) +
  geom_abline(color = "red", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = palabra), check_overlap = T, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkgreen",
                       high = "gray") +
  facet_wrap(~rey.x, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Juan Carlos I", x = "Felipe VI")

