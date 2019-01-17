#######################      Bigramas y redes semánticas       ###########################
#             Este fichero contiene el script para el reinicio del entorno               #
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

ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")
anno <- as.character(c(1975:2018))
rey <- c(rep("Juan Carlos I", 39),rep("Felipe VI", 5))
mensajes <- data_frame(anno = character(),
                       rey = character(),
                       parrafo = numeric(),
                       texto = character())
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes",
                              ficheros[i],
                              sep = "/"))
  temporal <- data_frame(anno = anno[i],
                       rey = rey[i],
                       parrafo = seq_along(discurso),
                       texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}
mensajes$rey <- factor(mensajes$rey, levels = c("Juan Carlos I", "Felipe VI"))
mensajes$anno <- factor(mensajes$anno)
vacias <- as_tibble(read.delim("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/vacias.txt",
                               header= TRUE,
                               quote = '"',
                               encoding = "UTF-8",
                               stringsAsFactors = F))
rm(temporal, anno, discurso,ficheros,i,rey)

# Continúa aquí…