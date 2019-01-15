##################             Cosechando textos de la red             ###################
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

install.packages("rvest") # Solo la primera vez que uses este script
library(rvest)
listado <- read.delim("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/MensajesIndice.txt", header = T, sep = "\t", stringsAsFactors = F)
for (i in 1:nrow(listado)){
  url <- listado$Directio[i]
  discurso <- read_html(url)
  mensaje <- discurso %>%
    html_nodes(".wysiwyg") %>%
    html_nodes(":not(.resaltar)") %>%
    html_text()
  salida <- paste(listado$Anno[i], "txt", sep=".")
  writeLines(mensaje, salida)
}
