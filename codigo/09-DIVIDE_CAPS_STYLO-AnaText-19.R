################################# DIVIDE EN SUBFICHEROS ###################################
#                         Este fichero contiene el script para la                         #
#                          sudivisión de ficheros de texto plano                          #
#          dentro del curso Análisis Automático de Textos y Estilomtería con R            #
#          del Laboratorio de Innovación en Humanidades Digitales (LinhdUNED)             #
#                                       edición 2019                                      #

#  Proyecto 7PartidasDigital "Edición crítica digital de las Siete Partidas de Alfonso X" #
#        Proyecto financiado por el MINECO, referencia FFI2016-75014-P AEI-FEDER, EU      #
#                Universidad de Valladolid -- IP José Manuel Fradejas Rueda               #
#                              https://7partidas.hypotheses.org/                          #
#                             https://github.com/7PartidasDigital                         #
#                         Este material se distribuye con una licencia                    #
#                                            MIT                                          #
#                                         v. 0.0.1                                        #

# Establece AnaText como directorio de trabajo

# Atención a los nombres de los ficheros de entrada y salida

texto <- readLines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/celestina.txt", encoding = "UTF-8") # Lee un fichero
dir.create("datos/celestina") # Cambia de acuerdo con la obra / autor
texto <- texto[8:length(texto)] # Solo en el caso de Ayala
posicion_capitulo <- grep("^(Capítulo)|(Prólogo)", texto) # Localiza donde comienza cada CAPÍTULO
posicion_capitulo <- grep("^ACTO", texto) # Localiza donde comienza cada ACTO
texto <- c(texto,"FIN") # añade una marca para marcar fin del texto, pero no la grabará
ultima_posicion <- length(texto) # averigua la nueva última posición
posicion_capitulo <- c(posicion_capitulo, ultima_posicion) # añade la última posición al fichero
# Extrae y graba todos los capítulos salvo el último
for(x in 1:length(posicion_capitulo)){
  if(x != length(posicion_capitulo)){
inicio <- posicion_capitulo[x]+1
fin <- posicion_capitulo[x+1]-1
  capitulo <- texto[inicio:fin]
  # Este bloque es absolutamente opcional. NO USAR CON AYALA.
  capitulo <- gsub("^ARGUMENTO .*$", "", capitulo, perl = T)
  capitulo <- gsub("^[[:upper:]]+\\.– ", "", capitulo, perl = T)
  # Acuérdate de cambiar el directorio de obra y el nombre de obra
  writeLines(capitulo, paste("datos/celestina/",
                             "CELESTINA",
                             "-",
                             formatC(x, width = 2, flag = 0),
                             ".txt",
                             sep = ""))
  }
}
