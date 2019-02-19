#################################   ANÁLISIS DE GRUPOS  ###################################
#                         Este fichero contiene el script para el                         #
#               Análisis de grupos según el libro de Matthew L. Jockers                   #
#                   Text Analysis with R for Students of Literature                       #
#      adaptado para el curso Análisis Automático de Textos y Estilomtería con R          #
#          del Laboratorio de Innovación en Humanidades Digitales (LinhdUNED)             #
#                                       edición 2019                                      #

# Establece el directorio de trabajo en AnaText

library(XML)
dir.entrada <- "datos/autoresXML" #Ojo al grupo
ficheros.v <- dir(path=dir.entrada, pattern=".*xml")
# Función para crear una tabla de frecuencias relativas de un fichero TEI
TablaPalabrasTEI <- function(documento){
  parrafos <- getNodeSet(documento, "/tei:TEI/tei:text/tei:body//tei:p",
                         c(tei="http://www.tei-c.org/ns/1.0"))
  palabras <- paste(sapply(parrafos,xmlValue), collapse=" ")
  palabras.minusculas <- tolower(palabras)
  palabras.l <- strsplit(palabras.minusculas, "\\W")
  palabra.v <- unlist(palabras.l)
  libro.frecuencias.t <- table(palabra.v[which(palabra.v!="")])
  libro.relativas.t <- 100*(libro.frecuencias.t/sum(libro.frecuencias.t))
  return(libro.relativas.t)
}
libro.frecuencias.l <- list() # Objeto lista para guardar los resultados
for (i in 1:length(ficheros.v)){
  documento <- xmlTreeParse(file.path(dir.entrada,
                                      ficheros.v[i]),
                            useInternalNodes = TRUE)
  datospalabras <- TablaPalabrasTEI(documento)
  libro.frecuencias.l[[ficheros.v[i]]] <- datospalabras
}
frecuencias.l <- mapply(data.frame,
                        ID=seq_along(libro.frecuencias.l),
                        libro.frecuencias.l,
                        SIMPLIFY = FALSE,
                        MoreArgs = list(stringsAsFactors=FALSE))
frecuencias.df <- do.call(rbind,frecuencias.l)
resultado <- xtabs(Freq ~ ID+Var1, data=frecuencias.df)
final.m <- apply(resultado, 2, as.numeric)
pequena.m <- final.m[,apply(final.m,2,mean)>=.25]
# Crear un objeto distancia
dm <- dist(pequena.m)
#Ejecuta el análisis de grupo en el objeto distancia
cluster <- hclust(dm)
#Recupera los nombres de los ficheros para usarlos como etiqueta
cluster$labels <- names(libro.frecuencias.l)
#Dibuja los resultados en un dendrograma
plot(cluster)
