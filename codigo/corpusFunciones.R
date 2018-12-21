# muestra.ficheros presenta los ficheros del directorio de trabajo
# bien organizados como una lista en la pantalla de la consola
muestra.ficheros <- function(nombre.ficheros.v) {
  for (i in 1:length(nombre.ficheros.v)){
    cat(i, nombre.ficheros.v[i], "\n", sep=" ")
  }
}

# hacer.fichero.palabras.l La función toma un vector de nombres de ficheros
#  y una ruta de directorio y construye una lista en la que cada elemento
# de la lista es un vector de palabras de cada uno de los ficheros en el
# vector de nombres de ficheros
hacer.fichero.palabras.l <- function(ficheros.v, dir.entrada){
  # Crea una lista vacía
  vector.palabras.texto.l <- list()
  # Recorre cada uno de los ficheros
  for(i in 1:length(ficheros.v)){
    # Lee los ficheros del directorio (ten en cuenta que es aquí donde necesita
    # saber la ruta)
    texto.v <- scan(paste(dir.entrada, ficheros.v[i], sep = "/"), what = "character", sep = "\n")
    # Los convierte en una sola cadena
    texto.v <- paste(texto.v, collapse=" ")
    # lo pasa a minúsculas y lo divide en palabras
    texto.minusculas.v <- tolower(texto.v)
    palabras.texto.v <- strsplit(texto.minusculas.v, "\\W")
    palabras.texto.v <- unlist(palabras.texto.v)
    # borra los espacios en blanco
    palabras.texto.v <- palabras.texto.v[which(palabras.texto.v!="")]
    # utiliza el índice del vector ficheros.v como el "nombre" en la lista
    vector.palabras.texto.l[[ficheros.v[i]]] <- palabras.texto.v
  }
  return(vector.palabras.texto.l)
}

# hazloKwic Función que utiliza una lista que contiene vectores de texto
# de una serie de ficheros de texto y permite que el usuario genere unas
# concordnacias KWIC interactivamente.
hazloKwic <- function(vector.palabras.texto.nombrado.l){
  # Estas instrucciones pedirán al usuario el fichero en el que
  # buscar, el término que localizar y el número de "contexto" a
  # ambos lados de la palabra clave
  muestra.ficheros(names(vector.palabras.texto.nombrado.l))
  id.fichero <- as.numeric(readline("¿Qué fichero quiere explorar? Introduce el número de un fichero: "))
  contexto <- as.numeric(readline("¿Cuántas palabras quieres ver antes y después? Introduce un número: "))
  clave <- tolower(readline("Introduce una palabra clave: "))
  dianas.v <- which(vector.palabras.texto.nombrado.l[[id.fichero]] == clave)
  if(length(dianas.v)>0){
  for(h in 1:length(dianas.v)){
    inicio <- dianas.v[h]-contexto
    if(inicio < 1){
      inicio <- 1
    }
    fin <- dianas.v[h]+contexto
    cat(vector.palabras.texto.nombrado.l[[id.fichero]][inicio:fin], "\n")
    }
  }
}

#Mejorada
hazloKwicMejor <- function(vector.palabras.texto.nombrado.l){
  # Estas instrucciones pedirán al usuario el fichero en el que
  # buscar, el término que localizar y el número de "contexto" a
  # ambos lados de la palabra clave
  muestra.ficheros(names(vector.palabras.texto.nombrado.l))
  id.fichero <- as.numeric(readline("¿Qué fichero quiere explorar? Introduce el número de un fichero: "))
  contexto <- as.numeric(readline("¿Cuántas palabras quieres ver antes y después? Introduce un número: "))
  clave <- tolower(readline("Introduce una palabra clave: "))
  dianas.v <- which(vector.palabras.texto.nombrado.l[[id.fichero]] == clave)
  if (length(dianas.v) > 0) {
    resultado <- NULL
    for (h in 1:length(dianas.v)) {
      inicio <- dianas.v[h]-contexto
      if(inicio < 1){
        inicio <- 1
      }
      fin <- dianas.v[h]+contexto
      cat(vector.palabras.texto.nombrado.l[[id.fichero]][inicio:fin], "\n")
      mifila <-
        cbind(dianas.v[h],
              paste(vector.palabras.texto.nombrado.l[[id.fichero]][inicio:(dianas.v[h] -1)], collapse = " "), 
              paste(vector.palabras.texto.nombrado.l[[id.fichero]][dianas.v[h]], collapse = " "),
              paste(vector.palabras.texto.nombrado.l[[id.fichero]][(dianas.v[h] + 1):fin], collapse =" "))
      resultado <- rbind(resultado,mifila)
    }
  }
  colnames(resultado) <- c("posición", "izquierda", "clave", "derecha")
  return(resultado)
}

#Optima
hazloKwicOptimo <- function(vector.palabras.texto.nombrado.l){
  # Estas instrucciones pedirán al usuario
  muestra.ficheros(names(vector.palabras.texto.nombrado.l))
  # el fichero en el que buscar
  id.fichero <- as.numeric(readline("¿Qué fichero quiere explorar? Introduce el número de un fichero: "))
  # la cantidad de "contexto" a ambos lados de la palabra clave
  contexto <- as.numeric(readline("¿Cuántas palabras quieres ver antes y después? Introduce un número: "))
  # y la palabra clave
  clave <- tolower(readline("Introduce una palabra clave: "))
  dianas.v <- which(vector.palabras.texto.nombrado.l[[id.fichero]] == clave)
  if (length(dianas.v) > 0) {
    resultado <- NULL
    for (h in 1:length(dianas.v)) {
      inicio <- dianas.v[h]-contexto
      if(inicio < 1){
        inicio <- 1
      }
      fin <- dianas.v[h] + contexto
      cat("\n------------------------", h, "------------------------\n")
      cat(vector.palabras.texto.nombrado.l[[id.fichero]][inicio:(dianas.v[h]-1)], sep=" ")
      cat(" [", vector.palabras.texto.nombrado.l[[id.fichero]][dianas.v[h]], "] ", sep="")
      cat(vector.palabras.texto.nombrado.l[[id.fichero]][(dianas.v[h]+1):fin], sep=" ")
      mifila <- cbind(dianas.v[h],
              paste(vector.palabras.texto.nombrado.l[[id.fichero]][inicio:(dianas.v[h] -1)], collapse = " "), 
              paste(vector.palabras.texto.nombrado.l[[id.fichero]][dianas.v[h]], collapse = " "),
              paste(vector.palabras.texto.nombrado.l[[id.fichero]][(dianas.v[h] + 1):fin], collapse =" "))
      resultado <- rbind(resultado,mifila)
    }
  colnames(resultado) <- c("posición", "izquierda", "clave", "derecha")
  grabar <- as.numeric((readline("Quieres guardar el resultado en un fichero: 1 = Sí o 2 = No \n")))
  if (grabar==1){
    write.csv(resultado,
              paste("resultados/", clave,"_En_", contexto,
                    names(vector.palabras.texto.nombrado.l)[id.fichero], ".csv", sep=""))
  }
}  else {
    cat ("LA PALABRA CLAVE SOLICITADA NO EXISTE\n")
  }
}

# Función para crear una tabla de frecuencias relativas de un fichero TEI
getTEIWordTableList <- function(documento){
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

# Función para crear una tabla de frecuencias relativas
# de un fichero TEI por trozos
getTEIWordSegmentTableList <- function(documento, chunck.size=10){
  parrafos <- getNodeSet(documento, "/tei:TEI/tei:text/tei:body//tei:p",
                         c(tei="http://www.tei-c.org/ns/1.0"))
  palabras <- paste(sapply(parrafos,xmlValue), collapse=" ")
  palabras.minusculas <- tolower(palabras)
  palabras.l <- strsplit(palabras.minusculas, "\\W")
  palabra.v <- unlist(palabras.l)
  long.maxima <- length(palabra.v)/chunck.size
  x <- seq_along(palabra.v)
  trozos.l <- split(palabra.v, ceiling(x/long.maxima))
  trozos.l <- lapply(trozos.l, quitaEspacios)
  absoluta.trozos.l <- lapply(trozos.l, table)
  relativas.trozos.l <- lapply(absoluta.trozos.l, prop.table)
  return(relativas.trozos.l)
}

# Quita espacios en blanco
quitaEspacios <- function(x){
  x[which(x!="")]
}

# Función mi.mapply para unir los trozos en una data.frame
mi.mapply <- function(x){
  mi.lista <- mapply(data.frame, ID=seq_along(x),
  x, SIMPLIFY=FALSE,
  MoreArgs=list(stringsAsFactors=FALSE))
  mi.df <- do.call(rbind, mi.lista)
  return(mi.df)
}

# Función para trocear un XML, bien por porcentaje (defecto) o por tamaño)
CreaSegmentoFlexibleTexto <- function(objeto.documento, tamano.trozo=1000, porcentaje=TRUE){
  parrafos <- getNodeSet(objeto.documento, "/d:TEI/d:text/d:body//d:p", c(d = "http://www.tei-c.org/ns/1.0"))
  palabras <- paste(sapply(parrafos, xmlValue), collapse=" ")
  palabras.minuscula <- tolower(palabras)
  palabras.minuscula <- gsub("[^[:alnum:][:space:]']", " ", palabras.minuscula)
  palabras.l <- strsplit(palabras.minuscula, "\\s+")
  palabra.v <- unlist(palabras.l)
  x <- seq_along(palabra.v)
  if(porcentaje){
    max.longitud <- length(palabra.v)/tamano.trozo
    trozos.l <- split(palabra.v, ceiling(x/max.longitud))
}
  else {
    trozos.l <- split(palabra.v, ceiling(x/tamano.trozo))
    # Maneja los trozos pequeños del final
    if (length(trozos.l[[length(trozos.l)]]) <=
        length(trozos.l[[length(trozos.l)]])/2){
      trozos.l[[length(trozos.l)-1]] <- c(trozos.l[[length(trozos.l)-1]],
                                           trozos.l[[length(trozos.l)]])
      trozos.l[[length(trozos.l)]] <- NULL
    }
  }
    trozos.l <- lapply(trozos.l, paste, collapse=" ")
    trozos.df <- do.call(rbind, trozos.l)
    return(trozos.df)
}


 # Función para dividir los textos etiquetados en vector
divideTexto <- function(x){
  unlist(strsplit(texto, " "))
}

# Función para buscar pares palabra/PoS determinado
seleccionaPalabraEtiquetada <- function(palabras.etiquetadas, etiqueta.objetivo){
  palabras.etiquetadas[grep(etiqueta.objetivo, palabras.etiquetadas)]
}

# Función para borrar etiquetas PoS
borraEtiquetas <- function(palabra.pos){
  sub("/[A-Z]{2,3}", "", palabra.pos)
}


# Función para crear segmentos flexibles de textos etiquetados
SegmentoFlexibleEtiquetado <- function(texto.etiquetado, tamano.trozo=500, percentage=TRUE){
  palabras.etiquetadas <- divideTexto(texto.etiquetado)
  palabras.etiquetadas.validas <- c(seleccionaPalabraEtiquetada(palabras.etiquetadas,"/NN$"))
  palabras <- borraEtiquetas(palabras.etiquetadas.validas)
  palabras.minusculas <- tolower(palabras)
  palabras.v <- gsub("[^[:alnum:][:space:]']", "", palabras.minusculas)
  x <- seq_along(palabras.v)
  if(percentage){
    tamano.max <- length(palabras.v)/tamano.trozo
    trozos.l <- split(palabras.v, ceiling(x/tamano.max))
    } else {
      trozos.l <- split(palabras.v, ceiling(x/tamano.trozo))
      if(length(trozos.l[[length(trozos.l)]]) <=
         length(trozos.l[[length(trozos.l)]])/2){
        trozos.l[[length(trozos.l)-1]] <- c(trozos.l[[length(trozos.l)-1]], trozos.l[[length(trozos.l)]])
        trozos.l[[length(trozos.l)]] <- NULL
        }
      }
  trozos.l <- lapply(trozos.l, paste, collapse=" ")
  trozos.df <- do.call(rbind, trozos.l)
  return(trozos.df)
}