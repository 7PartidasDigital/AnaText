# ================================================================
# preparar_toponimos.R
#
# Limpieza y normalización del dataset de topónimos municipales.
#
# Este script:
#   1. Lee el archivo original `toponimos.tsv`
#   2. Elimina la columna `idioma`
#   3. Añade la columna `comunidad` a partir de `provincia`
#      (resolviendo variantes bilingües como "Araba/Álava")
#   4. Genera un nuevo archivo limpio: `toponimos_limpio.tsv`
#
# El archivo resultante está pensado para ser usado directamente
# por la aplicación Shiny sin necesidad de transformaciones internas.
#
# Uso:
#   source("preparar_toponimos.R")
# ================================================================

library(stringi)
library(dplyr)

# -------------------------
# Archivo de entrada y salida
# -------------------------
entrada  <- "toponimos.tsv"
salida   <- "toponimos_limpio.tsv"

# -------------------------
# Función provincia → comunidad
# -------------------------

provincia_a_comunidad <- function(prov) {
  
  # quedarse con el nombre tras la barra si existe
  prov <- sub(".*/", "", prov)
  
  # normalizar
  p <- stringi::stri_trans_general(tolower(trimws(as.character(prov))), "Latin-ASCII")
  
  dict <- c(
    # Andalucía
    "almeria"="Andalucía","cadiz"="Andalucía","cordoba"="Andalucía","granada"="Andalucía",
    "huelva"="Andalucía","jaen"="Andalucía","malaga"="Andalucía","sevilla"="Andalucía",
    
    # Aragón
    "huesca"="Aragón","teruel"="Aragón","zaragoza"="Aragón",
    
    # Asturias
    "asturias"="Asturias","principado de asturias"="Asturias",
    
    # Baleares
    "illes balears"="Illes Balears","islas baleares"="Illes Balears","baleares"="Illes Balears",
    
    # Canarias
    "las palmas"="Canarias","santa cruz de tenerife"="Canarias",
    
    # Cantabria
    "cantabria"="Cantabria",
    
    # Castilla-La Mancha
    "albacete"="Castilla-La Mancha","ciudad real"="Castilla-La Mancha","cuenca"="Castilla-La Mancha",
    "guadalajara"="Castilla-La Mancha","toledo"="Castilla-La Mancha",
    
    # Castilla y León
    "avila"="Castilla y León","burgos"="Castilla y León","leon"="Castilla y León","palencia"="Castilla y León",
    "salamanca"="Castilla y León","segovia"="Castilla y León","soria"="Castilla y León",
    "valladolid"="Castilla y León","zamora"="Castilla y León",
    
    # Cataluña
    "barcelona"="Cataluña","girona"="Cataluña","gerona"="Cataluña",
    "lleida"="Cataluña","lerida"="Cataluña","tarragona"="Cataluña",
    
    # Comunitat Valenciana
    "alicante"="Comunitat Valenciana",
    "castellon"="Comunitat Valenciana",
    "valencia"="Comunitat Valenciana",
    
    # Extremadura
    "badajoz"="Extremadura","caceres"="Extremadura",
    
    # Galicia
    "a coruna"="Galicia","la coruna"="Galicia","coruna"="Galicia",
    "lugo"="Galicia","ourense"="Galicia","orense"="Galicia","pontevedra"="Galicia",
    
    # Madrid
    "madrid"="Comunidad de Madrid",
    
    # Murcia
    "murcia"="Región de Murcia",
    
    # Navarra
    "navarra"="Navarra",
    
    # País Vasco
    "alava"="País Vasco","gipuzkoa"="País Vasco","gipuzcoa"="País Vasco","guipuzcoa"="País Vasco","vizcaya"="País Vasco","bizkaia"="País Vasco",
    
    # La Rioja
    "la rioja"="La Rioja","rioja"="La Rioja",
    
    # Ceuta y Melilla
    "ceuta"="Ceuta","melilla"="Melilla"
  )
  
  unname(dict[p])
}


# -------------------------
# Leer
# -------------------------
df <- read.delim(
  entrada,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# -------------------------
# Transformar
# -------------------------
df2 <- df %>%
  mutate(comunidad = provincia_a_comunidad(provincia)) %>%
  select(-idioma)

# Aviso si hay provincias no reconocidas
no_rec <- unique(df2$provincia[is.na(df2$comunidad)])
if(length(no_rec) > 0){
  cat("\nProvincias sin comunidad asignada:\n")
  print(no_rec)
}

# -------------------------
# Guardar
# -------------------------
write.table(
  df2,
  salida,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nArchivo generado:", salida, "\n")
