######################     PoS Tagging Mensajes de Navidad (3)    ########################
#             Este fichero contiene el script para el etiquetado morfológico             #
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

# Carga las librerías.
library(tidyverse)
library(tidytext)

# Leer los Mensajes
ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")
anno <- as.character(1975:2018)
rey <- c(rep("Juan Carlos I", 39), rep("Felipe VI", 5))
mensajes <- tibble(anno = character(),
                   rey = character(),
                   parrafo = numeric(),
                   texto = character())

for (i in 1:length(ficheros)){
  discurso <- read_lines(paste("datos/mensajes", ficheros[i], sep = "/"))
  discurso <- gsub("[-–—]", " – ", discurso)
  discurso <- gsub(" ([\\.,;:])", "\\1", discurso)
  discurso <- gsub("  ", " ", discurso)
  discurso <- gsub("^ ", "", discurso)
  temporal <- tibble(anno = anno[i],
                     rey = rey[i],
                     parrafo = seq_along(discurso),
                     texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}

# Solo para los ordenadores con Windows
mensajes$texto <- iconv(mensajes$texto, from = "Latin1", to = "UTF-8")

library(udpipe)

# Carga en modelo 
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.3-181115.udpipe')

AnnoMensaje <- anno
ReyMensaje <- rey
#

Mensajes_Analizado <- tibble(parrafo_id = integer(),
                                 enunciado_id = integer(),
                                 enunciado = character(),
                                 token_id = character(),
                                 token = character(),
                                 lema = character(),
                                 upos = character(),
                                 xpos = character(),
                                 rasgos = character(),
                                 anno = character(),
                                 rey = character())

for(i in 1:length(AnnoMensaje)){
  temporal <- mensajes %>%
    filter(anno == AnnoMensaje[i]) %>%
    select(texto)
  analisis <- as_tibble(udpipe_annotate(modelo_ancora,
                                        temporal$texto))
  analisis <- analisis %>%
    add_column(anno = AnnoMensaje[i],
             rey = ReyMensaje[i]) %>%
    select(-(paragraph_id),
           -(deps),
           -(misc),
           -(head_token_id),
           -(dep_rel)) %>%
    rename(parrafo_id = doc_id,
           enunciado_id = sentence_id,
           enunciado = sentence,
           lema = lemma,
           rasgos = feats) %>%
    mutate(parrafo_id = as.numeric(str_extract(parrafo_id, "\\d+")))
  Mensajes_Analizado <- bind_rows(Mensajes_Analizado, analisis)
  rm(temporal, analisis)
}

colnames(Mensajes_Analizado)
Mensajes_Analizado <- Mensajes_Analizado[c(10:11,1:9)]

Mensajes_Analizado %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

clases <- Mensajes_Analizado %>%
  group_by(rey) %>%
  drop_na(upos) %>%
  count(upos)

ggplot(clases, aes(upos, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~rey)

clases <- Mensajes_Analizado %>%
  group_by(rey) %>%
  drop_na(upos) %>%
  count(upos) %>%
  mutate(frecuencia = n/sum(n)*100)

ggplot(clases, aes(upos, frecuencia)) +
  geom_col(fill = "red") +
  coord_flip() +
  facet_wrap(~rey, scales = "free_y")


palabras <- Mensajes_Analizado %>%
  group_by(rey) %>%
  filter(upos == "CCONJ") %>% # Si aquí cambias NOUN por otra upos ya tienes el resultado
  count(lema, sort = T) %>%
  mutate(frecuencia = n/sum(n)*100) %>%
  top_n(20) %>%
  ungroup()

ggplot(palabras, aes(lema, frecuencia))+
  geom_col(fill = "maroon") +
  facet_wrap(~ rey, scales = "free_y") +
  coord_flip()

