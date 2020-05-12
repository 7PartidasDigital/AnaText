
#########################                 CUENTAPALABRAS             ########################
#                   Estilometría y análisis de texto con R para filólogos                   #
#                                 José Manuel Fradejas Rueda                                #
#                                                                                           #
#                                                                                           #
#                                Código utilizado en el libro                               #
#                                                                                           #
#                                 Desarrollado y adaptado por                               #
#                                                                                           #
#                                 José Manuel Fradejas Rueda                                #
#                              Departamento de Lengua Española                              #
#                                 Universidad de Valladolid                                 #
#  Proyecto 7PartidasDigital "Edición crítica digital de las Siete Partidas de Alfonso X"   #
#        Proyecto financiado por el MINECO, referencia FFI2016-75014-P AEI-FEDER, EU        #
#                Universidad de Valladolid -- IP José Manuel Fradejas Rueda                 #
#                              https://7partidas.hypotheses.org/                            #
#                             https://github.com/7PartidasDigital                           #
#                         Este material se distribuye con una licencia                      #
#                                            MIT                                            #
#                                         v. 1.0.0                                          #

#            Este software es un apoyo a lo expuesto en el libro de referencia.             #
#     Ni la editorial ni el proyecto ni la Universidad de Valladolid garantizan que         #
#        sea válido para cualquier otra cosa que no sea como apoyo a lo expuesto            #
#                                 en el libro de referencia.                                #
#           Tampoco garantizan la permanencia de los enlaces ni su pertinencia.             #
#  Tan solo confirman que funcionan en el momento de escribirse el libro y este software.   #

#         Si se detecta cualquier errata o cambio de url, rogamos que escriban a            #

#                         7PartidasDigital[arroba]gmail[punto]                              #

#########################                MAYO 2020               ############################          


# Primer análisis de texto

setwd("PON AQUÍ LA RUTA A TU DIRECTORIO cuentapalabras")
annos <- 1975:2019
annos <- paste(annos, "txt", sep = ".")
ruta <- "https://tinyurl.com/7PartidasMensajes/"
dir.create("datos")
dir.create("datos/mensajes")
for (i in 1:length(annos)){
  entrada <- readLines(paste(ruta,
                             annos[i],
                             sep = ""),
                       encoding = "UTF-8")
  writeLines(entrada,
             paste("datos/mensajes",
                   annos[i],
                   sep = "/"))
}
rm(annos, entrada, i, ruta)


# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Primeros análisis

discurso <- readLines("datos/mensajes/1975.txt")
library(tidyverse)
library(tidytext)
mensaje <- tibble(parrafo = seq_along(discurso),
                   texto = discurso)
mensaje_palabras <- mensaje %>%
  unnest_tokens(palabra, texto)
mensaje %>%
  unnest_tokens(palabra,
                texto,
                to_lower = FALSE)

mensaje %>%
  unnest_tokens(oraciones,
                texto,
                token = "sentences",
                to_lower = FALSE)

mensaje_enunciados <- mensaje %>%
  unnest_tokens(oraciones,
                texto,
                token = "sentences",
                to_lower = FALSE)

mensaje_enunciados <- mensaje %>%
  unnest_tokens(oracion,
                texto,
                token = "sentences",
                to_lower = FALSE) %>%
  mutate(NumPal = str_count(oracion,
                            pattern = "\\w+"))


sum(mensaje_enunciados$NumPal)
sum(mensaje_enunciados[,3])
mensaje_enunciados[1,]
mensaje_enunciados[1,3]

mensaje_palabras %>%
  count(palabra)

mensaje_palabras %>%
  count(palabra,
        sort = TRUE)

mensaje_palabras <- mensaje_palabras %>%
  mutate(NumLetras = nchar(palabra))

mensaje_palabras %>%
  count(NumLetras)

mensaje_palabras %>%
  count(NumLetras,
        sort = T)

mensaje_frecuencias <- mensaje_palabras %>%
  count(palabra,
        sort = T) %>%
  mutate(relativa = n / sum(n))

print(mensaje_enunciados, n = Inf)

hist(mensaje_enunciados$NumPal)
plot(mensaje_enunciados$NumPal)

ggplot(mensaje_enunciados,
       aes(1:nrow(mensaje_enunciados),
           NumPal)) +
  geom_bar(stat = 'identity')

ggplot(mensaje_enunciados,
       aes(1:nrow(mensaje_enunciados),
           NumPal)) +
  geom_point()

ggplot(mensaje_enunciados,
       aes(1:nrow(mensaje_enunciados),
           NumPal)) +
  geom_line()

ggplot(mensaje_enunciados,
       aes(1:nrow(mensaje_enunciados),
           NumPal)) +
  geom_bar(stat = 'identity') +
  labs(x = "Número de oración")

ggplot(mensaje_enunciados,
       aes(1:nrow(mensaje_enunciados),
           NumPal)) +
  geom_bar(stat = 'identity') +
  labs(x = "Número de oración") +
  ggtitle("Número de palabras por oración",
          subtitle = "Mensaje de Navidad de 1975")

ggplot(mensaje_enunciados,
       aes(1:nrow(mensaje_enunciados),
           NumPal)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = mean(mensaje_enunciados$NumPal),
             linetype = "dashed",
             colour = "red",
             size = 0.8) +
  labs(x = "Número de oración") +
  ggtitle("Número de palabras por oración",
          subtitle = "Mensaje de Navidad de 1975")

ggplot(mensaje_enunciados,
       aes(1:nrow(mensaje_enunciados),
           NumPal)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = mean(mensaje_enunciados$NumPal),
             linetype = "dashed",
             colour = "red",
             size = 0.8) +
  geom_hline(yintercept = median(mensaje_enunciados$NumPal),
             linetype = "dotdash",
             colour = "blue",
             size = 0.8) +
  labs(x = "Número de oración") +
  ggtitle("Número de palabras por oración",
          subtitle = "Mensaje de Navidad de 1975")

# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Avance en el análisis textual
  
list.files("datos/mensajes")
library(tidyverse)
library(tidytext)
ficheros <- list.files(path = "datos/mensajes",
           pattern = "*.txt")
anno <- gsub("\\.txt",
             "",
             ficheros,
             perl = TRUE)
mensajes <- NULL
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes",
                            ficheros[i],
                            sep = "/"))
  temporal <- tibble(anno = anno[i],
                   parrafo = seq_along(discurso),
                   texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}

mensajes_palabras <- mensajes %>%
  unnest_tokens(palabra, texto)
mensajes_palabras %>%
  count(palabra, sort = T)
mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))
mensajes_frecuencias <- mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))
frecuencias_anno <- mensajes_palabras %>%
  group_by(anno) %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n)) %>%
  ungroup()
frecuencias_anno
palabras_1992 <- frecuencias_anno %>%
  filter(anno == 1992)
palabras_1992
sum(palabras_1992$n)
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(anno,
           n),
           stat = 'identity')
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(x = reorder(anno, n),
               y = n),
           stat = 'identity')
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(x = reorder(anno, -n),
               y = n),
           stat = 'identity')
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(anno, n),
           stat = 'identity',
           fill = "lightblue") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(x = 'Año',
       y = "Número de palabras") +
  ggtitle("Mensajes de Navidad 1975-2019",
          subtitle = "Número de palabras en cada mensaje")


# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Palabras vacías
# Carga las librerías
library(tidyverse)
library(tidytext)

ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")
anno <- gsub("\\.txt", "", ficheros, perl = T)
mensajes <- tibble(anno = character(),
                   parrafo = numeric(),
                   texto = character())
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes",
                              ficheros[i],
                              sep = "/"))
  temporal <- tibble(anno = anno[i],
                     parrafo = seq_along(discurso),
                     texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}

mensajes_palabras <- mensajes %>%
  unnest_tokens(palabra, texto)

mensajes_frecuencias <- mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))

rm(temporal,discurso,i)

# NUEVO PARA EL CAPÍTULO DESDE AQUÍ

vacias <- get_stopwords("es")

vacias <- vacias %>%
  rename(palabra = word)

mensajes_vaciado <- mensajes_palabras %>%
  anti_join(vacias)

vacias <- read_csv("https://tinyurl.com/7PartidasVacias",
                   locale = default_locale())

write_csv(vacias, "datos/vacias.txt")

mensajes_vaciado <- mensajes_palabras %>%
  anti_join(vacias)

mensajes_vaciado %>%
  count(palabra, sort = T)

mensajes_vaciado %>%
  count(palabra, sort = T) %>%
  filter(n > 65) %>%
  mutate(palabra = reorder(palabra, n)) %>%
  ggplot(aes(x = palabra, y = n, fill = palabra)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Mensajes de Navidad 1975-2019") +
  coord_flip()

# Pardo Bazán
pazos <- tibble(texto = read_lines("https://tinyurl.com/7PartidasPazos",
                                   locale = default_locale(),
                                   skip = 3))

pazos_palabras <- pazos %>%
  unnest_tokens(palabra, texto) %>%
  anti_join(vacias)

pazos_palabras %>%
  count(palabra, sort = T) %>%
  top_n(15) %>%
  mutate(palabra = reorder(palabra, n)) %>%
  ggplot(aes(x = palabra, y = n, fill = palabra)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Los Pazos de Ulloa",
       subtitle = "Emilia Pardo Bazán",
       x = NULL,
       y = "Número de veces que aparecen") +
  coord_flip()

pazos_palabras <- pazos %>%
  unnest_tokens(palabra, texto) %>%
  anti_join(vacias)

vacias_adhoc <- tibble(palabra = c("gabriel",
                                   "julián",
                                   "nucha",
                                   "marqués",
                                   "perucho",
                                   "pedro",
                                   "ulloa",
                                   "don"))

pazos_palabras <- pazos_palabras %>%
  anti_join(vacias_adhoc)

pazos_palabras %>%
  count(palabra, sort = T) %>%
  top_n(15) %>%
  mutate(palabra = reorder(palabra, n)) %>%
  ggplot(aes(x = palabra, y = n, fill = palabra)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Los Pazos de Ulloa",
       subtitle = "Emilia Pardo Bazán",
       x = NULL,
       y = "Número de veces que aparecen") +
  coord_flip()



# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Comparar léxicos

library(tidyverse)
library(tidytext)

ficheros <- list.files(path ="datos/mensajes",
                       pattern = "\\d+")

anno <- gsub("\\.txt", "", ficheros, perl = T)

mensajes <- tibble(anno = character(),
                   parrafo = numeric(),
                   texto = character())

for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes",
                              ficheros[i],
                              sep = "/"))
  temporal <- tibble(anno = anno[i],
                     parrafo = seq_along(discurso),
                     texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}

mensajes_palabras <- mensajes %>%
  unnest_tokens(palabra, texto)

mensajes_frecuencias <- mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))

rm(temporal, discurso, i)

vacias <- read_tsv("https://tinyurl.com/7PartidasVacias")

mensajes_vaciado <- mensajes_palabras %>%
  anti_join(vacias)

# NUEVO PARA EL CAPÍTULO DESDE AQUÍ


mensajes_vaciado <- mensajes_vaciado %>%
  mutate(rey = anno) %>%
  mutate(rey = str_replace(rey, "201[456789]", "Felipe VI")) %>%
  mutate(rey = str_replace(rey, "\\d+", "Juan Carlos I"))

mensajes_vaciado %>%
  group_by(rey) %>%
  count(palabra, sort = T) %>%
  top_n(10)

mensajes_vaciado %>%
  group_by(rey) %>%
  count(palabra, sort = T) %>%
  top_n(10) %>%
  ggplot(aes(reorder(palabra, n), n, fill = rey)) +
  geom_bar(stat = "identity") +
  facet_wrap(~rey, scales = "free_y") +
  labs(x = "",
       y = "Frecuencia absoluta") +
  coord_flip() +
  theme(legend.position="none")

mensajes_vaciado %>%
  group_by(rey) %>%
  count(palabra, sort = T) %>%
  top_n(30) %>%
  ggplot(aes(reorder(palabra, n), n, fill = rey)) +
  geom_bar(stat = "identity") +
  facet_wrap(~rey) +
  labs(x = "",
       y = "Frecuencia absoluta") +
  coord_flip() +
  theme(legend.position="none")

mensajes_vaciado %>%
  select(rey, anno, palabra) %>%
  filter(rey == "Felipe VI") %>%
  group_by(anno) %>%
  count(palabra, sort =T) %>%
  ungroup() %>%
  filter(n > 5) %>%
  ggplot(aes(x = palabra, y = n)) +
  geom_col(fill = "maroon") +
  coord_flip() +
  facet_wrap(~ anno, ncol = 3, scales = "free_y")

mensaje_anno <- mensajes_vaciado %>%
  select(rey, anno, palabra) %>%
  filter(rey != "Felipe VI") %>%
  group_by(anno) %>%
  count(palabra, sort =T) %>%
  ungroup()

mensaje_anno %>%
  filter(n > 8) %>%
  ggplot(aes(x = palabra,
             y = n)) +
  geom_col(fill = "aquamarine") +
  coord_flip() +
  facet_wrap(~ anno,
             ncol = 3,
             scales = "free_y")



# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Comparar frecuencias
library(tidyverse)
library(tidytext)

ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")

anno <- gsub("\\.txt", "", ficheros, perl = T)

JCI <- rep("Juan Carlos I", 39)

FVI <- rep("Felipe VI", 6)

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

vacias <- read_tsv("https://tinyurl.com/7PartidasVacias")

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

library(scales)

ggplot(data = reyes_frecuencias,
       mapping = aes(x = Felipe,
                     y = JuanCarlos,
                     color = abs(JuanCarlos - Felipe))) +
  geom_abline(color = "red", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = palabra), check_overlap = T, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkgreen",
                       high = "gray") +
  facet_wrap(~ rey.x, ncol = 2) + 
  theme(legend.position="none", strip.text.x = element_blank()) +
  labs(y = "Juan Carlos I", x = "Felipe VI")



# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Etiquetado automático

library(udpipe)
library(tidyverse)

udpipe_download_model(language = "spanish-gsd")

udpipe_download_model(language = "spanish-ancora")

modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.4-190531.udpipe')

texto <- "Se me permitirá que antes de referir el gran suceso de que fui testigo, diga algunas palabras sobre mi infancia, explicando por qué extraña manera me llevaron los azares de la vida a presenciar la terrible catástrofe de nuestra marina."

# Solo Windows
texto <- iconv(texto, from = "Latin1", to = "UTF-8")
# Fin solo Windows

analisis <- udpipe_annotate(modelo_ancora, texto)

analisis <- as_tibble(analisis)

analisis %>%
  count(upos, sort = TRUE)

analisis %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col() +
  coord_flip()

analisis %>%
  filter(upos == "NOUN") %>%
  count(token, sort = TRUE)

Pazos_Ulloa <- read_lines("https://tinyurl.com/7PartidasPazos",
                          skip = 3,
                          locale = default_locale())

Pazos_Ulloa <- gsub("[-–—]", " — ", Pazos_Ulloa)

Pazos_Ulloa <- gsub(" ([\\.,;:])", "\\1", Pazos_Ulloa)

Pazos_Ulloa <- gsub(" {2,10}", " ", Pazos_Ulloa)

Pazos_Ulloa <- gsub("^ ", "", Pazos_Ulloa)

PU_analisis <- udpipe_annotate(modelo_ancora, Pazos_Ulloa)

PU_analisis <- as_tibble(PU_analisis)

PU_analisis %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

PU_analisis %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

PU_analisis %>%
  filter(upos == "NOUN") %>%
  count(token, sort = T) %>%
  mutate(token = reorder(token, n)) %>%
  top_n(30) %>%
  ggplot(aes(token, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

PU_analisis %>%
  filter(upos == "VERB") %>%
  count(token, sort = T) %>%
  mutate(token = reorder(token, n)) %>%
  top_n(30) %>%
  ggplot(aes(token, n)) +
  geom_col(fill = "maroon") +
  coord_flip()

PU_analisis %>%
  filter(upos == "VERB") %>%
  count(lemma, sort = T) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  top_n(30) %>%
  ggplot(aes(lemma, n)) +
  geom_col(fill = "orange") +
  coord_flip()

PU_analisis %>%
  filter(upos == "AUX" | upos == "VERB") %>%
  count(lemma, sort = T) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  top_n(30) %>%
  ggplot(aes(lemma, n)) +
  geom_col(fill = "orange") +
  coord_flip()


# ANTES CONTINUAR
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.


## DE NUEVO. los mensajes
library(udpipe)
library(tidyverse)

modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.4-190531.udpipe')

ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")

anno <- as.character(1975:2019)

rey <- c(rep("Juan Carlos I", 39),
         rep("Felipe VI", 6))

mensajes <- tibble(anno = character(),
                   rey = character(),
                   parrafo = numeric(),
                   texto = character())

for (i in 1:length(ficheros)){
  discurso <- read_lines(paste("datos/mensajes",
                               ficheros[i],
                               sep = "/"))
  discurso <- gsub("[---]", " - ", discurso)
  discurso <- gsub(" ([\\.,;:])", "\\1", discurso)
  discurso <- gsub("  ", " ", discurso)
  discurso <- gsub("^ ", "", discurso)
  temporal <- tibble(anno = anno[i],
                     rey = rey[i],
                     parrafo = seq_along(discurso),
                     texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}

# Solo ordenadores Windows
mensajes$texto <- iconv(mensajes$texto, from ="Latin1", to = "UTF-8")
# Fin excepción

AnnoMensaje <- anno

ReyMensaje <- rey

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

Mensajes_Analizado <- Mensajes_Analizado[c(10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9)]

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
  count(upos, sort = T)

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
  filter(upos == "NOUN") %>%
  count(lema, sort = T) %>%
  mutate(frecuencia = n/sum(n)*100) %>%
  top_n(20) %>%
  ungroup()

ggplot(palabras, aes(lema, frecuencia))+
  geom_col(fill = "maroon") +
  facet_wrap(~ rey, scales = "free_y") +
  coord_flip()

palabras <- Mensajes_Analizado %>%
  group_by(rey) %>%
  filter(upos == "VERB" | upos == "AUX") %>%
  count(lema, sort = T) %>%
  mutate(frecuencia = n/sum(n)*100) %>%
  top_n(20) %>%
  ungroup()

ggplot(palabras, aes(lema, frecuencia))+
  geom_col(fill = "darkblue") +
  facet_wrap(~ rey, scales = "free_y") +
  coord_flip()


# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# BIGRAMAS Y DEMÁS
library(tidyverse)
library(tidytext)

ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")

anno <- as.character(c(1975:2019))

rey <- c(rep("Juan Carlos I", 39),rep("Felipe VI", 6))

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

mensajes$rey <- factor(mensajes$rey, levels = c("Juan Carlos I", "Felipe VI"))

mensajes$anno <- factor(mensajes$anno)

vacias <- read_tsv("https://tinyurl.com/7PartidasVacias",
                   locale = default_locale())

rm(temporal, anno, discurso, ficheros, i, rey)

mensajes %>%
  unnest_tokens(palabra, texto)

mensajes %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 2)

mensajes %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 3)

mensajes_bigramas <- mensajes %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 2)

mensajes_bigramas %>%
  count(bigrama, sort = T)

bigramas_separados <- mensajes_bigramas %>%
  separate(bigrama,
           c("palabra1", "palabra2"),
           sep = " ")

bigramas_filtrados <- bigramas_separados %>%
  filter(!palabra1 %in% vacias$palabra,
         !palabra2 %in% vacias$palabra)

bigramas_filtrados <- bigramas_filtrados %>%
  filter(!palabra1 %in% c("año", "feliz", "navidad", "buenas", "noches", "nuevo", "mejores", "deseos"),
         !palabra2 %in% c("año", "feliz", "navidad", "buenas", "noches", "nuevo", "mejores", "deseos"))

bigramas_unidos <- bigramas_filtrados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

bigramas_unidos %>%
  count(rey, bigrama, sort = T) %>%
  group_by(rey) %>%
  top_n(10) %>%
  ggplot() +
  geom_col(aes(y = n , x = reorder(bigrama, n)),
           fill = "maroon") +
  coord_flip() +
  facet_wrap(~ rey, ncol = 2, scales = "free") +
  theme_linedraw() + 
  labs(x = "Bigramas", y = "Frecuencia") + 
  ggtitle("Bigramas más frecuentes de cada rey", subtitle = "1975-2019")

# grafos

library(igraph)
library(ggraph)
library(grid)

recuento_bigramas <- bigramas_filtrados %>%
  count(palabra1, palabra2, sort = T)

grafo_bigramas <- recuento_bigramas %>%
  filter(n > 5) %>%
  graph_from_data_frame()
set.seed(2016)

ggraph(grafo_bigramas, layout = "nicely") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


ggraph(grafo_bigramas, layout = "nicely") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = arrow(type = "closed",
                               length = unit(3, "mm"))) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

library("widyr")

mensajes_rey <- mensajes %>%
  filter(rey =="Juan Carlos I") %>%
  mutate(seccion = row_number()) %>%
  unnest_tokens(palabra, texto) %>%
  filter(!palabra %in% vacias$palabra)

pares_palabras <- mensajes_rey %>%
  pairwise_count(palabra,
                 seccion,
                 sort = T)

pares_palabras %>%
  filter(item1 == "españoles")

palabras_correlacion <- mensajes_rey %>%
  group_by(palabra) %>%
  filter(n() >= 5) %>%
  pairwise_cor(palabra,
               seccion,
               sort = TRUE)

palabras_correlacion %>%
  filter(item1 == "terrorismo")

palabras_correlacion %>%
  filter(item1 %in% c("constitución",
                      "terrorismo",
                      "crisis",
                      "libertades",
                      "país",
                      "trabajo")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(x = "Segundo término",
       y = "correlación") +
  coord_flip()

palabras_correlacion %>%
  filter(correlation > .35) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Topic Modeling

library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)
library(scales)

vacias <- read_tsv("https://tinyurl.com/7PartidasVacias")

ruta <- "https://tinyurl.com/7PartidasLegajo/"

titulos <- c("Chomsky", "Freud", "Maquiavelo", "Voltaire")

ficheros <- c("filosofo1.txt", "filosofo2.txt", "filosofo3.txt", "filosofo4.txt")

ensayos <- tibble(texto = character(),
                  titulo = character(),
                  pagina = numeric())

for (j in 1:length(ficheros)){
  texto.entrada <- read_lines(paste(ruta,
                                    ficheros[j],
                                    sep = ""),
                              locale = default_locale())
  texto.todo <- paste(texto.entrada, collapse = " ")
  por.palabras <- strsplit(texto.todo, " ")
  texto.palabras <- por.palabras[[1]]
  trozos <- split(texto.palabras,
                  ceiling(seq_along(texto.palabras)/375))
  for (i in 1:length(trozos)){
    fragmento <- trozos[i]
    fragmento.unido <- tibble(texto = paste(unlist(fragmento),
                                            collapse = " "),
                              titulo = titulos[j],
                              pagina = i)
    ensayos <- bind_rows(ensayos, fragmento.unido)
  }
}

rm(ficheros, titulos, trozos, fragmento, ruta, fragmento.unido, texto.entrada, texto.palabras, texto.todo, por.palabras, i, j)

por_pagina_palabras <- ensayos %>%   
  unite(titulo_pagina, titulo, pagina) %>%                 
  unnest_tokens(palabra, texto)

palabra_conteo <- por_pagina_palabras %>%   
  anti_join(vacias) %>%   
  count(titulo_pagina, palabra, sort = TRUE) %>%   
  ungroup()

paginas_dtm <- palabra_conteo %>%
  cast_dtm(titulo_pagina, palabra, n)

paginas_lda <- LDA(paginas_dtm, k = 4, control = list(seed = 1234))

paginas_lda_td <- tidy(paginas_lda, matrix = "beta")

terminos_frecuentes <- paginas_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(caption = "Cuentapalabras 2020") +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

por_pagina_palabras <- ensayos %>%
  unite(titulo_pagina, titulo, pagina) %>%
  unnest_tokens(palabra, texto)

palabra_conteo <- por_pagina_palabras %>%
  anti_join(vacias) %>% 
  count(titulo_pagina, palabra, sort = TRUE) %>%
  ungroup()

paginas_dtm <- palabra_conteo %>%
  cast_dtm(titulo_pagina, palabra, n)

paginas_lda <- LDA(paginas_dtm, k = 4, control = list(seed = 1234))

paginas_lda_td <- tidy(paginas_lda, matrix = "beta")

terminos_frecuentes <- paginas_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

paginas_lda_gamma <- tidy(paginas_lda, matrix = "gamma")

paginas_lda_gamma <- paginas_lda_gamma %>%
  separate(document,
           c("titulo", "pagina"),
           sep = "_", convert = TRUE)

ggplot(paginas_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() +
  facet_wrap(~ titulo, nrow = 2)

paginas_clasificaciones <- paginas_lda_gamma %>%
  group_by(titulo, pagina) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

topico_pensador <- paginas_clasificaciones %>%
  count(titulo, topic) %>%
  group_by(titulo) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consenso = titulo, topic)

paginas_clasificaciones %>%
  inner_join(topico_pensador, by = "topic") %>%
  filter(titulo != consenso)

asignaciones <- augment(paginas_lda, data = paginas_dtm)

asignaciones <- asignaciones %>%
  separate(document, c("titulo",
                       "pagina"),
           convert = TRUE) %>%
  inner_join(topico_pensador,
             by = c(".topic" = "topic"))

asignaciones %>%
  count(titulo, consenso, wt = count) %>%
  group_by(titulo) %>%
  mutate(porcentaje = n / sum(n)) %>%
  ggplot(aes(consenso, titulo, fill = porcentaje)) +
  geom_tile() +
  scale_fill_gradient2(high = "blue", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Asignó las palabras a…",
       y = "Las palabras procedían de…",
       fill = "% de asignaciones")

palabras_equivocadas <- asignaciones %>%
  filter(titulo != consenso)

palabras_equivocadas %>%
  count(titulo, consenso, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))


# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.


# Análisis de atribución de autoría

setwd("~/Dropbox/cuentapalabras/datos")

dir.create("corpus")

ficheros <- c("Acevedo1.txt",
              "Acevedo2.txt",
              "Acevedo3.txt",
              "anonimo.txt",
              "Galdos1.txt",
              "Galdos2.txt",
              "Galdos3.txt",
              "Galdos4.txt",
              "Galdos5.txt",
              "Galdos6.txt",
              "Galdos7.txt",
              "Pereda1.txt",
              "Pereda2.txt",
              "Pereda3.txt",
              "Pereda4.txt",
              "Valera1.txt",
              "Valera2.txt",
              "ValleInclan1.txt",
              "ValleInclan2.txt",
              "ValleInclan3.txt")

ruta <- "https://tinyurl.com/7PartidasAutores/"

for (i in 1:length(ficheros)){
  lee <- readLines(paste(ruta,
                         ficheros[i],
                         sep= ""),
                   encoding = "UTF-8")
  writeLines(lee,
             paste("corpus/",
                   ficheros[i],
                   sep = ""))
}

texto <- readLines("https://tinyurl.com/7PartidasCelestina",
                   encoding = "UTF-8")

dir.create("datos/celestina")

posicion_capitulo <- grep("^ACTO", texto)

texto <- c(texto,"FIN")

ultima_posicion <- length(texto)

posicion_capitulo <- c(posicion_capitulo, ultima_posicion)

for(x in 1:length(posicion_capitulo)){
  if(x != length(posicion_capitulo)){
    inicio <- posicion_capitulo[x]+1
    fin <- posicion_capitulo[x+1]-1
    capitulo <- texto[inicio:fin]
    capitulo <- gsub("^ARGUMENTO .*$",
                     "", capitulo,
                     perl = T)
    capitulo <- gsub("^[[:upper:]]+\\.– ",
                     "", capitulo,
                     perl = T)
    write(capitulo, paste("datos/celestina/",
                          "CELESTINA",
                          "-",
                          formatC(x, width = 2, flag = 0),
                          ".txt",
                          sep = ""))
  }
}



# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Análisis de sentimientos
library(tidyverse)
library(tidytext)
library(syuzhet)

sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())

source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")

trafalgar <- read_lines("https://tinyurl.com/7PartidasEpisodios/01_EN-01-01-Trafalgar.txt",
                        locale = default_locale())

carlos4 <- read_lines("https://tinyurl.com/7PartidasEpisodios/02_EN-01-02-La_Corte_de_Carlos_IV.txt",
                      locale = default_locale())

marzo_mayo <- read_lines("https://tinyurl.com/7PartidasEpisodios/03_EN-01-03-El_19_de_Marzo_y_el_2_de_Mayo.txt",
                         locale = default_locale())

bailen <- read_lines("https://tinyurl.com/7PartidasEpisodios/04_EN-01-04-Bailen.txt",
                     locale = default_locale())

napoleon <- read_lines("https://tinyurl.com/7PartidasEpisodios/05_EN-01-05-Napoleon_en_Chamartin.txt",
                       locale = default_locale())

zaragoza <- read_lines("https://tinyurl.com/7PartidasEpisodios/06_EN-01-06-Zaragoza.txt",
                       locale = default_locale())

gerona <- read_lines("https://tinyurl.com/7PartidasEpisodios/07_EN-01-07-Gerona.txt",
                     locale = default_locale())

cadiz <- read_lines("https://tinyurl.com/7PartidasEpisodios/08_EN-01-08-Cadiz.txt",
                    locale = default_locale())

empecinado <- read_lines("https://tinyurl.com/7PartidasEpisodios/09_EN-01-09-Juan_Martin_El_Empecinado.txt",
                         locale = default_locale())

arapiles <- read_lines("https://tinyurl.com/7PartidasEpisodios/10_EN-01-10-La_Batalla_de_los_Arapiles.txt",
                       locale = default_locale())

titulos <- c("Trafalgar",
             "La Corte de Carlos IV",
             "El 19 de Marzo y el 2 de Mayo",
             "Bailen",
             "Napoleon en Chamartin",
             "Zaragoza",
             "Gerona",
             "Cadiz",
             "Juan Martin El Empecinado",
             "La Batalla de los Arapiles")

libros <- list(trafalgar,
               carlos4,
               marzo_mayo,
               bailen,
               napoleon,
               zaragoza,
               gerona,
               cadiz,
               empecinado,
               arapiles)

serie <- NULL

for(i in seq_along(titulos)) {
  limpio <- tibble(capitulo = seq_along(libros[[i]]),
                   texto = libros[[i]]) %>%
    unnest_tokens(palabra, texto) %>%
    mutate(libro = titulos[i]) %>%
    select(libro, everything())
  serie <- bind_rows(serie, limpio)
}

serie$libro <- factor(serie$libro, levels = rev(titulos))

serie %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 500 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(libro, indice = indice , sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo, libro = factor(libro, levels = titulos)) %>%
  ggplot(aes(indice, sentimiento, fill = libro)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ libro, ncol = 2, scales = "free_x")

recuenta_palabras_bing <- serie %>%
  inner_join(get_sentiments("bing")) %>%
  count(palabra, sentimiento, sort = TRUE)

recuenta_palabras_bing %>%
  group_by(sentimiento) %>%
  top_n(25) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(y = "Contribución al sentimiento", x = NULL) +
  coord_flip()



# ANTES DE CONTINUAR
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.


library(syuzhet)
library(tidyverse)
library(tidytext)

sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())

source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")

texto_entrada <- read_lines("https://tinyurl.com/7PartidasTextos/cuatro_jinetes_apocalipsis.txt",
                            locale = default_locale())

texto_analizar <- tibble(texto = texto_entrada)

texto_analizar <- texto_analizar %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo*-1)

puntuacion <- texto_analizar %>%
  mutate(sentimiento = positivo+negativo) %>%
  select(pagina, sentimiento)

ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Los cuatro jinetes del Apocalipsis")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

texto_trans <- get_dct_transform(puntuacion$sentimiento,
                                 low_pass_size = 10,
                                 #x_reverse_len = nrow(puntuacion),
                                 scale_range = TRUE)

texto_trans <- tibble(pagina = seq_along(texto_trans),
                      ft = texto_trans)

ggplot(texto_trans, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Los cuatro jinetes del Apocalipsis"))))

plot(texto_trans,
     type = "l",
     yaxt = 'n',
     ylab = "",
     xlab = "Tiempo narrativo",
     main = "La forma de la historia\nLos cuatro jinetes del Apocalipsis")
abline(h = 0.0, col = "red")


# ANTES DE PASAR AL SIGUIENTE CAPÍTULO
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# Palabras favoritas

library(tidyverse)
library(tidytext)

options(scipen = 999) # que no emplee notación científica

MdT <- read_tsv("https://tinyurl.com/7PartidasMdT")

MdT <- MdT %>%
  mutate(titulo = as.factor(titulo))


MdT %>% count(personaje, sort = TRUE)


MdT <- MdT %>% 
  mutate(personaje2 = ifelse(
    personaje %in% c("PACINO",
                     "JULIÁN",
                     "AMELIA",
                     "ALONSO",
                     "SALVADOR",
                     "ERNESTO"),
    personaje, "RESTO"))

MdT %>% count(personaje2, sort = T)


palabras_personaje <- MdT %>% 
  unnest_tokens(palabra, texto) %>% 
  mutate(palabra = str_extract(palabra, "[[:alpha:]]+")) %>%
  count(personaje2, palabra)

palabras_personaje

total_palabras <- palabras_personaje %>% 
  group_by(personaje2) %>% 
  summarize(total = sum(n)) %>%
  ungroup()


palabras_personaje %>% 
  group_by(personaje2) %>% 
  summarize(total = sum(n)) %>%
  mutate(porcentaje = total*100/(sum(total))) %>%
  arrange(desc(porcentaje)) %>%
  mutate(personaje2 = factor(personaje2,
                             levels=c("ERNESTO",
                                      "JULIÁN",
                                      "AMELIA",
                                      "ALONSO",
                                      "PACINO",
                                      "SALVADOR",
                                      "RESTO"))) %>%
  ggplot(aes(" ", porcentaje)) +
  geom_bar(stat="identity", aes(fill=personaje2)) +
  theme_classic() +
  ylab("Proporción de palabras pronunciadas (%)") + 
  xlab(" ") +
  scale_fill_manual(values = c("#000000", "#E69F00",
                               "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2",
                               "#D55E00", "#CC79A7")) +
  theme(legend.position=1,plot.title = element_text(size=12), 
        axis.title.y=element_text(margin = margin(0,10,0,0)),
        axis.title = element_text(size = 12)) +
  geom_text(aes(x = " ", y = cumsum(porcentaje)-porcentaje*0.5, 
                label=paste(personaje2,": ",round(porcentaje, digits = 1),"%", sep="")), 
            color="white", fontface="bold", size=4)

palabras_personaje <- left_join(palabras_personaje, total_palabras) %>% 
  bind_tf_idf(palabra, personaje2, n)

personaje_dibuja <- palabras_personaje %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(palabra = factor(palabra, levels = rev(unique(palabra))))

personaje_dibuja %>% 
  filter(personaje2 != "RESTO") %>% 
  group_by(personaje2) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(palabra, tf_idf, fill = personaje2)) +
  geom_col() +
  facet_wrap(~personaje2, scales = "free") +
  coord_flip() +
  labs(x = "tf-idf",
       title = "Palabras más características de cada personaje",
       subtitle = "Medidas por medio de tf-idf") +
  theme(legend.position = "none",
        axis.title.y = element_blank())

palabras_episodio <- MdT %>% 
  unnest_tokens(palabra, texto) %>% 
  count(episodio, titulo, palabra, sort = TRUE)

total_palabras <- palabras_episodio %>% 
  group_by(episodio, titulo) %>% 
  summarize(total = sum(n))

palabras_episodio <- left_join(palabras_episodio, total_palabras) %>% 
  bind_tf_idf(palabra, episodio, n)

episodio_peculiar <- palabras_episodio %>% 
  arrange(episodio, desc(tf_idf)) #%>% 
filter(!duplicated(episodio)) #%>% 
transmute(episodio,
          titulo,
          palabra,
          n, 
          total,
          tf_idf = round(tf_idf, 4))

episodio_peculiar %>%
  print(n = Inf)

episodio_peculiar <- palabras_episodio %>% 
  arrange(episodio, desc(tf_idf)) %>%
  group_by(episodio) %>%
  top_n(5)


# ANTES DE PASAR AL SIGUIENTE APÉNDICE
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.


# Apéndice NUBES

library(tidyverse)
library(tidytext)
library(wordcloud)

vacias <- read_csv("https://tinyurl.com/7PartidasVacias",
                   locale = default_locale())

ruta <- "https://tinyurl.com/7PartidasTextos/"

texto_entrada <- read_lines(paste(ruta, "cuatro_jinetes_apocalipsis.txt", sep = ""),
                            locale = default_locale())

texto_analisis <- tibble(texto = texto_entrada)

texto_analisis %>%
  unnest_tokens(palabra, texto) %>%
  anti_join(vacias) %>%
  count(palabra, sort = T) %>%
  with(wordcloud(palabra,
                 n,
                 max.words = 100,
                 color = brewer.pal(8, "Dark2")))

# ANTES DE PASAR AL SIGUIENTE APÉNDICE
# CIERRA RSTUDIO Y ARRÁNCALO DE NUEVO.
# ESTA SENCILLA ACCIÓN PUEDE SALVARTE
# DE VARIOS QUEBRADEROS DE CABEZA.

# COSECHAR CANCIONES

library(rvest)

dir.create("datos/sabina")

indice <- read_html("https://www.letras.com/joaquin-sabina/")

canciones <- indice %>%
  html_nodes("a") %>%
  html_attr("href")

cumplen <- grep("/joaquin-sabina/\\d+/", canciones)

canciones2 <- canciones[cumplen]

canciones2 <- paste("https://www.letras.com", canciones2, sep = "")

for (i in 1:length(canciones2)){
  cancion <- read_html(canciones2[i])
  letra <- gsub('<.*?>', '\n', html_nodes(cancion, "p"))
  titulo <- cancion %>%
    html_nodes("h1") %>%
    html_text()
  titulo <- titulo[2]
  letra <- gsub(".*Asociados.*", '', letra)
  letra <- gsub('.*Quiero recibir notificaciones.*', '', letra)
  letra <- gsub('.*Música comienza con letras.*', '', letra)
  titulo <- gsub(' ', "_", titulo)
  titulo <- gsub('[[:punct:]]', '', titulo)
  writeLines(letra, paste("datos/sabina/", titulo, ".txt", sep = ""))
}
