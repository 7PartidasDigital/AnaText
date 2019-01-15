##################     Profundizando en el Análisis de textos (1)     ####################
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

# Carga las librerías
library(tidyverse)
library(tidytext)

# Ahora cargará todos los ficheros de los mensajes
ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")
anno <- gsub("\\.txt", "", ficheros, perl = T)
mensajes <- data_frame(anno = character(), parrafo = numeric(), texto = character())
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes", ficheros[i], sep = "/"))
  temporal <- data_frame(anno = anno[i], parrafo = seq_along(discurso), texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}

# Regenera la tabla general con todas las palabras
mensajes_palabras <- mensajes %>%
  unnest_tokens(palabra, texto)

# Crea la tabla con todas las palabras y calcula frecuencias
mensajes_frecuencias <- mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))

# Borra objetos que no sirven y que son temporales
rm(temporal,discurso,i)

# Continúa desde aquí…

# Palabras vacías de serie
vacias <- get_stopwords("es")
vacias
vacias <- vacias %>%
  rename(palabra = word)
vacias
mensajes_vaciado <- mensajes_palabras %>%
  anti_join(vacias)

# Palabras vacías de "diseño"
vacias <- as_tibble(read.delim("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/vacias.txt",
                               header= TRUE,
                               quote = '"',
                               encoding = "UTF-8",
                               stringsAsFactors = F))
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
  ggtitle("Mensajes de Navidad") +
  coord_flip()


# Un inciso: Emilia Pardo Bazán
pazos <- data_frame(texto = read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/pazos_ulloa.txt",
                                       locale = default_locale(),
                                       skip = 3))

pazos_palabras <- pazos %>%
  unnest_tokens(palabra, texto) %>%
    anti_join(vacias)
vacias_adhoc <- data_frame(palabra = c("gabriel", "julián",
                                       "nucha", "marqués",
                                       "perucho", "pedro",
                                       "ulloa", "don"))
pazos_palabras <- pazos_palabras %>%
  anti_join(vacias_adhoc)
pazos_palabras %>%
  count(palabra, sort = T) %>%
  filter(n > 155) %>%
  mutate(palabra = reorder(palabra, n)) %>%
  ggplot(aes(x = palabra, y = n, fill = palabra)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Los Pazos de Ulloa\nEmilia Pardo Bazán") +
  coord_flip()
rm(pazos, pazos_palabras, vacias_adhoc)
# Fin inciso


mensajes_vaciado <- mensajes_vaciado %>%
  mutate(rey = anno) %>%
  mutate(rey = str_replace(rey, "201[45678]", "Felipe VI")) %>%  
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
  labs(x = "", y = "Frecuencia") +
  coord_flip() +
  theme(legend.position="none")


mensaje_anno <- mensajes_vaciado %>%
  select(rey, anno, palabra) %>%
  group_by(anno) %>%
  count(palabra, sort =T) %>%
  ungroup()

mensaje_anno <- mensajes_vaciado %>%
  select(rey, anno, palabra) %>%
  filter(rey == "Felipe VI") %>%
  group_by(anno) %>%
  count(palabra, sort =T) %>%
  ungroup()

mensaje_anno %>%
  filter(n > 5) %>%
  ggplot(aes(x = palabra, y = n)) +
  geom_col(fill = "aquamarine") +
  coord_flip() +
  facet_wrap(~ anno, ncol = 3, scales = "free_y")
