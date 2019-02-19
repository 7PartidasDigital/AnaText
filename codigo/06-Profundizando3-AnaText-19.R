##################     Profundizando en el Análisis de textos (3)     ####################
#                                                                                        #
#          dentro del curso Análisis Automático de Textos y Estilomtería con R           #
#          del Laboratorio de Innovación en Humanidades Digitales (LinhdUNED)            #
#                                       edición 2019                                     #
#                                         basado en                                      #
#                             https://uc-r.github.io/tidy_text                           #

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
options(digits = 8)
options(scipen=999) # Para evitar notación científica
ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")
anno <- gsub("\\.txt", "", ficheros, perl = T)
FF <- rep("Franco", 32)
JCI <- rep("Juan Carlos I", 39)
FVI <- rep("Felipe VI", 5)
rey <- c(FF, JCI, FVI)
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
vacias <- as_tibble(read.delim("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt",
                               header= TRUE,
                               quote = '"',
                               encoding = "UTF-8",
                               stringsAsFactors = F))


mensajes_palabras <- mensajes %>%
  unnest_tokens(palabra, texto) %>%
  select(rey, everything())

mensajes_palabras$rey <- factor(mensajes_palabras$rey, levels = c("Franco", "Juan Carlos I", "Felipe VI"))

mensajes_palabras %>%
  anti_join(vacias) %>%
  group_by(rey) %>%
  count(palabra, sort = T) %>%
  top_n(10)

mensajes_palabras %>%
  anti_join(vacias) %>%
  group_by(rey) %>%
  count(palabra, sort = T) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(rey = factor(rey,
                      levels = c("Franco", "Juan Carlos I", "Felipe VI")),
                      texto_ordenado = nrow(.):1) %>%
           ggplot(aes(reorder(palabra, texto_ordenado), n, fill = rey)) +
           geom_bar(stat = "identity") +
           facet_wrap(~ rey, scales = "free_y") +
           labs(x = "NULL", y = "Frecuencia") +
           coord_flip() +
           theme(legend.position="none")

# calculate percent of word use across all novels
mensajes_porcentaje <- mensajes_palabras %>%
  anti_join(vacias) %>%
  count(palabra) %>%
  transmute(palabra, todas_palabras = n / sum(n))

# calculate percent of word use within each novel
frecuencia <- mensajes_palabras %>%
  anti_join(vacias) %>%
  count(rey, palabra) %>%
  mutate(mensaje_palabras = n / sum(n)) %>%
  left_join(mensajes_porcentaje) %>%
  arrange(desc(mensaje_palabras)) %>%
  ungroup()

ggplot(frecuencia, aes(x = mensaje_palabras, y = todas_palabras, color = abs(todas_palabras - mensaje_palabras))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = palabra), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ rey, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Mensajes de Navidad 1937-2018", x = NULL)

frecuencia %>%
  group_by(rey) %>%
  summarize(correlacion = cor(mensaje_palabras, todas_palabras),
            p_valor = cor.test(mensaje_palabras, todas_palabras)$p.value)
