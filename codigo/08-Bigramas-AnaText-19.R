#######################      Bigramas y redes semánticas       ###########################
#                  Análisis Automático de Textos y Estilomtería con R                    #
#              Laboratorio de Innovación en Humanidades Digitales (LinhdUNED)            #
#                                       edición  2019                                    #

#                                         Basado en                                      #
#                                   Redes semánticas con R                               #
#                   https://rpubs.com/jboscomendoza/redes_semanticas_r                   #
#                                     Text Mining with R                                 #
#                              https://www.tidytextmining.com/                           #
#                                              y
#                              Text Mining: Word Relationships                           #
#                         http://uc-r.github.io/word_relationships                       #

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

mensajes %>%
  unnest_tokens(palabra,
                texto)


mensajes %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 3)

mensajes_bigramas <- mensajes %>%
  unnest_tokens(bigrama, texto, token = "ngrams", n = 2)

mensajes_bigramas %>%
  count(bigrama, sort = T)

bigramas_separados <- mensajes_bigramas %>%
  separate(bigrama,
           c("palabra1", "palabra2"),
           sep = " ") 

bigramas_filtrados <- bigramas_separados %>%
  filter(!palabra1 %in% vacias$palabra,
         !palabra2 %in% vacias$palabra)

bigramas_filtrados %>%
  count(palabra1, palabra2, sort = T)

# Borramos adhoc
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
  geom_col(aes(y = n , x = reorder(bigrama,n)),
           fill = "maroon") +
  coord_flip() +
  facet_wrap(~ rey, ncol = 2, scales = "free") +
  theme_linedraw() + 
  labs(x = "Bigramas", y = "Frecuencia") + 
  ggtitle("Bigramas más frecuentes de cada rey", subtitle = "1975-2018")

####

library(igraph)
library(ggraph)

recuento_bigramas <- bigramas_filtrados %>%
  count(palabra1, palabra2, sort = T)

grafo_bigramas <- recuento_bigramas %>%
  filter(n > 5) %>%
  graph_from_data_frame()

grafo_bigramas




ggraph(grafo_bigramas, layout = "nicely") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#set.seed(2016)

ggraph(grafo_bigramas, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = arrow(type = "closed",
                               length = unit(3, "mm"))) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

  

# Correlación: Qué palabras tienden a coocurrir en los párrafos

mensajes_rey <- mensajes %>%
  filter(rey =="Juan Carlos I") %>%
  mutate(seccion = row_number()) %>%
  unnest_tokens(palabra, texto) %>%
  filter(!palabra %in% vacias$palabra)

mensajes_rey
# Instala la library widyr
library(widyr)
pares_palabras <- mensajes_rey %>%
  pairwise_count(palabra, seccion, sort = T)

pares_palabras

pares_palabras %>%
  filter(item1 == "españoles")

palabras_correlacion <- mensajes_rey %>%
  group_by(palabra) %>%
  filter(n() >= 10) %>%
  pairwise_cor(palabra, seccion, sort = TRUE)

palabras_correlacion

palabras_correlacion %>%
  filter(item1 == "terrorismo")

palabras_correlacion %>%
  filter(item1 %in% c("mujeres", "hombres", "política", "patrimonio", "país", "trabajo")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

palabras_correlacion %>%
  filter(correlation > .30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
