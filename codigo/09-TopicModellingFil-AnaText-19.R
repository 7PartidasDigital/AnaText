################################# TOPIC MODELLING CON TIDYTEXT ##############################
#                  Análisis Automático de Textos y Estilometría con R                       #
#              Laboratorio de Innovación en Humanidades Digitales (LinhdUNED)               #
#                                       edición  2019                                       #
#                                                                                           #
#                                         Basado en                                         #
#                                                                                           #
#       https://cran.r-project.org/web/packages/tidytext/vignettes/topic_modeling.html      #
#                     https://www.tidytextmining.com/topicmodeling.html                     #
# https://peerchristensen.netlify.com/post/topic-modelling-of-trustpilot-reviews-with-r-and-tidytext/
#           https://www.datacamp.com/community/tutorials/ML-NLP-lyric-analysis
#                       https://cfss.uchicago.edu/text_topicmodels.html
#                                                                                           #
#                                 Adaptación desarrollada por                               #
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
#                                                                                           #


# Establece directorio de trabajo
# Recuerda que tiene que ser AnaText

# Carga las librerías necesarias
library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)
library(scales)

# Carga la lista de stopwords españolas
vacias <- as_tibble(read_delim("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt",
                               delim = "\t",
                               col_names = TRUE,
                               quote = "\"",
                               locale = default_locale()))
# Los textos los cargas desde un repositorio externo
url <- "https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/filosofos/"
titulos <- c("Chomsky", "Freud", "Maquiavelo", "Voltaire")
ficheros <- c("filosofo1.txt", "filosofo2.txt", "filosofo3.txt", "filosofo4.txt")
ensayos <- tibble(texto = character(),
                      titulo = character(),
                      pagina = numeric())

# Lee los texto y los divide en "páginas"
for (j in 1:length(ficheros)){
texto.entrada <- read_lines(paste(url, ficheros[j], sep = ""),
                            locale = default_locale())
texto.todo <- paste(texto.entrada, collapse = " ")
por.palabras <- strsplit(texto.todo, " ")
texto.palabras <- por.palabras[[1]]
trozos <- split(texto.palabras,
                ceiling(seq_along(texto.palabras)/375))
for (i in 1:length(trozos)){
  fragmento <- trozos[i]
  fragmento.unido <- data_frame(texto = paste(unlist(fragmento),
                                              collapse = " "),
                                titulo = titulos[j],
                                pagina = i)
  ensayos <- bind_rows(ensayos, fragmento.unido)
}
}

# Borra todas los objetos que no serán necesarios
rm(ficheros, titulos, trozos, fragmento, url, fragmento.unido, texto.entrada, texto.palabras, texto.todo, por.palabras, i, j)

# Divide (tokeniza) en palabras por capítulo
por_pagina_palabras <- ensayos %>%   
  unite(titulo_pagina, titulo, pagina) %>%                 
  unnest_tokens(palabra, texto)  

por_pagina_palabras

por_pagina_palabras %>%
  count(palabra, sort = T)

# Elimina palabras vacías
palabra_conteo <- por_pagina_palabras %>%   
  anti_join(vacias) %>%   
  count(titulo_pagina, palabra, sort = TRUE) %>%   
  ungroup()  

# Y ahora podemos saber cuántas palabras de "valor" hay en cada capítulo y cuál es su frecuencia
palabra_conteo

# En este momento, este dataframe está ordenado, con un término por documento por fila. 
# Sin embargo, el paquete TOPICMODELS requiere un DocumentTermMatrix (del paquete TM). 
# Se logra la adaptación a DocumentTermMatrix con cast_dtm de tidytext:
paginas_dtm <- palabra_conteo %>%
  cast_dtm(titulo_pagina, palabra, n)

paginas_dtm

# Ahora estás listo para usar el paquete topicmodels y crear un modelo LDA 
# con varios tópicos. Lo vas hacer con uno por cada título.
# Sin embargo, si se requiere un mayor número de tópicos al de títulos de obras, entonces,
# el valor de k = 4.

paginas_lda <- LDA(paginas_dtm, k = 4, control = list(seed = 1234)) # Ojo al valor de k
paginas_lda

# (En este caso, sabemos que hay 4 temas porque hay 4 libros, 
# en la práctica es posible que tengamos que probar algunos valores diferentes para k).
# Ahora tidytext da la opción de volver a un análisis ordenado, 
# utilizando las voces ordenadas.

paginas_lda_td <- tidy(paginas_lda, matrix = "beta")
paginas_lda_td

# Para cada combinación, el modelo decide la probabilidad de que ese término se genere a partir de ese tema.
# Podríamos usar top_n  para encontrar los 5 términos principales dentro de cada tema:

terminos_frecuentes <- paginas_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terminos_frecuentes 

# El modelo se presta a una visualización:

terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# ¡Estos temas están claramente asociados con los cuatro libros!

# Clasificación por documento

# Cada "páginas" es un "documento" en este análisis. Por lo tanto, es
# posible que quieras saber qué temas están asociados con cada documento. 

# Surge una cuestión importante: ¿Ouedes juntar los pñaginas en los pensadores correctos?

paginas_lda_gamma <- tidy(paginas_lda, matrix = "gamma")
paginas_lda_gamma

# Separa el nombre del documento en título y número de página:

paginas_lda_gamma <- paginas_lda_gamma %>%
  separate(document, c("titulo", "pagina"), sep = "_", convert = TRUE)
paginas_lda_gamma


# Cuando examinamos resultados
ggplot(paginas_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() +
  facet_wrap(~ titulo, nrow = 2)

# Notamos que casi todos los páginas refereridas a Chomsky, Freud y Maquiavelo 
# se identificaron de manera casi única como un único tópico (pensador).

paginas_clasificaciones <- paginas_lda_gamma %>%
  group_by(titulo, pagina) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

paginas_clasificaciones

# Lo siguiente confirma numéricamente lo que se ha visto en el gráfico anterior
# ya que busca el filósofo de consenso para cada tópico

topico_pensador <- paginas_clasificaciones %>%
  count(titulo, topic) %>%
  group_by(titulo) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consenso = titulo, topic)

topico_pensador

# Ahora puedes ver qué páginas identificó erróneamente:

paginas_clasificaciones %>%
  inner_join(topico_pensador, by = "topic") %>%
  filter(titulo != consenso)

# La mayoría de los errores de asignación de produjo en Voltaire. 

# Por asignaciones de palabras: augment

# Un paso importante es asignar cada palabra de cada documento a un tópico 
# Cuantas más palabras en un documento se asignan a ese tópico, en general,
# más peso (gamma) tienen

asignaciones <- augment(paginas_lda, data = paginas_dtm)

asignaciones

# Podemos combinar esto con los identificadores de los filósofos
# para ver qué  palabras fueron clasificadas incorrectamente.

asignaciones <- asignaciones %>%
  separate(document, c("titulo", "pagina"),
           sep = "_",
           convert = TRUE) %>%
  inner_join(topico_pensador,
             by = c(".topic" = "topic"))

asignaciones

# Puedes crear una "matriz de confusión" para ver con mayor claridad
# los aciertos y los errores de asignación

asignaciones %>% 
  count(titulo, consenso, wt = count) %>%
  spread(consenso, n, fill = 0)


asignaciones %>%
  count(titulo, consenso, wt = count) %>%
  group_by(titulo) %>%
  mutate(porcentaje = n / sum(n)) %>%
  ggplot(aes(consenso, titulo, fill = porcentaje)) +
  geom_tile() +
  scale_fill_gradient2(high = "blue", label = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Asignó las palabras a…",
       y = "Las palabras procedían de…",
       fill = "% de asignaciones")

# ¿Cuáles fueron las palabras más comúnmente confundidas?

palabras_equivocadas <- asignaciones %>%
  filter(titulo != consenso)

palabras_equivocadas

palabras_equivocadas %>%
  count(titulo, consenso, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))
