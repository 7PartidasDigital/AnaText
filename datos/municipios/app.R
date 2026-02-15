# app.R
library(shiny)
library(bslib)
library(shinycssloaders)
library(leaflet)
library(DT)
library(dplyr)
library(stringi)
library(htmlwidgets)
library(webshot2)
library(htmltools)

options(scipen = 999)

# -------------------------------
# BBOX España peninsular
# -------------------------------
LAT_MIN <- 35.95
LAT_MAX <- 43 + 25/60
LON_MIN <- -10
LON_MAX <- 4.40

TSV_URL <- "https://raw.githubusercontent.com/7PartidasDigital/AnaText/refs/heads/master/datos/municipios/toponimos.tsv"

# -------------------------------
# Helpers
# -------------------------------
norm_txt <- function(x, strip_accents = TRUE){
  x <- tolower(trimws(as.character(x)))
  if(strip_accents) x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x
}

parse_num <- function(x){
  x <- gsub(",", ".", trimws(as.character(x)), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Escapa metacaracteres regex de una cadena (versión robusta)
escape_regex <- function(s) {
  if (is.null(s)) return(s)
  gsub("([\\.\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", s, perl = TRUE)
}

download_tsv_with_fallbacks <- function(url) {
  alt1 <- gsub("/refs/heads/", "/", url, fixed = TRUE)
  candidates <- unique(c(url, alt1))
  
  tf <- tempfile(fileext = ".tsv")
  for (u in candidates) {
    ok <- tryCatch({
      suppressWarnings(download.file(u, tf, quiet = TRUE, mode = "wb"))
      TRUE
    }, error = function(e) FALSE)
    if (ok && file.exists(tf) && file.info(tf)$size > 0) return(tf)
  }
  stop("No se pudo descargar el TSV.")
}

read_and_prepare <- function(path, sep = "\t", header = TRUE, clip_bbox = TRUE) {
  df <- read.delim(
    path,
    sep = sep,
    header = header,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    quote = "",
    fileEncoding = "UTF-8"
  )
  
  # Requerimos lon/lat y columnas mínimas
  needed <- c("tipo", "toponimo", "idioma", "lon", "lat", "provincia")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop("Faltan columnas esperadas en la tabla: ", paste(miss, collapse = ", "))
  }
  
  df <- df %>%
    mutate(
      lon = parse_num(lon),
      lat = parse_num(lat)
    ) %>%
    filter(!is.na(lon), !is.na(lat))
  
  if (isTRUE(clip_bbox)) {
    df <- df %>% filter(
      lat >= LAT_MIN, lat <= LAT_MAX,
      lon >= LON_MIN, lon <= LON_MAX
    )
  }
  
  df
}

# -------------------------------
# UI
# -------------------------------
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#003366",
    base_font = font_google("Source Sans Pro")
  ),
  
  tags$style(HTML("
    .uva-header { display:flex; align-items:center; gap:20px; padding:12px 0; border-bottom:2px solid #003366; margin-bottom:18px; }
    .uva-logo { height:55px; }
    .uva-title { font-size:26px; font-weight:600; color:#003366; }
    .footer-uv { text-align:center; color:#666; font-size:12px; padding:18px 0; border-top:1px solid #ddd; margin-top:40px; }
    .small-note { font-size:11px; color:#777; }
    .counters { font-size:13px; color:#333; margin-bottom:8px; }
    .results-actions { margin-bottom:10px; }
  ")),
  
  # header con logo
  tags$div(
    class = "uva-header",
    tags$img(src = "https://imagencorporativa.uva.es/.marca_principal_horizontal/AZUL-P654C/logo-pantone-654.png", class = "uva-logo"),
    tags$div(class = "uva-title", "Buscador de topónimos")
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      
      h4("Datos"),
      radioButtons("data_source", "Usar datos de:",
                   choices = c("Base remota" = "base",
                               "Subir mi tabla (TSV)" = "upload",
                               "Mi tabla guardada" = "saved"),
                   selected = "base"),
      fileInput("file", "Subir TSV (si eliges 'Subir mi tabla')", accept = c(".tsv", ".txt")),
      actionButton("save_filtered", "Guardar resultado como mi tabla", icon = icon("save")),
      actionButton("clear_saved", "Borrar tabla guardada", icon = icon("trash")),
      tags$div(class = "small-note", "Guardar permite iterar: exportas, corriges y vuelves a usar"),
      
      # -- CONTADOR dinámico --
      tags$hr(),
      div(class = "counters",
          textOutput("counters")
      ),
      
      tags$hr(),
      
      h4("Búsqueda"),
      textInput("q", NULL, placeholder = "Introduce un topónimo (ej: torre, vill, fuente...)"),
      radioButtons("mode", "Tipo de búsqueda",
                   choices = c("Contiene" = "contains",
                               "Exacta" = "exact",
                               "Comienza (palabra)" = "starts",
                               "Acaba (palabra)" = "ends"),
                   selected = "contains"),
      checkboxInput("ignore_accents", "Ignorar acentos", TRUE),
      tags$div(class = "small-note", "Comienza/Acaba usan límites de palabra \\b (palabras enteras)"),
      
      tags$hr(),
      
      h4("Mapa"),
      checkboxInput("cluster", "Agrupar puntos", TRUE),
      sliderInput("max_points", "Máx. puntos", min = 100, max = 10000, value = 5000, step = 100),
      
      tags$hr(),
      
      downloadButton("download_map", "Descargar mapa"),
      br(), br(),
      downloadButton("download_table_small", "Descargar tabla (TSV: topónimo+provincia)"),
      br(), br(),
      downloadButton("download_dataset_full", "Descargar dataset (TSV completa)"),
      br(), br(),
      downloadButton("download_example", "Descargar ejemplo (TSV)")
    ),
    
    # Tabset con Mapa / Resultados / Ayuda
    card(
      full_screen = TRUE,
      tabsetPanel(
        tabPanel(
          title = "Mapa",
          withSpinner(leafletOutput("map", height = "68vh"), type = 4)
        ),
        
        tabPanel(
          title = "Resultados",
          div(style = "padding:12px;",
              div(class = "results-actions",
                  actionButton("remove_selected", "Eliminar seleccionados y guardar", icon = icon("trash")),
                  span(" "),
                  actionButton("clear_selection", "Limpiar selección", icon = icon("eraser"))
              ),
              DTOutput("results_tbl")
          )
        ),
        
        tabPanel(
          title = "Ayuda",
          div(style = "padding:20px; line-height:1.5; max-width:900px;",
              h4("Cómo usar la app — Guía rápida"),
              p("Esta pestaña resume las funciones principales y da ejemplos para buscar y gestionar datos."),
              
              h5("1) Fuentes de datos"),
              tags$ul(
                tags$li(strong("Base remota:"), " la tabla por defecto (no necesitas subir nada)."),
                tags$li(strong("Subir mi tabla:"), " carga tu propio TSV (debe contener al menos las columnas: tipo, toponimo, idioma, lon, lat, provincia)."),
                tags$li(strong("Mi tabla guardada:"), " si guardaste un resultado filtrado con el botón “Guardar resultado como mi tabla”, puedes seleccionarlo aquí.")
              ),
              
              h5("2) Búsqueda"),
              tags$ul(
                tags$li(strong("Contiene:"), " busca la cadena en cualquier parte del toponimo (p. ej. 'rosa' encontrará 'La Rosa', 'rosal')."),
                tags$li(strong("Exacta:"), " el toponimo (después de normalizar acentos y minúsculas) debe coincidir exactamente con el término."),
                tags$li(strong("Comienza (palabra):"), " buscará el término como inicio de palabra usando límites de palabra (\\b). Ej: 'san' encontrará 'San Juan' y 'San Pedro', pero no 'Lasangria'."),
                tags$li(strong("Acaba (palabra):"), " buscará el término como final de palabra usando límites de palabra (\\b). Ej: 'del' encontrará 'Puente del Rey' pero no 'Delgado'.")
              ),
              
              h5("3) Normalización y caracteres especiales"),
              p("Si activas 'Ignorar acentos' la búsqueda compara versiones sin acentos (Á → A). Además, la app escapa caracteres especiales (por ejemplo '+' o '?') para que puedas buscar literales sin romper las expresiones regulares."),
              
              h5("4) Guardar / iterar"),
              tags$ul(
                tags$li("Usa ", strong("Guardar resultado como mi tabla"), " para mantener en memoria la tabla filtrada completa (todas las columnas)."),
                tags$li("Puedes descargarla con ", strong("Descargar dataset (TSV completa)"), " y editarla localmente. Si la subes de nuevo (Subir mi tabla), podrás seguir trabajando sin rehacer filtros.")
              ),
              
              h5("5) Descargas"),
              tags$ul(
                tags$li(strong("Descargar mapa:"), " genera un PNG del mapa actual."),
                tags$li(strong("Descargar tabla (TSV: topónimo+provincia):"), " exporta solo las dos columnas para usos rápidos."),
                tags$li(strong("Descargar dataset (TSV completa):"), " exporta todas las columnas de la tabla activa (remota/subida/guardada)."),
                tags$li(strong("Descargar ejemplo (TSV):"), " descarga 10 filas de ejemplo para ver el formato correcto.")
              ),
              
              h5("6) Resolución de problemas"),
              tags$ul(
                tags$li("Si la tabla se queda en blanco: comprueba que el TSV tiene nombres exactos de columnas y que lon/lat son numéricos (las comas decimales se convierten automáticamente)."),
                tags$li("Si la descarga del PNG falla en shinyapps.io: usa la descarga del dataset y genera el mapa localmente o pídeme que añada la opción de exportar HTML interactivo."),
                tags$li("Si subes un TSV con separador distinto, conviértelo a TSV (tab) o pídeme que añada autodetección de separador.")
              ),
              
              h5("7) Ejemplos rápidos"),
              tags$ul(
                tags$li(strong("Buscar por inicio de palabra:"), " escribe \"san\" + selecciona \"Comienza\" → devolverá 'San Bartolomé', 'San Juan'..."),
                tags$li(strong("Buscar por final de palabra:"), " escribe \"del\" + selecciona \"Acaba\" → devolverá 'Puente del Rey', 'Valle del'...")
              ),
              
              br(),
              p(class = "small-note", "¿Quieres que añada capturas o un PDF de ayuda? Puedo incluirlo.")
          )
        )
      )
    )
  ),
  
  tags$div(class = "footer-uv",
           tags$b("GIR Filología Digital"),
           tags$br(),
           "Universidad de Valladolid",
           tags$br(),
           "2026")
)

# -------------------------------
# SERVER
# -------------------------------
server <- function(input, output, session){
  
  # cache: base remota descargada UNA sola vez por proceso
  base_df <- reactiveVal(NULL)
  observeEvent(TRUE, {
    tryCatch({
      tf <- download_tsv_with_fallbacks(TSV_URL)
      base_df(read_and_prepare(tf, sep = "\t", header = TRUE, clip_bbox = TRUE))
    }, error = function(e) {
      base_df(data.frame())
      showNotification(paste("Error al descargar base remota:", conditionMessage(e)), type = "error")
    })
  }, once = TRUE)
  
  # lugar para almacenar la tabla "guardada" por el usuario (en memoria del servidor)
  saved_df <- reactiveVal(NULL)
  
  # función para leer tabla subida por usuario (sin tocar base)
  uploaded_df <- reactive({
    f <- input$file
    req(input$data_source == "upload")
    if (is.null(f)) return(NULL)
    tryCatch({
      read_and_prepare(f$datapath, sep = "\t", header = TRUE, clip_bbox = TRUE)
    }, error = function(e) {
      showNotification(paste("Error al leer archivo subido:", conditionMessage(e)), type = "error")
      return(NULL)
    })
  })
  
  # seleccionar la fuente activa según radioButtons
  active_df <- reactive({
    src <- input$data_source
    if (src == "base") {
      df <- base_df()
      if (is.null(df)) return(data.frame())
      return(df)
    } else if (src == "upload") {
      df <- uploaded_df()
      if (is.null(df)) return(data.frame())
      return(df)
    } else if (src == "saved") {
      df <- saved_df()
      if (is.null(df)) {
        showNotification("No hay tabla guardada.", type = "warning")
        return(data.frame())
      }
      return(df)
    }
    data.frame()
  })
  
  # Guardar resultado filtrado como "mi tabla" (botón general)
  observeEvent(input$save_filtered, {
    df_act <- active_df()
    if (nrow(df_act) == 0) {
      showNotification("No hay datos para guardar.", type = "warning")
      return()
    }
    df_filt <- filtered_full()
    if (nrow(df_filt) == 0) {
      showNotification("No hay filas filtradas para guardar.", type = "warning")
      return()
    }
    saved_df(df_filt)
    showNotification("Resultado guardado como 'Mi tabla'. Ahora selecciona 'Mi tabla guardada' en 'Usar datos de:' para usarla.", type = "message")
  })
  
  observeEvent(input$clear_saved, {
    saved_df(NULL)
    showNotification("Tabla guardada borrada.", type = "message")
  })
  
  # ---- Filtrado ----
  # filtered_full: filtra la tabla ACTIVA y devuelve todas las columnas
  filtered_full <- reactive({
    df <- active_df()
    if (nrow(df) == 0) return(df)
    
    q_raw <- trimws(input$q)
    if (!nzchar(q_raw)) return(df)
    
    # normalización
    strip_acc <- isTRUE(input$ignore_accents)
    top_norm <- norm_txt(df$toponimo, strip_accents = strip_acc)
    q_norm <- norm_txt(q_raw, strip_accents = strip_acc)
    
    mode <- input$mode
    
    # escape del término para que no interprete metacaracteres
    q_esc <- escape_regex(q_norm)
    
    keep <- switch(mode,
                   contains = grepl(q_esc, top_norm, perl = TRUE),
                   exact = top_norm == q_norm,
                   starts = grepl(paste0("\\b", q_esc), top_norm, perl = TRUE),
                   ends = grepl(paste0(q_esc, "\\b"), top_norm, perl = TRUE),
                   grepl(q_esc, top_norm, perl = TRUE)
    )
    
    df[which(keep), , drop = FALSE]
  })
  
  # map_data: versión limitada en tamaño para el mapa (max_points)
  map_data <- reactive({
    d <- filtered_full()
    if (nrow(d) > input$max_points) d <- d[seq_len(input$max_points), , drop = FALSE]
    d
  })
  
  # ---- RENDER MAP ----
  output$map <- renderLeaflet({
    d <- map_data()
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    
    if (nrow(d) == 0) return(m %>% setView(-3.7, 40.4, 6))
    
    popup <- paste0("<b>", htmlEscape(d$toponimo), "</b><br>", htmlEscape(d$provincia))
    
    if (isTRUE(input$cluster)) {
      m <- m %>% addCircleMarkers(
        lng = d$lon, lat = d$lat, radius = 5, stroke = FALSE, fillOpacity = .8,
        popup = popup, clusterOptions = markerClusterOptions()
      )
    } else {
      m <- m %>% addCircleMarkers(lng = d$lon, lat = d$lat, radius = 5, stroke = FALSE, fillOpacity = .8, popup = popup)
    }
    
    m %>% fitBounds(min(d$lon), min(d$lat), max(d$lon), max(d$lat))
  })
  
  # ---- CONTADORES DINÁMICOS ----
  output$counters <- renderText({
    src_df <- active_df()
    n_src <- if (is.data.frame(src_df)) nrow(src_df) else 0
    n_filt <- nrow(filtered_full())
    paste0("Fuente: ", n_src, " filas  |  Filtradas: ", n_filt, " filas")
  })
  
  # ---- RENDER RESULTS DT ----
  output$results_tbl <- renderDT({
    dat <- filtered_full()
    datatable(dat, options = list(pageLength = 15, scrollX = TRUE), selection = list(mode = "multiple"))
  }, server = TRUE)
  
  # ---- ACCIONES SOBRE RESULTADOS: eliminar seleccionados y guardar ----
  observeEvent(input$remove_selected, {
    sel <- input$results_tbl_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("No hay filas seleccionadas.", type = "warning")
      return()
    }
    
    df_filt <- filtered_full()
    if (nrow(df_filt) == 0) {
      showNotification("No hay filas filtradas.", type = "warning")
      return()
    }
    
    # eliminar las filas seleccionadas (sel son índices relativos a datatable)
    remaining <- df_filt[-sel, , drop = FALSE]
    saved_df(remaining)
    showNotification(paste0("Se eliminaron ", length(sel), " fila(s) y se guardó el resultado como 'Mi tabla'."), type = "message")
    
    # opcional: cambiar la fuente activa a 'saved' automáticamente
    updateRadioButtons(session, "data_source", selected = "saved")
  })
  
  # limpiar selección en la tabla (nota: esto solo resetea la selección en el cliente)
  observeEvent(input$clear_selection, {
    proxy <- dataTableProxy("results_tbl")
    selectRows(proxy, NULL)
  })
  
  # ---- DESCARGAS ----
  # mapa PNG
  output$download_map <- downloadHandler(
    filename = function() paste0("mapa_toponimos_", Sys.Date(), ".png"),
    content = function(file) {
      d <- map_data()
      m <- leaflet(d) %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircleMarkers(~lon, ~lat, radius = 5, stroke = FALSE)
      tmp <- tempfile(fileext = ".html")
      saveWidget(m, tmp, selfcontained = TRUE)
      webshot2::webshot(tmp, file, vwidth = 1400, vheight = 900, zoom = 2)
    }
  )
  
  # descarga pequeña: toponimo + provincia
  output$download_table_small <- downloadHandler(
    filename = function() {
      q <- trimws(input$q); suf <- if (nzchar(q)) gsub("\\s+", "_", norm_txt(q, TRUE)) else "todo"
      paste0("toponimos_", suf, "_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      d <- filtered_full() %>% select(toponimo, provincia) %>% distinct()
      write.table(d, file, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # descarga completa: todas las columnas de la tabla activa filtrada
  output$download_dataset_full <- downloadHandler(
    filename = function() {
      q <- trimws(input$q); suf <- if (nzchar(q)) gsub("\\s+", "_", norm_txt(q, TRUE)) else "todo"
      paste0("toponimos_full_", suf, "_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      d <- filtered_full()
      write.table(d, file, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # descarga de ejemplo: 10 filas (si hay base remota), o estructura mínima si no hay datos
  output$download_example <- downloadHandler(
    filename = function() paste0("ejemplo_toponimos_", Sys.Date(), ".tsv"),
    content = function(file) {
      base <- base_df()
      if (is.null(base) || nrow(base) == 0) {
        # estructura mínima
        sample_df <- data.frame(
          tipo = c("Municipio"), toponimo = c("Ejemplo"), idioma = c(NA),
          lon = c(-3.5), lat = c(40.4), provincia = c("EjemploProv"),
          stringsAsFactors = FALSE
        )
        write.table(sample_df, file, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
      } else {
        ex <- head(base, 10)
        write.table(ex, file, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  # avisos al cambiar source
  observe({
    if (input$data_source == "saved" && is.null(saved_df())) {
      showNotification("No hay tabla guardada. Guarda un resultado o sube un TSV.", type = "warning", duration = 4)
    }
  })
}

shinyApp(ui, server)
