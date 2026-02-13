library(shiny)
library(bslib)
library(shinycssloaders)
library(leaflet)
library(dplyr)
library(stringi)
library(htmlwidgets)
library(webshot2)
library(htmltools)

options(scipen = 999)

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

download_tsv_with_fallbacks <- function(url) {
  
  alt1 <- gsub("/refs/heads/", "/", url, fixed = TRUE)
  candidates <- unique(c(url, alt1))
  
  tf <- tempfile(fileext = ".tsv")
  
  for (u in candidates) {
    ok <- tryCatch({
      suppressWarnings(download.file(u, tf, quiet = TRUE, mode = "wb"))
      TRUE
    }, error = function(e) FALSE)
    
    if (ok && file.exists(tf) && file.info(tf)$size > 0) {
      return(tf)
    }
  }
  
  stop("No se pudo descargar el TSV.")
}

load_data <- function(){
  
  tf <- download_tsv_with_fallbacks(TSV_URL)
  
  df <- read.delim(
    tf,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  df %>%
    mutate(
      lon = parse_num(lon),
      lat = parse_num(lat)
    ) %>%
    filter(
      !is.na(lon), !is.na(lat),
      lat >= LAT_MIN, lat <= LAT_MAX,
      lon >= LON_MIN, lon <= LON_MAX
    )
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
  
  # ---------- CSS PROFESIONAL ----------
  tags$style(HTML("

    .uva-header {
      display:flex;
      align-items:center;
      gap:20px;
      padding:12px 0;
      border-bottom:2px solid #003366;
      margin-bottom:18px;
    }

    .uva-logo {
      height:55px;
    }

    .uva-title {
      font-size:26px;
      font-weight:600;
      color:#003366;
    }

    .footer-uv {
      text-align:center;
      color:#666;
      font-size:12px;
      padding:18px 0;
      border-top:1px solid #ddd;
      margin-top:40px;
    }

  ")),
  
  # ---------- HEADER CON LOGO ----------
  tags$div(
    class = "uva-header",
    
    tags$img(
      src = "https://imagencorporativa.uva.es/.marca_principal_horizontal/AZUL-P654C/logo-pantone-654.png",
      class = "uva-logo"
    ),
    
    tags$div(
      class = "uva-title",
      "Buscador de topónimos españoles"
    )
  ),
  
  layout_sidebar(
    
    sidebar = sidebar(
      width = 330,
      
      h4("Búsqueda"),
      
      textInput(
        "q",
        NULL,
        placeholder = "Introduce un topónimo (ej: torre, vill, fuente...)"
      ),
      
      radioButtons(
        "mode",
        "Modo",
        choices = c("Contiene" = "contains", "Exacta" = "exact"),
        inline = TRUE
      ),
      
      checkboxInput("ignore_accents", "Ignorar acentos", TRUE),
      
      tags$hr(),
      
      h4("Mapa"),
      
      checkboxInput("cluster", "Agrupar puntos", TRUE),
      
      sliderInput(
        "max_points",
        "Máx. puntos",
        min = 100,
        max = 10000,
        value = 5000,
        step = 100
      ),
      
      tags$hr(),
      
      downloadButton(
        "download_map",
        "Descargar mapa",
        class = "btn-primary"
      ),
      
      br(), br(),
      
      downloadButton(
        "download_table",
        "Descargar tabla (TSV)",
        class = "btn-outline-primary"
      )
    ),
    
    card(
      full_screen = TRUE,
      withSpinner(
        leafletOutput("map", height = "78vh"),
        type = 4
      )
    )
  ),
  
  # ---------- FOOTER ----------
  tags$div(
    class = "footer-uv",
    tags$b("GIR Filología Digital"),
    tags$br(),
    "Universidad de Valladolid",
    tags$br(),
    "2026"
  )
)

# -------------------------------
# SERVER
# -------------------------------
server <- function(input, output, session){
  
  df <- reactiveVal(load_data())
  
  filtered <- reactive({
    
    req(df())
    data <- df()
    
    q_raw <- trimws(input$q)
    
    if (!nzchar(q_raw))
      return(data)
    
    top <- norm_txt(data$toponimo, input$ignore_accents)
    q   <- norm_txt(q_raw, input$ignore_accents)
    
    keep <- if (input$mode == "exact")
      top == q
    else
      grepl(q, top, fixed = TRUE)
    
    data[keep, , drop = FALSE]
  })
  
  map_data <- reactive({
    
    d <- filtered()
    
    if (nrow(d) > input$max_points)
      d <- d[seq_len(input$max_points), ]
    
    d
  })
  
  output$map <- renderLeaflet({
    
    d <- map_data()
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    if (nrow(d) == 0)
      return(m %>% setView(-3.7, 40.4, 6))
    
    popup <- paste0(
      "<b>", htmlEscape(d$toponimo), "</b><br>",
      htmlEscape(d$provincia)
    )
    
    if (input$cluster) {
      
      m <- m %>%
        addCircleMarkers(
          lng = d$lon,
          lat = d$lat,
          radius = 5,
          stroke = FALSE,
          fillOpacity = .8,
          popup = popup,
          clusterOptions = markerClusterOptions()
        )
      
    } else {
      
      m <- m %>%
        addCircleMarkers(
          lng = d$lon,
          lat = d$lat,
          radius = 5,
          stroke = FALSE,
          fillOpacity = .8,
          popup = popup
        )
    }
    
    m %>% fitBounds(
      min(d$lon),
      min(d$lat),
      max(d$lon),
      max(d$lat)
    )
  })
  
  output$download_map <- downloadHandler(
    
    filename = function(){
      paste0("mapa_toponimos_", Sys.Date(), ".png")
    },
    
    content = function(file){
      
      d <- map_data()
      
      m <- leaflet(d) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(~lon, ~lat, radius = 5, stroke = FALSE)
      
      tmp <- tempfile(fileext = ".html")
      
      saveWidget(m, tmp, selfcontained = TRUE)
      
      webshot2::webshot(
        tmp,
        file,
        vwidth = 1400,
        vheight = 900,
        zoom = 2
      )
    }
  )
  
  output$download_table <- downloadHandler(
    
    filename = function(){
      
      q <- trimws(input$q)
      
      suf <- if (nzchar(q))
        gsub("\\s+", "_", norm_txt(q, TRUE))
      else
        "todo"
      
      paste0("toponimos_", suf, "_", Sys.Date(), ".tsv")
    },
    
    content = function(file){
      
      d <- filtered() %>%
        select(toponimo, provincia) %>%
        distinct()
      
      write.table(
        d,
        file,
        sep = "\t",
        row.names = FALSE,
        quote = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )
}

shinyApp(ui, server)
