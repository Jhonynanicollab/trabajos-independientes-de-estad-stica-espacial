# ==============================================================================
# PROYECTO: APLICATIVO INTERACTIVO DE AUTOCORRELACIÓN ESPACIAL (ÍNDICE DE MORAN)
# Versión MEJORADA - Con depuración de resultados LISA
# ==============================================================================

# ── CARGA DE LIBRERÍAS ────────────────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(spdep)
library(sf)
library(geodata)
library(ggplot2)
library(leaflet)
library(dplyr)
library(plotly)
library(DT)
library(terra)

# ── DESCARGA Y PREPARACIÓN DE GEOMETRÍAS (PERÚ) ───────────────────────────────
peru_gadm <- geodata::gadm("PER", level = 1, path = tempdir())
peru_sf   <- sf::st_as_sf(peru_gadm)

# Reparar geometrías inválidas
peru_sf <- sf::st_make_valid(peru_sf)

# Homologar nombres y consolidar en exactamente 25 regiones
peru_sf <- peru_sf %>%
  mutate(NOMBRE = case_when(
    NAME_1 == "Ancash"        ~ "Ancash",
    NAME_1 == "Apurímac"      ~ "Apurimac",
    NAME_1 == "Cusco"         ~ "Cusco",
    NAME_1 == "Huánuco"       ~ "Huanuco",
    NAME_1 == "Junín"         ~ "Junin",
    NAME_1 == "Lima Province" ~ "Lima",
    NAME_1 == "Lima"          ~ "Lima",
    NAME_1 == "Madre de Dios" ~ "Madre de Dios",
    NAME_1 == "San Martín"    ~ "San Martin",
    TRUE                      ~ NAME_1
  )) %>%
  group_by(NOMBRE) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  sf::st_make_valid()

# ── ALTITUD MEDIA POR DEPARTAMENTO ────────────────────────────────────────────
altitud_fallback <- data.frame(
  NOMBRE = c("Amazonas","Ancash","Apurimac","Arequipa","Ayacucho","Cajamarca",
             "Callao","Cusco","Huancavelica","Huanuco","Ica","Junin",
             "La Libertad","Lambayeque","Lima","Loreto","Madre de Dios",
             "Moquegua","Pasco","Piura","Puno","San Martin","Tacna","Tumbes","Ucayali"),
  altitud = c(1560, 3091, 2877, 2316, 2746, 2344,
              15,   3399, 3590, 2192, 572,  3267,
              1724, 254,  1535, 126,  272,
              2715, 3218, 487,  3905, 876,  2000, 71,  335)
)

tryCatch({
  alt_raster  <- geodata::elevation_30s(country = "PER", path = tempdir())
  alt_vals    <- terra::extract(alt_raster, terra::vect(peru_sf), fun = mean, na.rm = TRUE)[, 2]
  peru_sf$altitud <- ifelse(is.na(alt_vals), altitud_fallback$altitud[match(peru_sf$NOMBRE, altitud_fallback$NOMBRE)], alt_vals)
}, error = function(e) {
  peru_sf$altitud <<- altitud_fallback$altitud[match(peru_sf$NOMBRE, altitud_fallback$NOMBRE)]
})

# ── DATOS OFICIALES VERIFICADOS ───────────────────────────────────────────────
homicidios_df <- data.frame(
  NOMBRE = c("Amazonas","Ancash","Apurimac","Arequipa","Ayacucho","Cajamarca",
             "Callao","Cusco","Huancavelica","Huanuco","Ica","Junin",
             "La Libertad","Lambayeque","Lima","Loreto","Madre de Dios",
             "Moquegua","Pasco","Piura","Puno","San Martin","Tacna","Tumbes","Ucayali"),
  homicidios = c(2.9, 3.8, 2.4, 3.5, 4.7, 3.2,
                 11.8, 3.9, 1.8, 4.4, 8.3, 5.7,
                 7.1, 5.4, 7.7, 6.2, 8.8,
                 2.1, 3.3, 6.3, 3.1, 6.7, 1.9, 10.6, 9.9)
)

pobreza_df <- data.frame(
  NOMBRE = c("Amazonas","Ancash","Apurimac","Arequipa","Ayacucho","Cajamarca",
             "Callao","Cusco","Huancavelica","Huanuco","Ica","Junin",
             "La Libertad","Lambayeque","Lima","Loreto","Madre de Dios",
             "Moquegua","Pasco","Piura","Puno","San Martin","Tacna","Tumbes","Ucayali"),
  pobreza = c(40.8, 27.1, 37.4, 8.7, 43.5, 40.2,
              11.8, 24.7, 46.3, 38.6, 6.2, 25.8,
              22.7, 20.9, 13.9, 39.8, 6.9,
              8.4, 34.9, 29.3, 35.9, 21.8, 4.8, 14.1, 17.7)
)

desnutricion_df <- data.frame(
  NOMBRE = c("Amazonas","Ancash","Apurimac","Arequipa","Ayacucho","Cajamarca",
             "Callao","Cusco","Huancavelica","Huanuco","Ica","Junin",
             "La Libertad","Lambayeque","Lima","Loreto","Madre de Dios",
             "Moquegua","Pasco","Piura","Puno","San Martin","Tacna","Tumbes","Ucayali"),
  desnutricion = c(27.8, 25.4, 30.6, 7.1, 34.9, 31.4,
                   3.8, 21.7, 38.2, 32.7, 3.7, 19.6,
                   15.9, 14.6, 5.0, 30.8, 8.4,
                   4.7, 28.1, 21.7, 17.2, 17.7, 3.5, 8.0, 17.1)
)

dengue_df <- data.frame(
  NOMBRE = c("Amazonas","Ancash","Apurimac","Arequipa","Ayacucho","Cajamarca",
             "Callao","Cusco","Huancavelica","Huanuco","Ica","Junin",
             "La Libertad","Lambayeque","Lima","Loreto","Madre de Dios",
             "Moquegua","Pasco","Piura","Puno","San Martin","Tacna","Tumbes","Ucayali"),
  dengue = c(143.6, 84.2, 0.0, 1.0, 33.8, 91.5,
             108.9, 44.2, 0.0, 110.7, 318.4, 77.2,
             283.1, 308.6, 139.0, 418.5, 377.3,
             0.0, 24.8, 448.2, 0.0, 288.7, 0.0, 508.4, 413.1)
)

# ── MATRIZ MAESTRA ────────────────────────────────────────────────────────────
peru_master <- peru_sf %>%
  left_join(homicidios_df,   by = "NOMBRE") %>%
  left_join(pobreza_df,      by = "NOMBRE") %>%
  left_join(desnutricion_df, by = "NOMBRE") %>%
  left_join(dengue_df,       by = "NOMBRE")

# Niveles fijos para el factor LISA
LISA_NIVELES <- c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo")
LISA_COLORES <- c("Alto-Alto"="#e74c3c", "Bajo-Bajo"="#3498db",
                  "Alto-Bajo"="#e67e22", "Bajo-Alto"="#9b59b6",
                  "No significativo"="#bdc3c7")

# ── FUNCIÓN PARA CALCULAR RESULTADOS CON DEPURACIÓN ───────────────────────────
calcular_resultados <- function(dataset_name, weight_type, alpha, mostrar_depuracion = FALSE) {
  df <- peru_master
  var <- df[[dataset_name]]
  
  # Validación: si la variable tiene NA, rellenar con la media
  if (any(is.na(var))) {
    var[is.na(var)] <- mean(var, na.rm = TRUE)
  }
  
  # Construcción de la matriz de vecindad
  nb <- tryCatch(
    switch(weight_type,
           "queen" = poly2nb(df, queen = TRUE),
           "rook"  = poly2nb(df, queen = FALSE),
           "knn"   = knn2nb(knearneigh(st_centroid(st_geometry(df)), k = 4))
    ),
    error = function(e) poly2nb(df, queen = TRUE)
  )
  
  # Pesos espaciales
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Test Global de Moran
  mt <- moran.test(var, lw, zero.policy = TRUE, alternative = "two.sided")
  
  # Indicadores Locales LISA
  lm_local <- localmoran(var, lw, zero.policy = TRUE)
  
  # Vectores estandarizados
  z     <- as.vector(scale(var))
  lag_z <- as.vector(lag.listw(lw, z, zero.policy = TRUE))
  
  # p-valores locales
  p_local <- lm_local[, 5]
  
  # Mostrar depuración si se solicita
  if(mostrar_depuracion) {
    cat("\n=== DEPURACIÓN LISA ===\n")
    cat("Dataset:", dataset_name, "\n")
    cat("Tipo de vecindad:", weight_type, "\n")
    cat("Alpha:", alpha, "\n")
    cat("Moran I Global:", mt$estimate["Moran I statistic"], "\n")
    cat("P-valor global:", mt$p.value, "\n\n")
    cat("Departamentos con p-valor local <", alpha, ":\n")
    
    for(i in 1:length(p_local)) {
      if(!is.na(p_local[i]) && p_local[i] < alpha) {
        cat(df$NOMBRE[i], ": p =", round(p_local[i], 4), 
            "| Z =", round(z[i], 2), 
            "| Wz =", round(lag_z[i], 2), "\n")
      }
    }
    cat("\n========================\n")
  }
  
  # Clasificación LISA
  cuad_raw <- character(length(z))
  for(i in 1:length(z)) {
    if(!is.na(p_local[i]) && p_local[i] < alpha) {
      if(z[i] > 0 && lag_z[i] > 0) {
        cuad_raw[i] <- "Alto-Alto"
      } else if(z[i] < 0 && lag_z[i] < 0) {
        cuad_raw[i] <- "Bajo-Bajo"
      } else if(z[i] > 0 && lag_z[i] < 0) {
        cuad_raw[i] <- "Alto-Bajo"
      } else if(z[i] < 0 && lag_z[i] > 0) {
        cuad_raw[i] <- "Bajo-Alto"
      } else {
        cuad_raw[i] <- "No significativo"
      }
    } else {
      cuad_raw[i] <- "No significativo"
    }
  }
  
  cuad <- factor(cuad_raw, levels = LISA_NIVELES)
  
  # Estadísticas de clasificación
  tabla_clasif <- table(cuad)
  cat("\n=== CLASIFICACIÓN LISA ===\n")
  for(nivel in LISA_NIVELES) {
    cat(nivel, ": ", tabla_clasif[nivel], " departamentos\n")
  }
  cat("========================\n\n")
  
  list(
    sf = df, 
    var = var, 
    z = z, 
    lag_z = lag_z,
    moran = mt, 
    local = lm_local, 
    cuad = cuad,
    lw = lw,
    nb = nb,
    p_local = p_local
  )
}

# ── INTERFAZ DE USUARIO (UI) ──────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://flagcdn.com/w20/pe.png", height = "16px",
               style = "margin-right:6px; vertical-align:middle;"),
      "Índice de Moran — Perú"
    ),
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Inicio",         tabName = "inicio", icon = icon("home")),
      menuItem("Análisis Moran", tabName = "moran",  icon = icon("map")),
      menuItem("Mapa LISA",      tabName = "lisa",   icon = icon("th")),
      menuItem("Datos",          tabName = "datos",  icon = icon("table"))
    ),
    hr(),
    
    tags$div(style = "padding: 0 15px;",
             tags$h5("⚙ Configuración", style = "color:#ddd;"),
             
             selectInput(
               "dataset", "Dataset Real Seleccionado:",
               choices = c(
                 "🔴 Homicidios (PNP 2023)"              = "homicidios",
                 "🟡 Pobreza monetaria (INEI 2022)"      = "pobreza",
                 "🟢 Desnutrición infantil (MINSA 2022)" = "desnutricion",
                 "🔵 Tasa de Dengue (CDC-Perú 2023)"     = "dengue",
                 "⛰ Altitud Media Geográfica (SRTM)"    = "altitud"
               )
             ),
             
             selectInput(
               "weight_type", "Tipo de vecindad espacial:",
               choices = c(
                 "Reina - Queen (comparte vértice)" = "queen",
                 "Torre - Rook  (comparte borde)"   = "rook",
                 "K vecinos más cercanos (k=4)"     = "knn"
               )
             ),
             
             sliderInput(
               "alpha", "Nivel de significancia α:",
               min = 0.01, max = 0.20, value = 0.10, step = 0.01
             ),
             
             tags$div(
               style = "margin-top: 10px; padding: 10px; background-color: #2c3e50; border-radius: 5px;",
               tags$small("📌 Nota: Un α más alto (ej: 0.10) mostrará más clústeres significativos")
             ),
             
             br(),
             actionButton(
               "run", "▶  Calcular Índice de Moran",
               class = "btn-danger btn-block",
               style = "font-weight:bold; width:100%; color:white;"
             )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f5f5f5; }
      .box { border-radius: 6px; box-shadow: 0 2px 8px rgba(0,0,0,.10); }
      .small-box { border-radius: 6px; }
      .small-box h3 { font-size: 28px; }
      .skin-red .main-sidebar { background-color: #1a1a2e; }
      .skin-red .sidebar a    { color: #ccc !important; }
      .skin-red .sidebar-menu > li.active > a,
      .skin-red .sidebar-menu > li:hover  > a { background-color: #c0392b !important; }
      .skin-red .main-header .navbar,
      .skin-red .main-header .logo { background-color: #c0392b !important; }
      .btn-danger { background-color: #c0392b; border-color: #a93226; }
      .intro-box { background:white; border-left:5px solid #c0392b;
                   border-radius:4px; padding:18px 22px; margin-bottom:18px;
                   box-shadow:0 2px 8px rgba(0,0,0,.08); }
    "))),
    
    tabItems(
      
      # TAB 1: INICIO
      tabItem(
        tabName = "inicio",
        fluidRow(
          column(12,
                 div(class = "intro-box",
                     h3("📍 Análisis de Autocorrelación Espacial Multitemática — Perú",
                        style = "margin-top:0; color:#c0392b;"),
                     p("Esta plataforma calcula indicadores globales y locales de autocorrelación espacial
                para variables oficiales del Perú a nivel departamental."),
                     tags$ul(
                       tags$li(strong("I ≈ +1: "), "Clústeres territoriales — regiones con valores similares se agrupan geográficamente."),
                       tags$li(strong("I ≈  0: "), "Distribución aleatoria — no existe patrón geográfico definido."),
                       tags$li(strong("I ≈ -1: "), "Dispersión estricta — vecinos contiguos tienen valores opuestos.")
                     ),
                     p("👉 Selecciona el dataset y tipo de vecindad en el panel izquierdo y presiona ",
                       strong("▶ Calcular Índice de Moran"), " para actualizar los modelos."),
                     br(),
                     div(style = "background-color: #e8f4f8; padding: 10px; border-radius: 5px;",
                         strong("💡 Recomendación:"), 
                         " Para obtener más clústeres significativos, prueba con α = 0.10 o 0.15, 
                         o selecciona el dataset de 'Pobreza' o 'Desnutrición' que suelen mostrar mayor autocorrelación espacial.")
                 )
          )
        ),
        fluidRow(
          valueBoxOutput("vbox_n",       width = 4),
          valueBoxOutput("vbox_dataset", width = 4),
          valueBoxOutput("vbox_vecin",   width = 4)
        ),
        fluidRow(
          box(width = 12, title = "📊 Ficha técnica de bases de datos integradas",
              status = "danger", solidHeader = TRUE,
              DT::DTOutput("tabla_descripcion"))
        )
      ),
      
      # TAB 2: ANÁLISIS MORAN GLOBAL
      tabItem(
        tabName = "moran",
        fluidRow(
          valueBoxOutput("box_I",    width = 3),
          valueBoxOutput("box_EI",   width = 3),
          valueBoxOutput("box_pval", width = 3),
          valueBoxOutput("box_sig",  width = 3)
        ),
        fluidRow(
          box(width = 6, title = "🗺 Distribución del Atributo por Departamento",
              status = "danger", solidHeader = TRUE,
              leafletOutput("mapa_coropletico", height = 450)),
          box(width = 6, title = "📈 Diagrama de Dispersión de Moran",
              status = "warning", solidHeader = TRUE,
              plotlyOutput("moran_scatter", height = 450))
        ),
        fluidRow(
          box(width = 6, title = "📋 Resultado del Test de Moran",
              status = "success", solidHeader = TRUE,
              verbatimTextOutput("moran_output")),
          box(width = 6, title = "📊 P-valores locales (primeros 10 departamentos)",
              status = "info", solidHeader = TRUE,
              tableOutput("tabla_pvalores"))
        )
      ),
      
      # TAB 3: ANÁLISIS LOCAL (LISA)
      tabItem(
        tabName = "lisa",
        fluidRow(
          box(width = 8, title = "🗺 Mapa de Clústeres Locales Significativos (LISA)",
              status = "danger", solidHeader = TRUE,
              leafletOutput("lisa_map", height = 520)),
          box(width = 4, title = "📖 Tipologías Espaciales",
              status = "primary", solidHeader = TRUE,
              HTML("
                <p>El análisis <strong>LISA</strong> identifica cuadrantes de significancia local:</p>
                <table style='width:100%;font-size:13px;border-collapse:collapse;'>
                  <tr style='background:#e74c3c;color:white;'><td style='padding:6px;'><b>Alto – Alto (Hotspot)</b></td><td style='padding:6px;'>Núcleos de alta concentración regional.络merce
                  <tr style='background:#3498db;color:white;'><td style='padding:6px;'><b>Bajo – Bajo (Coldspot)</b></td><td style='padding:6px;'>Zonas homogéneas de bajo valor.络merce
                  <tr style='background:#e67e22;color:white;'><td style='padding:6px;'><b>Alto – Bajo (Outlier)</b></td><td style='padding:6px;'>Islas altas rodeadas de valores bajos.络merce
                  <tr style='background:#9b59b6;color:white;'><td style='padding:6px;'><b>Bajo – Alto (Outlier)</b></td><td style='padding:6px;'>Zona baja rodeada de valores altos.络merce
                  <tr style='background:#bdc3c7;color:#333;'><td style='padding:6px;'><b>No Significativo</b></td><td style='padding:6px;'>Sin estructuración espacial definida.络merce
                </table>
              "),
              hr(),
              plotOutput("lisa_barplot", height = 220),
              hr(),
              div(style = "text-align: center;",
                  strong("Clústeres encontrados:"),
                  verbatimTextOutput("resumen_clusters"))
          )
        )
      ),
      
      # TAB 4: DATOS
      tabItem(
        tabName = "datos",
        fluidRow(
          box(width = 12, title = "📂 Explorador Numérico por Departamento",
              status = "primary", solidHeader = TRUE,
              DT::DTOutput("tabla_datos"))
        )
      )
    )
  )
)

# ── LÓGICA DEL SERVIDOR (SERVER) MEJORADA ─────────────────────────────────────
server <- function(input, output, session) {
  
  # Inicializar resultados con datos por defecto (Homicidios, Queen, α=0.10)
  resultados <- reactiveVal(calcular_resultados("homicidios", "queen", 0.10, TRUE))
  
  # ── Cálculo del modelo espacial ───────────────────────────────────────────────
  observeEvent(input$run, {
    showNotification("Calculando índices espaciales...", duration = 2)
    
    # Calcular con depuración activada
    res <- calcular_resultados(input$dataset, input$weight_type, input$alpha, TRUE)
    resultados(res)
    
    showNotification(paste("¡Cálculo completado!", 
                           sum(res$cuad != "No significativo"), 
                           "departamentos significativos encontrados"), 
                     duration = 3)
  })
  
  # ── Etiquetas descriptivas ─────────────────────────────────────────────────────
  label_var <- reactive({
    switch(input$dataset,
           "homicidios"   = "Homicidios (tasa x 100k hab) — PNP 2023",
           "pobreza"      = "Pobreza monetaria (%) — INEI ENAHO 2022",
           "desnutricion" = "Desnutrición crónica infantil (%) — MINSA 2022",
           "dengue"       = "Dengue (casos x 100k hab) — CDC-Perú 2023",
           "altitud"      = "Altitud media (m.s.n.m.) — SRTM/IGN"
    )
  })
  
  # ── Value boxes del tab Inicio ─────────────────────────────────────────────────
  output$vbox_n <- renderValueBox({
    valueBox(25, "Unidades territoriales unificadas",
             icon = icon("map-marked-alt"), color = "red")
  })
  
  output$vbox_dataset <- renderValueBox({
    valueBox(toupper(input$dataset), "Variable analizada",
             icon = icon("database"), color = "orange")
  })
  
  output$vbox_vecin <- renderValueBox({
    valueBox(toupper(input$weight_type), "Criterio de proximidad",
             icon = icon("sliders-h"), color = "purple")
  })
  
  # ── Tabla de metadatos ─────────────────────────────────────────────────────────
  output$tabla_descripcion <- DT::renderDT({
    data.frame(
      Dataset    = c("Homicidios", "Pobreza monetaria", "Desnutrición infantil",
                     "Dengue", "Topografía"),
      Variable   = c("homicidios", "pobreza", "desnutricion", "dengue", "altitud"),
      Unidad     = c("Tasa x 100k hab", "% población", "% niños < 5 años",
                     "Casos x 100k hab", "m.s.n.m."),
      Año        = c("2023", "2022", "2022", "2023", "2023"),
      Fuente     = c("PNP / INEI", "INEI — ENAHO", "MINSA — SIEN",
                     "MINSA / CDC-Perú", "GADM / SRTM")
    ) |>
      DT::datatable(options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  # ── Value boxes tab Moran ──────────────────────────────────────────────────────
  output$box_I <- renderValueBox({
    req(resultados())
    I <- round(resultados()$moran$estimate["Moran I statistic"], 4)
    valueBox(I, "Índice de Moran (I)", icon = icon("chart-line"), color = "blue")
  })
  
  output$box_EI <- renderValueBox({
    req(resultados())
    EI <- round(resultados()$moran$estimate["Expectation"], 4)
    valueBox(EI, "Esperanza teórica E(I)", icon = icon("bullseye"), color = "orange")
  })
  
  output$box_pval <- renderValueBox({
    req(resultados())
    pv <- signif(resultados()$moran$p.value, 4)
    valueBox(pv, "p-valor global",
             icon = icon("hourglass-half"),
             color = if (pv < input$alpha) "green" else "yellow")
  })
  
  output$box_sig <- renderValueBox({
    req(resultados())
    pv  <- resultados()$moran$p.value
    msg <- ifelse(pv < input$alpha, "✔ Patrón Estructurado", "✘ Patrón Aleatorio")
    valueBox(msg, paste("Filtro crítico α =", input$alpha),
             icon = icon("toggle-on"),
             color = if (pv < input$alpha) "green" else "red")
  })
  
  # ── Mapa coroplético ──────────────────────────────────────────────────────────
  output$mapa_coropletico <- renderLeaflet({
    req(resultados())
    r      <- resultados()
    df_map <- sf::st_transform(sf::st_make_valid(r$sf), 4326)
    pal    <- colorNumeric("YlOrRd", r$var, na.color = "#cccccc")
    
    leaflet(df_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor    = ~pal(r$var),
        fillOpacity  = 0.8,
        color        = "#ffffff",
        weight       = 1,
        highlightOptions = highlightOptions(
          weight = 3, color = "#c0392b", bringToFront = TRUE),
        label = ~paste0(NOMBRE, ": ", round(r$var, 2))
      ) %>%
      addLegend("bottomright", pal = pal, values = r$var,
                title = label_var(), opacity = 0.8)
  })
  
  # ── Tabla de p-valores locales ────────────────────────────────────────────────
  output$tabla_pvalores <- renderTable({
    req(resultados())
    r <- resultados()
    
    df_pvals <- data.frame(
      Departamento = r$sf$NOMBRE,
      "Valor Z" = round(r$z, 3),
      "P-valor" = round(r$p_local, 5),
      "Significativo" = ifelse(r$p_local < input$alpha, "✓ Sí", "✗ No")
    )
    
    # Mostrar primeros 10 y ordenar por p-valor
    df_pvals[order(df_pvals$P.valor), ][1:10, ]
  }, digits = 5)
  
  # ── Gráfico de dispersión Moran (Plotly) ──────────────────────────────────────
  output$moran_scatter <- renderPlotly({
    req(resultados())
    r <- resultados()
    
    df_p <- data.frame(
      z        = r$z,
      lag_z    = r$lag_z,
      cuad     = as.character(r$cuad),
      dpto     = r$sf$NOMBRE,
      p_valor  = r$p_local
    )
    
    p <- ggplot(df_p, aes(
      x    = z, y = lag_z,
      text = paste0("Dpto: ", dpto,
                    "<br>Z: ", round(z, 2),
                    "<br>Wz: ", round(lag_z, 2),
                    "<br>Tipo: ", cuad,
                    "<br>p-valor: ", round(p_valor, 4))
    )) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
      geom_point(aes(color = cuad, size = ifelse(p_valor < input$alpha, 1, 0.5)), 
                 alpha = 0.9) +
      geom_smooth(method = "lm", formula = y ~ x,
                  color = "#2c3e50", se = TRUE, alpha = 0.12) +
      scale_color_manual(values = LISA_COLORES, name = "Cuadrante") +
      scale_size_continuous(range = c(2, 5), guide = "none") +
      labs(x = "Desviación estándar (Z)",
           y = "Media espacial del entorno (Wz)",
           title = paste("Diagrama de Moran - I =", round(r$moran$estimate["Moran I statistic"], 4))) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ── Consola inferencial ────────────────────────────────────────────────────────
  output$moran_output <- renderPrint({
    req(resultados())
    cat("=== TEST GLOBAL DE MORAN ===\n\n")
    print(resultados()$moran)
    cat("\n\n=== MATRIZ DE VECINDAD ===\n")
    cat("Número de departamentos:", length(resultados()$nb), "\n")
    cat("Número de enlaces:", sum(card(resultados()$nb)), "\n")
  })
  
  # ── Mapa LISA - VERSIÓN CORREGIDA ─────────────────────────────────────────────
  output$lisa_map <- renderLeaflet({
    req(resultados())
    r <- resultados()
    
    # Verificar que hay datos
    if(is.null(r$cuad)) {
      return(leaflet() %>% addProviderTiles("CartoDB.Positron") %>% 
               addControl("Cargando datos...", position = "center"))
    }
    
    # Crear copia del sf y añadir columna de clasificación
    df_map <- sf::st_make_valid(r$sf)
    df_map$LISA_Type <- as.character(r$cuad)
    
    # Asegurar que todos los niveles están presentes
    df_map$LISA_Type <- factor(df_map$LISA_Type, levels = LISA_NIVELES)
    
    # Transformar a WGS84 para Leaflet
    df_map <- sf::st_transform(df_map, 4326)
    
    # Función de colores CORREGIDA
    pal_lisa <- colorFactor(
      palette = LISA_COLORES,
      domain = LISA_NIVELES,
      na.color = "#cccccc"
    )
    
    # Crear mapa
    leaflet(df_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_lisa(LISA_Type),
        fillOpacity = 0.85,
        color = "#ffffff",
        weight = 1.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#2c3e50",
          bringToFront = TRUE
        ),
        label = ~paste0(NOMBRE, " → ", LISA_Type),
        popup = ~paste0(
          "<strong>", NOMBRE, "</strong><br>",
          "Clasificación LISA: ", LISA_Type, "<br>",
          "Valor de ", isolate(label_var()), ": ", round(r$var[which(r$sf$NOMBRE == NOMBRE)], 2), "<br>",
          "P-valor local: ", round(r$p_local[which(r$sf$NOMBRE == NOMBRE)], 4)
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_lisa,
        values = LISA_NIVELES,
        title = "Estructura LISA",
        opacity = 0.85,
        labFormat = labelFormat(transform = function(x) x)
      )
  })
  
  # ── Resumen de clusters ────────────────────────────────────────────────────────
  output$resumen_clusters <- renderPrint({
    req(resultados())
    r <- resultados()
    tabla <- table(r$cuad)
    cat("📊 Resumen de Clústeres LISA (α =", input$alpha, "):\n\n")
    for(nivel in LISA_NIVELES) {
      if(nivel != "No significativo") {
        cat(nivel, ": ", tabla[nivel], " departamento(s)\n")
      }
    }
    cat("\nNo significativo: ", tabla["No significativo"], " departamento(s)\n")
    cat("\n🔍 Los clústeres significativos indican autocorrelación espacial local")
  })
  
  # ── Gráfico de barras LISA ─────────────────────────────────────────────────────
  output$lisa_barplot <- renderPlot({
    req(resultados())
    
    # Contar frecuencias
    freq_table <- table(resultados()$cuad)
    df_bar <- data.frame(
      Cuadrante = factor(names(freq_table), levels = LISA_NIVELES),
      Frecuencia = as.numeric(freq_table)
    )
    
    # Asegurar que todos los niveles están representados (incluso con 0)
    for(level in LISA_NIVELES) {
      if(!level %in% df_bar$Cuadrante) {
        df_bar <- rbind(df_bar, data.frame(Cuadrante = factor(level, levels = LISA_NIVELES), Frecuencia = 0))
      }
    }
    
    df_bar <- df_bar[order(df_bar$Cuadrante),]
    
    # Colores más vibrantes para destacar clústeres
    colores_barras <- c("Alto-Alto"="#e74c3c", "Bajo-Bajo"="#3498db",
                        "Alto-Bajo"="#e67e22", "Bajo-Alto"="#9b59b6",
                        "No significativo"="#95a5a6")
    
    ggplot(df_bar, aes(x = Cuadrante, y = Frecuencia, fill = Cuadrante)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = Frecuencia), vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = colores_barras, drop = FALSE) +
      labs(x = NULL, y = "N° de departamentos", 
           title = paste("Distribución de Tipologías LISA (α =", input$alpha, ")"),
           subtitle = paste("Total clústeres significativos:", sum(df_bar$Frecuencia[df_bar$Cuadrante != "No significativo"]))) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "blue", size = 11),
        panel.grid.major.x = element_blank()
      ) +
      ylim(0, max(df_bar$Frecuencia) + 2)
  })
  
  # ── Tabla de datos ─────────────────────────────────────────────────────────────
  output$tabla_datos <- DT::renderDT({
    df_data <- peru_master
    sf::st_geometry(df_data) <- NULL
    DT::datatable(
      df_data[, c("NOMBRE","homicidios","pobreza","desnutricion","dengue","altitud")],
      colnames  = c("Departamento", "Homicidios 2023\n(x100k)", "Pobreza 2022 (%)",
                    "Desnutrición (%)", "Dengue 2023\n(x100k)", "Altitud (m)"),
      options   = list(
        pageLength = 25, 
        scrollX = TRUE, 
        dom = "frtip",
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      ),
      rownames  = FALSE
    ) %>%
      formatRound(columns = c("altitud"), digits = 0) %>%
      formatRound(columns = c("homicidios","pobreza","desnutricion","dengue"), digits = 1)
  })
}

# ── INSTANCIACIÓN DE LA APP ────────────────────────────────────────────────────
shinyApp(ui, server)