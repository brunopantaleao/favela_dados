# 08_plataforma_insights.R ------------------------------------------------
# Business Insights Platform — Favelas BR
#
# Tabs:
#   1. Mapa          — map coloured by any indicator, filtered by geo level
#   2. Rankings      — sortable table of FCUs with any indicator
#   3. Comparações   — bar charts comparing means across geo groups
#   4. Dados         — filtered table with CSV download
#
# Inputs (produced by scripts 06 + 07):
#   FAVELAS_IDS_CSV   — FCU-level indicators (no geometry)
#   FAVELAS_IDS_SHP   — FCU polygons with indicators
#   FAVELAS_RISCOS_CSV — FCU-level hazard flags

source("config.R")

# Packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(readr)
library(leaflet)
library(DT)
library(ggplot2)
library(forcats)
library(viridisLite)
library(scales)

# =========================================================================
# DATA LOADING
# Geometry: fetched from GitHub-hosted GeoJSON (produced by script 11)
#   — already simplified, already includes all indicator columns
#   — already includes tem_risco from script 06
# Tabular: read from local CSV for tables, rankings, and downloads
# =========================================================================

# sf object — fetch from GitHub (cached in memory for session duration)
# Update this URL after uploading favelas_br_simplified.geojson to GitHub
GEOJSON_URL <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_br_simplified.geojson"

message("Fetching GeoJSON from GitHub...")
fav_sf <- st_read(GEOJSON_URL, quiet = TRUE) %>%
  st_make_valid() %>%
  filter(!st_is_empty(geometry))
message("  Loaded: ", nrow(fav_sf), " FCUs")

# Tabular data — read locally (fast, no geometry overhead)
fav_df <- read_csv2(FAVELAS_IDS_CSV, show_col_types = FALSE)

riscos <- read_csv2(FAVELAS_RISCOS_CSV, show_col_types = FALSE) %>%
  mutate(cd_fcu = as.character(cd_fcu))

# Join hazard flags onto tabular data
fav_df <- fav_df %>%
  mutate(cd_fcu = as.character(cd_fcu)) %>%
  left_join(riscos %>% select(cd_fcu, tem_risco), by = "cd_fcu")

# fav_sf already has tem_risco baked in from script 11 export
fav_sf <- fav_sf %>%
  mutate(CD_FCU = as.character(CD_FCU))

# =========================================================================
# INDICATOR CATALOGUE
# Defines every selectable indicator: column name, display label, direction
# (higher = better for colouring), and group for the dropdown.
# =========================================================================
indicadores <- list(
  
  # — Composite indices
  list(col = "IDS",  label = "IDS — Índice de Desenvolvimento Social",   dir = +1, group = "Índices"),
  list(col = "IDA",  label = "IDA — Índice de Acessibilidade Urbana",    dir = +1, group = "Índices"),
  
  # — Sanitation
  list(col = "PERC_AGUA",  label = "Água encanada (%)",       dir = +1, group = "Saneamento"),
  list(col = "PERC_ESGO",  label = "Esgoto rede geral (%)",   dir = +1, group = "Saneamento"),
  list(col = "PERC_LIXO",  label = "Coleta de lixo (%)",      dir = +1, group = "Saneamento"),
  
  # — Income & Education
  list(col = "RENDA_SM",   label = "Renda média (sal. mín.)", dir = +1, group = "Renda e Educação"),
  list(col = "PERC_ANALF", label = "Analfabetismo 15+ (%)",   dir = -1, group = "Renda e Educação"),
  
  # — Urban accessibility (IDA components)
  list(col = "P_VIAPAV",  label = "Via pavimentada (%)",        dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_BUEIRO",  label = "Bueiro / boca de lobo (%)",  dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ILUM",    label = "Iluminação pública (%)",     dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ONTON",   label = "Ponto de ônibus (%)",        dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_VIABIC",  label = "Ciclovia / ciclofaixa (%)",  dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_CALCAD",  label = "Calçada (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_OBSTAC",  label = "Obstáculo na calçada (%)",   dir = -1, group = "Acessibilidade Urbana"),
  list(col = "P_RAMPA",   label = "Rampa para cadeirante (%)",  dir = +1, group = "Acessibilidade Urbana"),
  
  # — Hazards
  list(col = "tem_risco",  label = "Exposição a risco (flag)",  dir = -1, group = "Riscos Naturais")
)

# Build named vector for selectInput (grouped)
ind_choices <- setNames(
  sapply(indicadores, `[[`, "col"),
  sapply(indicadores, `[[`, "label")
)
ind_groups  <- sapply(indicadores, `[[`, "group")
ind_grouped <- split(ind_choices, ind_groups)

ind_label <- setNames(
  sapply(indicadores, `[[`, "label"),
  sapply(indicadores, `[[`, "col")
)

# =========================================================================
# GEO HIERARCHIES (for filters)
# =========================================================================
regioes  <- sort(unique(na.omit(fav_df$nm_uf)))   # UF as proxy until NM_REGIAO available
ufs      <- sort(unique(na.omit(fav_df$nm_uf)))
municipios <- sort(unique(na.omit(fav_df$nm_mun)))

# =========================================================================
# HELPERS
# =========================================================================
fmt_pct   <- function(x) paste0(round(x, 1), "%")
fmt_num   <- function(x) formatC(x, format = "f", digits = 2, big.mark = ".")
fmt_int   <- function(x) formatC(x, format = "d", big.mark = ".")

# Filter fav_df by sidebar selections
filter_data <- function(df, uf, mun) {
  if (!is.null(uf)  && length(uf)  > 0 && uf  != "")
    df <- df %>% filter(nm_uf  %in% uf)
  if (!is.null(mun) && length(mun) > 0 && mun != "")
    df <- df %>% filter(nm_mun %in% mun)
  df
}

filter_sf <- function(sf_obj, uf, mun) {
  if (!is.null(uf)  && length(uf)  > 0 && uf  != "")
    sf_obj <- sf_obj %>% filter(NM_UF  %in% uf)
  if (!is.null(mun) && length(mun) > 0 && mun != "")
    sf_obj <- sf_obj %>% filter(NM_MUN %in% mun)
  sf_obj
}

# =========================================================================
# UI
# =========================================================================
ui <- page_navbar(
  title = "Favelas BR — Plataforma de Análise",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  fillable = TRUE,
  
  # -----------------------------------------------------------------------
  # Sidebar (shared across tabs via nav_panel layout)
  # -----------------------------------------------------------------------
  sidebar = sidebar(
    width = 260,
    title = "Filtros",
    
    selectInput("sel_uf", "Estado (UF)",
                choices  = c("Todos" = "", ufs),
                selected = "",
                multiple = TRUE
    ),
    
    selectInput("sel_mun", "Município",
                choices  = c("Todos" = "", municipios),
                selected = "",
                multiple = TRUE
    ),
    
    hr(),
    
    selectInput("sel_ind", "Indicador",
                choices  = ind_grouped,
                selected = "IDS"
    ),
    
    hr(),
    p(tags$small(tags$i(
      "Dados: Censo IBGE 2022 · IBGE FCU 2022 · AOP · SGB"
    )))
  ),
  
  # -----------------------------------------------------------------------
  # Tab 1 — Mapa
  # -----------------------------------------------------------------------
  nav_panel(
    title = tagList(icon("map"), " Mapa"),
    card(
      full_screen = TRUE,
      card_header(textOutput("mapa_titulo")),
      uiOutput("mapa_ui")
    )
  ),
  
  # -----------------------------------------------------------------------
  # Tab 2 — Rankings
  # -----------------------------------------------------------------------
  nav_panel(
    title = tagList(icon("trophy"), " Rankings"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header(textOutput("ranking_titulo")),
        card_body(
          DTOutput("tabela_ranking")
        )
      )
    )
  ),
  
  # -----------------------------------------------------------------------
  # Tab 3 — Comparações
  # -----------------------------------------------------------------------
  nav_panel(
    title = tagList(icon("chart-bar"), " Comparações"),
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Média por Estado (UF)"),
        card_body(plotOutput("chart_uf", height = "420px"))
      ),
      
      card(
        card_header("Média por Município (top 20)"),
        card_body(plotOutput("chart_mun", height = "420px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Distribuição do indicador selecionado"),
        card_body(plotOutput("chart_hist", height = "280px"))
      )
    )
  ),
  
  # -----------------------------------------------------------------------
  # Tab 4 — Dados
  # -----------------------------------------------------------------------
  nav_panel(
    title = tagList(icon("table"), " Dados"),
    card(
      card_header(
        layout_columns(
          col_widths = c(8, 4),
          textOutput("dados_titulo"),
          downloadButton("download_csv", "Baixar CSV", class = "btn-sm btn-success")
        )
      ),
      card_body(
        DTOutput("tabela_dados")
      )
    )
  )
)

# =========================================================================
# SERVER
# =========================================================================
server <- function(input, output, session) {
  
  # -----------------------------------------------------------------------
  # Reactive: update municipality choices when UF changes
  # -----------------------------------------------------------------------
  observeEvent(input$sel_uf, {
    if (length(input$sel_uf) > 0 && input$sel_uf != "") {
      muns_filtrados <- fav_df %>%
        filter(nm_uf %in% input$sel_uf) %>%
        pull(nm_mun) %>% unique() %>% sort()
    } else {
      muns_filtrados <- municipios
    }
    updateSelectInput(session, "sel_mun",
                      choices  = c("Todos" = "", muns_filtrados),
                      selected = input$sel_mun[input$sel_mun %in% muns_filtrados]
    )
  })
  
  # -----------------------------------------------------------------------
  # Reactive datasets
  # -----------------------------------------------------------------------
  dados_filtrados <- reactive({
    filter_data(fav_df, input$sel_uf, input$sel_mun)
  })
  
  sf_filtrado <- reactive({
    filter_sf(fav_sf, input$sel_uf, input$sel_mun)
  })
  
  ind_col   <- reactive({ input$sel_ind })
  ind_nome  <- reactive({ ind_label[[input$sel_ind]] })
  
  # -----------------------------------------------------------------------
  # Tab 1 — Mapa
  # -----------------------------------------------------------------------
  output$mapa_titulo <- renderText({
    if (length(input$sel_uf) == 0 || all(input$sel_uf == "")) {
      "Mapa — selecione um Estado para visualizar"
    } else {
      paste0("Mapa — ", ind_nome(),
             " | ", nrow(dados_filtrados()), " favelas")
    }
  })
  
  # Render either a placeholder or the map depending on UF selection
  output$mapa_ui <- renderUI({
    if (length(input$sel_uf) == 0 || all(input$sel_uf == "")) {
      div(
        style = paste(
          "height: 680px; display: flex; flex-direction: column;",
          "align-items: center; justify-content: center;",
          "background: #f8f9fa; border-radius: 6px; color: #6c757d;"
        ),
        tags$i(class = "fa fa-map fa-3x", style = "margin-bottom: 16px; color: #adb5bd;"),
        tags$h5("Selecione um Estado para carregar o mapa",
                style = "font-weight: 500; margin-bottom: 8px;"),
        tags$p("Use o filtro 'Estado (UF)' na barra lateral.",
               style = "font-size: 0.9rem;")
      )
    } else {
      leafletOutput("mapa", width = "100%", height = "680px")
    }
  })
  
  output$mapa <- renderLeaflet({
    req(length(input$sel_uf) > 0, !all(input$sel_uf == ""))
    
    sf_obj <- sf_filtrado()
    col    <- ind_col()
    
    vals <- sf_obj[[col]]
    
    pal <- colorNumeric(
      palette  = viridis(100),
      domain   = vals,
      na.color = "#CCCCCC",
      reverse  = FALSE
    )
    
    hover <- sprintf(
      "<b>%s</b><br>%s: <b>%s</b><br>Município: %s<br>UF: %s<br>Pop.: %s",
      sf_obj$NM_FCU,
      ind_nome(),
      ifelse(is.na(vals), "—", round(vals, 3)),
      sf_obj$NM_MUN,
      sf_obj$NM_UF,
      formatC(sf_obj$TOT_PES, format = "d", big.mark = ".")
    )
    
    leaflet(sf_obj) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~pal(vals),
        fillOpacity = 0.8,
        weight      = 0.8,
        color       = "white",
        label       = lapply(hover, HTML),
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#FFD700",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal    = pal,
        values = vals,
        title  = ind_nome(),
        opacity = 0.9,
        position = "bottomright"
      )
  })
  
  # -----------------------------------------------------------------------
  # Tab 2 — Rankings
  # -----------------------------------------------------------------------
  output$ranking_titulo <- renderText({
    paste0("Ranking por ", ind_nome(),
           " — ", nrow(dados_filtrados()), " favelas")
  })
  
  output$tabela_ranking <- renderDT({
    col <- ind_col()
    df  <- dados_filtrados() %>%
      select(
        Favela    = nm_fcu,
        Município = nm_mun,
        UF        = nm_uf,
        IDS, IDA,
        Indicador = any_of(col),
        População = total_pessoas,
        Renda_SM  = renda_sm_pond
      ) %>%
      arrange(desc(.data[[ifelse(col %in% names(.), col, "IDS")]]))
    
    datatable(
      df,
      rownames  = FALSE,
      filter    = "top",
      extensions = "Buttons",
      options   = list(
        pageLength = 20,
        dom        = "Bfrtip",
        buttons    = list(),   # CSV download handled by Tab 4
        scrollX    = TRUE,
        columnDefs = list(list(className = "dt-right",
                               targets   = 4:ncol(df) - 1))
      )
    ) %>%
      formatRound(columns = c("IDS", "IDA", "Renda_SM"), digits = 3) %>%
      formatRound(columns = intersect(c("Indicador"), names(df)), digits = 2) %>%
      formatCurrency(columns = "População", currency = "", interval = 3,
                     mark = ".", digits = 0)
  })
  
  # -----------------------------------------------------------------------
  # Tab 3 — Comparações
  # -----------------------------------------------------------------------
  chart_theme <- theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.x        = element_text(size = 9),
      plot.title         = element_blank()
    )
  
  output$chart_uf <- renderPlot({
    col  <- ind_col()
    nome <- ind_nome()
    df   <- dados_filtrados()
    
    if (!col %in% names(df)) return(NULL)
    
    df %>%
      group_by(UF = nm_uf) %>%
      summarise(media = mean(.data[[col]], na.rm = TRUE),
                n     = n(), .groups = "drop") %>%
      filter(!is.na(UF)) %>%
      mutate(UF = fct_reorder(UF, media)) %>%
      ggplot(aes(x = media, y = UF, fill = media)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(media, 2)),
                hjust = -0.15, size = 3.2) +
      scale_fill_viridis_c(option = "D") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = nome, y = NULL) +
      chart_theme
  })
  
  output$chart_mun <- renderPlot({
    col  <- ind_col()
    nome <- ind_nome()
    df   <- dados_filtrados()
    
    if (!col %in% names(df)) return(NULL)
    
    df %>%
      group_by(Município = nm_mun) %>%
      summarise(media = mean(.data[[col]], na.rm = TRUE),
                n     = n(), .groups = "drop") %>%
      filter(!is.na(Município)) %>%
      slice_max(order_by = n, n = 20) %>%
      mutate(Município = fct_reorder(Município, media)) %>%
      ggplot(aes(x = media, y = Município, fill = media)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(media, 2)),
                hjust = -0.15, size = 3) +
      scale_fill_viridis_c(option = "D") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = nome, y = NULL) +
      chart_theme
  })
  
  output$chart_hist <- renderPlot({
    col  <- ind_col()
    nome <- ind_nome()
    df   <- dados_filtrados()
    
    if (!col %in% names(df)) return(NULL)
    
    ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = 40, fill = "#3B82F6", colour = "white", linewidth = 0.3) +
      geom_vline(aes(xintercept = median(.data[[col]], na.rm = TRUE)),
                 colour = "tomato", linewidth = 1, linetype = "dashed") +
      annotate("text",
               x     = median(df[[col]], na.rm = TRUE),
               y     = Inf, vjust = 2, hjust = -0.1,
               label = paste0("Mediana: ", round(median(df[[col]], na.rm = TRUE), 2)),
               colour = "tomato", size = 3.5) +
      labs(x = nome, y = "Nº de favelas") +
      theme_minimal(base_size = 12)
  })
  
  # -----------------------------------------------------------------------
  # Tab 4 — Dados (full table + download)
  # -----------------------------------------------------------------------
  output$dados_titulo <- renderText({
    paste0(nrow(dados_filtrados()), " favelas selecionadas")
  })
  
  tabela_export <- reactive({
    dados_filtrados() %>%
      select(
        cd_fcu, nm_fcu, nm_mun, nm_uf,
        total_pessoas, total_dp_ocupados,
        perc_agua_adequada, perc_esgoto_adequado, perc_lixo_coleta,
        renda_sm_pond, perc_analfabeto_populacao,
        num_medio_banheiros_por_morador,
        perc_via_pavimentada, perc_bueiro, perc_iluminacao_publica,
        perc_ponto_onibus, perc_via_bicicleta, perc_calcada,
        perc_obstaculo_calcada, perc_rampa_cadeirante,
        IDS, IDA,
        any_of(c("tem_risco"))
      ) %>%
      rename(
        "Código FCU"           = cd_fcu,
        "Nome da favela"       = nm_fcu,
        "Município"            = nm_mun,
        "UF"                   = nm_uf,
        "População"            = total_pessoas,
        "Domicílios ocupados"  = total_dp_ocupados,
        "Água encanada (%)"    = perc_agua_adequada,
        "Esgoto rede geral (%)"= perc_esgoto_adequado,
        "Coleta de lixo (%)"   = perc_lixo_coleta,
        "Renda média (SM)"     = renda_sm_pond,
        "Analfabetismo 15+ (%)"= perc_analfabeto_populacao,
        "Banheiros/morador"    = num_medio_banheiros_por_morador,
        "Via pavimentada (%)"  = perc_via_pavimentada,
        "Bueiro (%)"           = perc_bueiro,
        "Iluminação pública (%)"= perc_iluminacao_publica,
        "Ponto de ônibus (%)"  = perc_ponto_onibus,
        "Ciclovia (%)"         = perc_via_bicicleta,
        "Calçada (%)"          = perc_calcada,
        "Obstáculo calçada (%)"= perc_obstaculo_calcada,
        "Rampa cadeirante (%)" = perc_rampa_cadeirante,
        "IDS"                  = IDS,
        "IDA"                  = IDA
      )
  })
  
  output$tabela_dados <- renderDT({
    datatable(
      tabela_export(),
      rownames  = FALSE,
      filter    = "top",
      options   = list(
        pageLength = 25,
        scrollX    = TRUE,
        dom        = "frtip"
      )
    ) %>%
      formatRound(columns = c("IDS", "IDA", "Renda média (SM)",
                              "Banheiros/morador"), digits = 3) %>%
      formatRound(columns = c("Água encanada (%)", "Esgoto rede geral (%)",
                              "Coleta de lixo (%)", "Analfabetismo 15+ (%)",
                              "Via pavimentada (%)", "Bueiro (%)",
                              "Iluminação pública (%)", "Ponto de ônibus (%)",
                              "Ciclovia (%)", "Calçada (%)",
                              "Obstáculo calçada (%)", "Rampa cadeirante (%)"),
                  digits = 1) %>%
      formatCurrency(columns = c("População", "Domicílios ocupados"),
                     currency = "", interval = 3, mark = ".", digits = 0)
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      uf_str  <- if (length(input$sel_uf)  > 0 && input$sel_uf  != "")
        paste0("_", paste(input$sel_uf,  collapse = "-")) else ""
      mun_str <- if (length(input$sel_mun) > 0 && input$sel_mun != "")
        paste0("_", paste(input$sel_mun, collapse = "-")) else ""
      paste0("favelas_br", uf_str, mun_str, "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv2(tabela_export(), file)
    }
  )
}

# =========================================================================
# RUN
# =========================================================================
shinyApp(ui, server)

# =========================================================================
# EXPORT MAP AS SELF-CONTAINED HTML
# =========================================================================
# Run this block manually after the script has loaded fav_sf.
# Produces a single .html file — opens in any browser, no R server needed.

library(htmlwidgets)

vals <- fav_sf[["IDS"]]
pal  <- colorNumeric(viridis(100), domain = vals, na.color = "#CCCCCC")

hover <- sprintf(
  "<b>%s</b><br>IDS: <b>%s</b><br>Município: %s<br>UF: %s<br>Pop.: %s",
  fav_sf$NM_FCU,
  ifelse(is.na(vals), "—", round(vals, 3)),
  fav_sf$NM_MUN,
  fav_sf$NM_UF,
  formatC(fav_sf$TOT_PES, format = "d", big.mark = ".")
)

mapa_html <- leaflet(fav_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor   = ~pal(vals),
    fillOpacity = 0.8,
    weight      = 0.8,
    color       = "white",
    label       = lapply(hover, HTML),
    highlightOptions = highlightOptions(
      weight      = 2,
      color       = "#FFD700",
      fillOpacity = 0.95,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal      = pal,
    values   = vals,
    title    = "IDS",
    opacity  = 0.9,
    position = "bottomright"
  )

saveWidget(
  widget        = mapa_html,
  file          = file.path(OUTPUT_DIR, "mapa_ids_favelas_br.html"),
  selfcontained = TRUE
)

message("Exported: ", file.path(OUTPUT_DIR, "mapa_ids_favelas_br.html"))

# =========================================================================
# DEPLOY TO SHINYAPPS.IO
# =========================================================================
# STEP 1 — Run once to store your credentials (get token from shinyapps.io
#           under Account > Tokens > Show):
#
rsconnect::setAccountInfo(name='brunopanta', token='B27DCC2A8B7F7DD01FE40E328806A21F', secret='BOlXhUZwbhg7YsCrvIJoG0BaNvkTPghRKF6ph4cm')



# STEP 2 — Run the block below to prepare the deployment folder and deploy.

if (FALSE) {
  library(rsconnect)
  library(sf)
  
  # --- 1) Create a self-contained deployment folder -----------------------
  DEPLOY_DIR <- "C:/Users/USUARIO/Dropbox/Pesquisa/Dados/FAVELAS_BR/deploy"
  dir.create(DEPLOY_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(DEPLOY_DIR, "data"), showWarnings = FALSE)
  
  # Copy the app script (will read from relative paths inside deploy/)
  file.copy(
    "C:/Users/USUARIO/Dropbox/Pesquisa/Dados/FAVELAS_BR/Inputs/08_plataforma_insights.R",
    file.path(DEPLOY_DIR, "app.R"),
    overwrite = TRUE
  )
  
  # Copy data files needed at runtime
  # Shapefile needs all 5 sidecar files
  shp_base <- tools::file_path_sans_ext(FAVELAS_IDS_SHP)
  for (ext in c(".shp", ".dbf", ".shx", ".prj", ".cpg")) {
    f <- paste0(shp_base, ext)
    if (file.exists(f))
      file.copy(f, file.path(DEPLOY_DIR, "data", paste0("favelas_ids", ext)),
                overwrite = TRUE)
  }
  # Pre-filter to Northeast only before copying — keeps bundle small
  NORDESTE <- c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte",
                "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia")
  
  fav_csv_full <- read_csv2(FAVELAS_IDS_CSV, show_col_types = FALSE) %>%
    filter(nm_uf %in% NORDESTE)
  write_csv2(fav_csv_full, file.path(DEPLOY_DIR, "data", "favelas_ids.csv"))
  
  riscos_full <- read_csv2(FAVELAS_RISCOS_CSV, show_col_types = FALSE)
  fcu_nordeste <- fav_csv_full$cd_fcu
  write_csv2(
    riscos_full %>% filter(cd_fcu %in% fcu_nordeste),
    file.path(DEPLOY_DIR, "data", "favelas_riscos.csv")
  )
  
  # Save filtered sf as RDS — much faster to read than shapefile,
  # no sidecar files, and smaller bundle size.
  fav_shp_full <- st_read(FAVELAS_IDS_SHP, quiet = TRUE) %>%
    st_make_valid() %>%
    filter(NM_UF %in% NORDESTE)
  saveRDS(fav_shp_full,
          file.path(DEPLOY_DIR, "data", "favelas_ids_nordeste.rds"))
  message("Shapefile filtered and saved as RDS.")
  
  # --- 2) Patch app.R to use relative data paths --------------------------
  app_code <- readLines(file.path(DEPLOY_DIR, "app.R"))
  
  # Replace the source("config.R") block with inline relative paths
  app_code <- gsub(
    'setwd.*
source\("config\.R"\)',
    '# Deployment: data loaded from relative paths
FAVELAS_IDS_SHP   <- "data/favelas_ids.shp"
FAVELAS_IDS_CSV   <- "data/favelas_ids.csv"
FAVELAS_RISCOS_CSV <- "data/favelas_riscos.csv"
OUTPUT_DIR        <- "."',
    app_code
  )
  
  # Remove the HTML export and deploy blocks (not needed on server)
  start_html   <- grep("# EXPORT MAP AS SELF-CONTAINED HTML", app_code)
  start_deploy <- grep("# DEPLOY TO SHINYAPPS\.IO", app_code)
  if (length(start_html) > 0 && length(start_deploy) > 0) {
    app_code <- app_code[1:(start_html[1] - 1)]
  }
  
  writeLines(app_code, file.path(DEPLOY_DIR, "app.R"))
  
  # --- 3) Deploy ----------------------------------------------------------
  rsconnect::deployApp(
    appDir      = DEPLOY_DIR,
    appName     = "plataforma_favelas_br",
    forceUpdate = TRUE
  )
  
  message("Deployed! Visit: https://YOUR_SHINYAPPS_USERNAME.shinyapps.io/plataforma_favelas_br")
}