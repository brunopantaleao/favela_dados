# Favelas BR — Plataforma de Análise
# Deployed version — all data fetched from GitHub

# GitHub raw URLs
GEOJSON_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_br_simplified.geojson"
CSV_IDS_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_ids_ida.csv"
CSV_RISK_URL <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_riscos.csv"

# Packages
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
# =========================================================================
message("Loading GeoJSON from GitHub...")
fav_sf <- st_read(GEOJSON_URL, quiet = TRUE) %>%
  st_make_valid() %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(CD_FCU = as.character(CD_FCU))
message("  Loaded: ", nrow(fav_sf), " FCUs")

fav_df <- read_csv2(CSV_IDS_URL, show_col_types = FALSE)

riscos <- read_csv2(CSV_RISK_URL, show_col_types = FALSE) %>%
  mutate(cd_fcu = as.character(cd_fcu))

fav_df <- fav_df %>%
  mutate(cd_fcu = as.character(cd_fcu)) %>%
  left_join(riscos %>% select(cd_fcu, tem_risco), by = "cd_fcu")

# =========================================================================
# CAPITAL COORDINATES — for auto-zoom when UF is selected
# =========================================================================
capitais <- tibble::tribble(
  ~nm_uf,                  ~lat,     ~lng,  ~zoom,
  "Acre",                 -9.975,  -67.824,   11,
  "Alagoas",              -9.666,  -35.735,   11,
  "Amapá",                 0.034,  -51.066,   11,
  "Amazonas",             -3.119,  -60.021,   11,
  "Bahia",               -12.971,  -38.501,   11,
  "Ceará",                -3.717,  -38.543,   11,
  "Distrito Federal",    -15.779,  -47.929,   11,
  "Espírito Santo",      -20.319,  -40.338,   11,
  "Goiás",               -16.686,  -49.264,   11,
  "Maranhão",             -2.530,  -44.303,   11,
  "Mato Grosso",         -15.601,  -56.097,   11,
  "Mato Grosso do Sul",  -20.469,  -54.620,   11,
  "Minas Gerais",        -19.917,  -43.934,   11,
  "Pará",                 -1.455,  -48.502,   11,
  "Paraíba",              -7.115,  -34.863,   11,
  "Paraná",              -25.428,  -49.273,   11,
  "Pernambuco",           -8.054,  -34.881,   11,
  "Piauí",                -5.092,  -42.803,   11,
  "Rio de Janeiro",      -22.906,  -43.173,   11,
  "Rio Grande do Norte",  -5.795,  -35.209,   11,
  "Rio Grande do Sul",   -30.033,  -51.230,   11,
  "Rondônia",             -8.761,  -63.902,   11,
  "Roraima",               2.819,  -60.673,   11,
  "Santa Catarina",      -27.595,  -48.548,   11,
  "São Paulo",           -23.550,  -46.633,   11,
  "Sergipe",             -10.916,  -37.073,   11,
  "Tocantins",           -10.249,  -48.324,   11
)

# =========================================================================
# INDICATOR CATALOGUE
# =========================================================================
indicadores <- list(
  list(col = "IDS",       label = "IDS — Índice de Desenvolvimento Social",   dir = +1, group = "Índices"),
  list(col = "IDA",       label = "IDA — Índice de Acessibilidade Urbana",    dir = +1, group = "Índices"),
  list(col = "PERC_AGUA", label = "Água encanada (%)",                        dir = +1, group = "Saneamento"),
  list(col = "PERC_ESGO", label = "Esgoto rede geral (%)",                    dir = +1, group = "Saneamento"),
  list(col = "PERC_LIXO", label = "Coleta de lixo (%)",                       dir = +1, group = "Saneamento"),
  list(col = "RENDA_SM",  label = "Renda média (sal. mín.)",                  dir = +1, group = "Renda e Educação"),
  list(col = "PERC_ANALF",label = "Analfabetismo 15+ (%)",                    dir = -1, group = "Renda e Educação"),
  list(col = "P_VIAPAV",  label = "Via pavimentada (%)",                      dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_BUEIRO",  label = "Bueiro / boca de lobo (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ILUM",    label = "Iluminação pública (%)",                   dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ONTON",   label = "Ponto de ônibus (%)",                      dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_VIABIC",  label = "Ciclovia / ciclofaixa (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_CALCAD",  label = "Calçada (%)",                              dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_OBSTAC",  label = "Obstáculo na calçada (%)",                 dir = -1, group = "Acessibilidade Urbana"),
  list(col = "P_RAMPA",   label = "Rampa para cadeirante (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "tem_risco", label = "Exposição a risco (flag)",                 dir = -1, group = "Riscos Naturais")
)

ind_choices <- setNames(sapply(indicadores, `[[`, "col"), sapply(indicadores, `[[`, "label"))
ind_groups  <- sapply(indicadores, `[[`, "group")
ind_grouped <- split(ind_choices, ind_groups)
ind_label   <- setNames(sapply(indicadores, `[[`, "label"), sapply(indicadores, `[[`, "col"))

# =========================================================================
# GEO HIERARCHIES
# =========================================================================
ufs        <- sort(unique(na.omit(fav_df$nm_uf)))
municipios <- sort(unique(na.omit(fav_df$nm_mun)))

# =========================================================================
# HELPERS
# =========================================================================
filter_data <- function(df, uf, mun) {
  if (!is.null(uf)  && length(uf)  > 0 && !all(uf  == ""))
    df <- df %>% filter(nm_uf  %in% uf)
  if (!is.null(mun) && length(mun) > 0 && !all(mun == ""))
    df <- df %>% filter(nm_mun %in% mun)
  df
}

filter_sf <- function(sf_obj, uf, mun) {
  if (!is.null(uf)  && length(uf)  > 0 && !all(uf  == ""))
    sf_obj <- sf_obj %>% filter(NM_UF  %in% uf)
  if (!is.null(mun) && length(mun) > 0 && !all(mun == ""))
    sf_obj <- sf_obj %>% filter(NM_MUN %in% mun)
  sf_obj
}

# Build rich popup HTML for a single sf row
# Uses shapefile short names (IDS, IDA, PERC_AGUA, etc.)
make_popup <- function(sf_obj) {
  fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(round(x, 1), "%"))
  fmt_num <- function(x) ifelse(is.na(x), "—", round(x, 3))
  fmt_sm  <- function(x) ifelse(is.na(x), "—", paste0(round(x, 2), " SM"))

  mapply(function(nm, ids, ida,
                  agua, esgo, lixo, banh,
                  renda, analf,
                  viapav, bueiro, ilum, onton, viabic, calcad, obstac, rampa,
                  risco, pop) {
    sprintf(
      "<div style='font-family:sans-serif;min-width:220px;'>
        <b style='font-size:1.05em;'>%s</b>
        <hr style='margin:4px 0;'>
        <table style='width:100%%;font-size:0.88em;border-collapse:collapse;'>

          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Índices compostos</td></tr>
          <tr><td>IDS</td><td><b>%s</b></td></tr>
          <tr><td>IDA</td><td><b>%s</b></td></tr>

          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Saneamento</td></tr>
          <tr><td>Água encanada</td><td>%s</td></tr>
          <tr><td>Esgoto rede geral</td><td>%s</td></tr>
          <tr><td>Coleta de lixo</td><td>%s</td></tr>
          <tr><td>Banheiros/morador</td><td>%s</td></tr>

          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Renda e Educação</td></tr>
          <tr><td>Renda média</td><td>%s</td></tr>
          <tr><td>Analfabetismo 15+</td><td>%s</td></tr>

          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Acessibilidade Urbana</td></tr>
          <tr><td>Via pavimentada</td><td>%s</td></tr>
          <tr><td>Bueiro</td><td>%s</td></tr>
          <tr><td>Iluminação pública</td><td>%s</td></tr>
          <tr><td>Ponto de ônibus</td><td>%s</td></tr>
          <tr><td>Ciclovia</td><td>%s</td></tr>
          <tr><td>Calçada</td><td>%s</td></tr>
          <tr><td>Obstáculo calçada</td><td>%s</td></tr>
          <tr><td>Rampa cadeirante</td><td>%s</td></tr>

          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Informações gerais</td></tr>
          <tr><td>População</td><td>%s</td></tr>
          <tr><td>Risco natural</td><td>%s</td></tr>

        </table>
      </div>",
      nm,
      fmt_num(ids), fmt_num(ida),
      fmt_pct(agua), fmt_pct(esgo), fmt_pct(lixo), fmt_num(banh),
      fmt_sm(renda), fmt_pct(analf),
      fmt_pct(viapav), fmt_pct(bueiro), fmt_pct(ilum), fmt_pct(onton),
      fmt_pct(viabic), fmt_pct(calcad), fmt_pct(obstac), fmt_pct(rampa),
      formatC(pop, format = "d", big.mark = "."),
      ifelse(is.na(risco), "—", ifelse(risco == 1, "Sim", "Não"))
    )
  },
  nm     = sf_obj$NM_FCU,
  ids    = sf_obj$IDS,
  ida    = sf_obj$IDA,
  agua   = sf_obj$PERC_AGUA,
  esgo   = sf_obj$PERC_ESGO,
  lixo   = sf_obj$PERC_LIXO,
  banh   = sf_obj$I_BANH,
  renda  = sf_obj$RENDA_SM,
  analf  = sf_obj$PERC_ANALF,
  viapav = sf_obj$P_VIAPAV,
  bueiro = sf_obj$P_BUEIRO,
  ilum   = sf_obj$P_ILUM,
  onton  = sf_obj$P_ONTON,
  viabic = sf_obj$P_VIABIC,
  calcad = sf_obj$P_CALCAD,
  obstac = sf_obj$P_OBSTAC,
  rampa  = sf_obj$P_RAMPA,
  risco  = sf_obj$tem_risco,
  pop    = sf_obj$TOT_PES,
  SIMPLIFY = TRUE)
}

# =========================================================================
# UI
# =========================================================================
ui <- page_navbar(
  title    = "Favela Dados — Clique na favela e saiba mais",
  theme    = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  fillable = TRUE,

  sidebar = sidebar(
    width = 260,
    title = "Filtros",
    selectInput("sel_uf",  "Estado (UF)",  choices = c("Todos" = "", ufs),       selected = "", multiple = TRUE),
    selectInput("sel_mun", "Município",    choices = c("Todos" = "", municipios), selected = "", multiple = TRUE),
    hr(),
    selectInput("sel_ind", "Indicador",    choices = ind_grouped, selected = "IDS"),
    hr(),
    p(tags$small(tags$i("Dados: Censo IBGE 2022 · IBGE FCU 2022 · SGB")))
  ),

  nav_panel(
    title = tagList(icon("map"), " Mapa"),
    card(full_screen = TRUE,
      card_header(textOutput("mapa_titulo")),
      uiOutput("mapa_ui")
    )
  ),

  nav_panel(
    title = tagList(icon("chart-bar"), " Comparações"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Média por Estado (UF)"),        card_body(plotOutput("chart_uf",   height = "420px"))),
      card(card_header("Média por Município (top 20)"), card_body(plotOutput("chart_mun",  height = "420px")))
    ),
    layout_columns(col_widths = c(12),
      card(card_header("Distribuição do indicador"), card_body(plotOutput("chart_hist", height = "280px")))
    )
  ),

  nav_panel(
    title = tagList(icon("table"), " Dados"),
    card(
      card_header(layout_columns(col_widths = c(8, 4),
        textOutput("dados_titulo"),
        downloadButton("download_csv", "Baixar CSV", class = "btn-sm btn-success")
      )),
      card_body(DTOutput("tabela_dados"))
    )
  )
)

# =========================================================================
# SERVER
# =========================================================================
server <- function(input, output, session) {

  observeEvent(input$sel_uf, {
    muns_filtrados <- if (length(input$sel_uf) > 0 && !all(input$sel_uf == "")) {
      fav_df %>% filter(nm_uf %in% input$sel_uf) %>% pull(nm_mun) %>% unique() %>% sort()
    } else { municipios }
    updateSelectInput(session, "sel_mun",
      choices  = c("Todos" = "", muns_filtrados),
      selected = input$sel_mun[input$sel_mun %in% muns_filtrados]
    )
  })

  dados_filtrados <- reactive({ filter_data(fav_df, input$sel_uf, input$sel_mun) })
  sf_filtrado     <- reactive({ filter_sf(fav_sf,  input$sel_uf, input$sel_mun) })
  ind_col         <- reactive({ input$sel_ind })
  ind_nome        <- reactive({ ind_label[[input$sel_ind]] })

  # -----------------------------------------------------------------------
  # Tab 1 — Mapa
  # -----------------------------------------------------------------------
  output$mapa_titulo <- renderText({
    if (length(input$sel_uf) == 0 || all(input$sel_uf == ""))
      "Mapa — selecione um Estado para visualizar"
    else
      paste0("Mapa — ", ind_nome(), " | ", nrow(dados_filtrados()), " favelas")
  })

  output$mapa_ui <- renderUI({
    if (length(input$sel_uf) == 0 || all(input$sel_uf == "")) {
      div(
        style = "height:680px;display:flex;flex-direction:column;align-items:center;justify-content:center;background:#f8f9fa;border-radius:6px;color:#6c757d;",
        tags$i(class = "fa fa-map fa-3x", style = "margin-bottom:16px;color:#adb5bd;"),
        tags$h5("Selecione um Estado para carregar o mapa", style = "font-weight:500;margin-bottom:8px;"),
        tags$p("Use o filtro 'Estado (UF)' na barra lateral.", style = "font-size:0.9rem;")
      )
    } else {
      leafletOutput("mapa", width = "100%", height = "680px")
    }
  })

  output$mapa <- renderLeaflet({
    req(length(input$sel_uf) > 0, !all(input$sel_uf == ""))

    sf_obj <- sf_filtrado()
    col    <- ind_col()
    nome   <- ind_nome()   # extract reactive before leaflet call
    vals   <- sf_obj[[col]]
    pal    <- colorNumeric(viridis(100), domain = vals, na.color = "#CCCCCC")
    popups <- unname(make_popup(sf_obj))

    uf_sel   <- input$sel_uf[1]
    cap      <- capitais %>% filter(nm_uf == uf_sel)
    map_lat  <- if (nrow(cap) > 0) cap$lat[1]  else -15
    map_lng  <- if (nrow(cap) > 0) cap$lng[1]  else -50
    map_zoom <- if (nrow(cap) > 0) cap$zoom[1] else  5

    leaflet(sf_obj) %>%
      setView(lng = map_lng, lat = map_lat, zoom = map_zoom) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~pal(vals),
        fillOpacity = 0.8,
        weight      = 0.8,
        color       = "white",
        popup       = popups,
        label       = ~NM_FCU,  
        highlightOptions = highlightOptions(
          weight = 2, color = "#FFD700",
          fillOpacity = 0.95, bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = pal, values = vals, title = nome,
                opacity = 0.9, position = "bottomright")
  })

  observeEvent(input$sel_uf, {
    req(length(input$sel_uf) > 0, !all(input$sel_uf == ""))
    uf_sel <- input$sel_uf[1]
    cap    <- capitais %>% filter(nm_uf == uf_sel)
    if (nrow(cap) > 0) {
      leafletProxy("mapa") %>%
        setView(lng = cap$lng[1], lat = cap$lat[1], zoom = cap$zoom[1])
    }
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Tab 2 — Comparações
  # -----------------------------------------------------------------------
  chart_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 9),
          plot.title  = element_blank())

  output$chart_uf <- renderPlot({
    col <- ind_col(); nome <- ind_nome(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    df %>% group_by(UF = nm_uf) %>%
      summarise(media = mean(.data[[col]], na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(!is.na(UF)) %>% mutate(UF = fct_reorder(UF, media)) %>%
      ggplot(aes(x = media, y = UF, fill = media)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(media, 2)), hjust = -0.15, size = 3.2) +
      scale_fill_viridis_c(option = "D") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = nome, y = NULL) + chart_theme
  })

  output$chart_mun <- renderPlot({
    col <- ind_col(); nome <- ind_nome(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    df %>% group_by(Município = nm_mun) %>%
      summarise(media = mean(.data[[col]], na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(!is.na(Município)) %>% slice_max(order_by = n, n = 20) %>%
      mutate(Município = fct_reorder(Município, media)) %>%
      ggplot(aes(x = media, y = Município, fill = media)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(media, 2)), hjust = -0.15, size = 3) +
      scale_fill_viridis_c(option = "D") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = nome, y = NULL) + chart_theme
  })

  output$chart_hist <- renderPlot({
    col <- ind_col(); nome <- ind_nome(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    med <- median(df[[col]], na.rm = TRUE)
    ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = 40, fill = "#3B82F6", colour = "white", linewidth = 0.3) +
      geom_vline(xintercept = med, colour = "tomato", linewidth = 1, linetype = "dashed") +
      annotate("text", x = med, y = Inf, vjust = 2, hjust = -0.1,
               label = paste0("Mediana: ", round(med, 2)),
               colour = "tomato", size = 3.5) +
      labs(x = nome, y = "Nº de favelas") + theme_minimal(base_size = 12)
  })

  # -----------------------------------------------------------------------
  # Tab 3 — Dados
  # -----------------------------------------------------------------------
  output$dados_titulo <- renderText({
    paste0(nrow(dados_filtrados()), " favelas selecionadas")
  })

  tabela_export <- reactive({
    dados_filtrados() %>%
      select(cd_fcu, nm_fcu, nm_mun, nm_uf, total_pessoas, total_dp_ocupados,
             perc_agua_adequada, perc_esgoto_adequado, perc_lixo_coleta,
             renda_sm_pond, perc_analfabeto_populacao, num_medio_banheiros_por_morador,
             perc_via_pavimentada, perc_bueiro, perc_iluminacao_publica,
             perc_ponto_onibus, perc_via_bicicleta, perc_calcada,
             perc_obstaculo_calcada, perc_rampa_cadeirante, IDS, IDA,
             any_of("tem_risco")) %>%
      rename(
        "Código FCU"            = cd_fcu,
        "Nome da favela"        = nm_fcu,
        "Município"             = nm_mun,
        "UF"                    = nm_uf,
        "População"             = total_pessoas,
        "Domicílios ocupados"   = total_dp_ocupados,
        "Água encanada (%)"     = perc_agua_adequada,
        "Esgoto rede geral (%)" = perc_esgoto_adequado,
        "Coleta de lixo (%)"    = perc_lixo_coleta,
        "Renda média (SM)"      = renda_sm_pond,
        "Analfabetismo 15+ (%)" = perc_analfabeto_populacao,
        "Banheiros/morador"     = num_medio_banheiros_por_morador,
        "Via pavimentada (%)"   = perc_via_pavimentada,
        "Bueiro (%)"            = perc_bueiro,
        "Iluminação pública (%)"= perc_iluminacao_publica,
        "Ponto de ônibus (%)"   = perc_ponto_onibus,
        "Ciclovia (%)"          = perc_via_bicicleta,
        "Calçada (%)"           = perc_calcada,
        "Obstáculo calçada (%)" = perc_obstaculo_calcada,
        "Rampa cadeirante (%)"  = perc_rampa_cadeirante,
        "IDS"                   = IDS,
        "IDA"                   = IDA
      )
  })

  output$tabela_dados <- renderDT({
    datatable(tabela_export(), rownames = FALSE, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "frtip")) %>%
      formatRound(columns = c("IDS", "IDA", "Renda média (SM)", "Banheiros/morador"), digits = 3) %>%
      formatRound(columns = c("Água encanada (%)", "Esgoto rede geral (%)", "Coleta de lixo (%)",
                              "Analfabetismo 15+ (%)", "Via pavimentada (%)", "Bueiro (%)",
                              "Iluminação pública (%)", "Ponto de ônibus (%)", "Ciclovia (%)",
                              "Calçada (%)", "Obstáculo calçada (%)", "Rampa cadeirante (%)"),
                  digits = 1) %>%
      formatCurrency(columns = c("População", "Domicílios ocupados"),
                     currency = "", interval = 3, mark = ".", digits = 0)
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      uf_str  <- if (length(input$sel_uf)  > 0 && !all(input$sel_uf  == ""))
        paste0("_", paste(input$sel_uf,  collapse = "-")) else ""
      mun_str <- if (length(input$sel_mun) > 0 && !all(input$sel_mun == ""))
        paste0("_", paste(input$sel_mun, collapse = "-")) else ""
      paste0("favelas_br", uf_str, mun_str, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) { write_csv2(tabela_export(), file) }
  )
}

shinyApp(ui, server)
