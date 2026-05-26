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

# =========================================================================
# UI
# =========================================================================
ui <- page_navbar(
  title    = "Favelas BR — Plataforma de Análise",
  theme    = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  fillable = TRUE,

  sidebar = sidebar(
    width = 260,
    title = "Filtros",
    selectInput("sel_uf",  "Estado (UF)",  choices = c("Todos" = "", ufs),        selected = "", multiple = TRUE),
    selectInput("sel_mun", "Município",    choices = c("Todos" = "", municipios),  selected = "", multiple = TRUE),
    hr(),
    selectInput("sel_ind", "Indicador",   choices = ind_grouped, selected = "IDS"),
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
    title = tagList(icon("trophy"), " Rankings"),
    card(
      card_header(textOutput("ranking_titulo")),
      card_body(DTOutput("tabela_ranking"))
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

  output$mapa_titulo <- renderText({
    if (length(input$sel_uf) == 0 || all(input$sel_uf == ""))
      "Mapa — selecione um Estado para visualizar"
    else
      paste0("Mapa — ", ind_nome(), " | ", nrow(dados_filtrados()), " favelas")
  })

  output$mapa_ui <- renderUI({
    if (length(input$sel_uf) == 0 || all(input$sel_uf == "")) {
      div(style = "height:680px;display:flex;flex-direction:column;align-items:center;justify-content:center;background:#f8f9fa;border-radius:6px;color:#6c757d;",
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
    vals   <- sf_obj[[col]]
    pal    <- colorNumeric(viridis(100), domain = vals, na.color = "#CCCCCC")
    hover  <- sprintf("<b>%s</b><br>%s: <b>%s</b><br>Município: %s<br>UF: %s<br>Pop.: %s",
      sf_obj$NM_FCU, ind_nome(), ifelse(is.na(vals), "—", round(vals, 3)),
      sf_obj$NM_MUN, sf_obj$NM_UF, formatC(sf_obj$TOT_PES, format = "d", big.mark = "."))
    leaflet(sf_obj) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(vals), fillOpacity = 0.8, weight = 0.8, color = "white",
        label = lapply(hover, HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#FFD700", fillOpacity = 0.95, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = vals, title = ind_nome(), opacity = 0.9, position = "bottomright")
  })

  output$ranking_titulo <- renderText({
    paste0("Ranking por ", ind_nome(), " — ", nrow(dados_filtrados()), " favelas")
  })

  output$tabela_ranking <- renderDT({
    col <- ind_col()
    df  <- dados_filtrados() %>%
      select(Favela = nm_fcu, Município = nm_mun, UF = nm_uf,
             IDS, IDA, Indicador = any_of(col), População = total_pessoas, Renda_SM = renda_sm_pond) %>%
      arrange(desc(.data[[ifelse(col %in% names(.), col, "IDS")]]))
    datatable(df, rownames = FALSE, filter = "top",
      options = list(pageLength = 20, scrollX = TRUE, dom = "frtip")) %>%
      formatRound(columns = c("IDS", "IDA", "Renda_SM"), digits = 3) %>%
      formatRound(columns = intersect("Indicador", names(df)), digits = 2) %>%
      formatCurrency(columns = "População", currency = "", interval = 3, mark = ".", digits = 0)
  })

  chart_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(), axis.text.x = element_text(size = 9), plot.title = element_blank())

  output$chart_uf <- renderPlot({
    col <- ind_col(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    df %>% group_by(UF = nm_uf) %>%
      summarise(media = mean(.data[[col]], na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(!is.na(UF)) %>% mutate(UF = fct_reorder(UF, media)) %>%
      ggplot(aes(x = media, y = UF, fill = media)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(media, 2)), hjust = -0.15, size = 3.2) +
      scale_fill_viridis_c(option = "D") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = ind_nome(), y = NULL) + chart_theme
  })

  output$chart_mun <- renderPlot({
    col <- ind_col(); df <- dados_filtrados()
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
      labs(x = ind_nome(), y = NULL) + chart_theme
  })

  output$chart_hist <- renderPlot({
    col <- ind_col(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = 40, fill = "#3B82F6", colour = "white", linewidth = 0.3) +
      geom_vline(aes(xintercept = median(.data[[col]], na.rm = TRUE)),
                 colour = "tomato", linewidth = 1, linetype = "dashed") +
      annotate("text", x = median(df[[col]], na.rm = TRUE), y = Inf, vjust = 2, hjust = -0.1,
               label = paste0("Mediana: ", round(median(df[[col]], na.rm = TRUE), 2)),
               colour = "tomato", size = 3.5) +
      labs(x = ind_nome(), y = "Nº de favelas") + theme_minimal(base_size = 12)
  })

  output$dados_titulo <- renderText({ paste0(nrow(dados_filtrados()), " favelas selecionadas") })

  tabela_export <- reactive({
    dados_filtrados() %>%
      select(cd_fcu, nm_fcu, nm_mun, nm_uf, total_pessoas, total_dp_ocupados,
             perc_agua_adequada, perc_esgoto_adequado, perc_lixo_coleta,
             renda_sm_pond, perc_analfabeto_populacao, num_medio_banheiros_por_morador,
             perc_via_pavimentada, perc_bueiro, perc_iluminacao_publica,
             perc_ponto_onibus, perc_via_bicicleta, perc_calcada,
             perc_obstaculo_calcada, perc_rampa_cadeirante, IDS, IDA, any_of("tem_risco")) %>%
      rename("Código FCU" = cd_fcu, "Nome da favela" = nm_fcu, "Município" = nm_mun, "UF" = nm_uf,
             "População" = total_pessoas, "Domicílios ocupados" = total_dp_ocupados,
             "Água encanada (%)" = perc_agua_adequada, "Esgoto rede geral (%)" = perc_esgoto_adequado,
             "Coleta de lixo (%)" = perc_lixo_coleta, "Renda média (SM)" = renda_sm_pond,
             "Analfabetismo 15+ (%)" = perc_analfabeto_populacao, "Banheiros/morador" = num_medio_banheiros_por_morador,
             "Via pavimentada (%)" = perc_via_pavimentada, "Bueiro (%)" = perc_bueiro,
             "Iluminação pública (%)" = perc_iluminacao_publica, "Ponto de ônibus (%)" = perc_ponto_onibus,
             "Ciclovia (%)" = perc_via_bicicleta, "Calçada (%)" = perc_calcada,
             "Obstáculo calçada (%)" = perc_obstaculo_calcada, "Rampa cadeirante (%)" = perc_rampa_cadeirante,
             "IDS" = IDS, "IDA" = IDA)
  })

  output$tabela_dados <- renderDT({
    datatable(tabela_export(), rownames = FALSE, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "frtip")) %>%
      formatRound(columns = c("IDS", "IDA", "Renda média (SM)", "Banheiros/morador"), digits = 3) %>%
      formatRound(columns = c("Água encanada (%)", "Esgoto rede geral (%)", "Coleta de lixo (%)",
                              "Analfabetismo 15+ (%)", "Via pavimentada (%)", "Bueiro (%)",
                              "Iluminação pública (%)", "Ponto de ônibus (%)", "Ciclovia (%)",
                              "Calçada (%)", "Obstáculo calçada (%)", "Rampa cadeirante (%)"), digits = 1) %>%
      formatCurrency(columns = c("População", "Domicílios ocupados"),
                     currency = "", interval = 3, mark = ".", digits = 0)
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      uf_str  <- if (length(input$sel_uf)  > 0 && !all(input$sel_uf  == "")) paste0("_", paste(input$sel_uf,  collapse = "-")) else ""
      mun_str <- if (length(input$sel_mun) > 0 && !all(input$sel_mun == "")) paste0("_", paste(input$sel_mun, collapse = "-")) else ""
      paste0("favelas_br", uf_str, mun_str, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) { write_csv2(tabela_export(), file) }
  )
}

shinyApp(ui, server)
