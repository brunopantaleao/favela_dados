# Favelas BR - Plataforma de Analise

GEOJSON_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_br_simplified.geojson"
CSV_IDS_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_ids_ida.csv"
CSV_RISK_URL <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_riscos.csv"
CSV_AOP_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_acesso_oportunidades.csv"

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
library(tidyr)

# =========================================================================
# DATA LOADING
# =========================================================================
message("Loading GeoJSON from GitHub...")
fav_sf <- st_read(GEOJSON_URL, quiet = TRUE) %>%
  st_make_valid() %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(CD_FCU = as.character(CD_FCU))
message("  Loaded: ", nrow(fav_sf), " FCUs")

fav_df <- read_csv2(CSV_IDS_URL, show_col_types = FALSE) %>%
  mutate(cd_fcu = as.character(cd_fcu))

riscos <- read_csv2(CSV_RISK_URL, show_col_types = FALSE) %>%
  mutate(cd_fcu = as.character(cd_fcu))

aop <- tryCatch({
  tmp <- read_csv2(CSV_AOP_URL, show_col_types = FALSE) %>%
    mutate(cd_fcu = as.character(cd_fcu))
  # keep whichever naming convention the file uses
  keep <- intersect(names(tmp),
                    c("cd_fcu","CMAET60","CMAST60","CMATT60","CMACT60",
                      "CMAET060","CMAST060","CMATT060","CMACT060"))
  tmp <- tmp[, keep]
  # rename 060 -> 60 if present
  names(tmp) <- sub("060$", "60", names(tmp))
  tmp
}, error = function(e) { message("AOP not found - skipping"); NULL })

fav_df <- fav_df %>%
  left_join(riscos %>% select(cd_fcu, tem_risco), by = "cd_fcu")

if (!is.null(aop)) {
  fav_df <- fav_df %>% left_join(aop, by = "cd_fcu")
} else {
  fav_df$CMAET60 <- NA_real_; fav_df$CMAST60 <- NA_real_
  fav_df$CMATT60 <- NA_real_; fav_df$CMACT60 <- NA_real_
}

# =========================================================================
# CAPITAL COORDINATES  (for UF zoom)
# nm_uf must match fav_df$nm_uf exactly - accented Portuguese names
# =========================================================================
capitais <- tibble::tribble(
  ~nm_uf,                  ~lat,     ~lng,  ~zoom,
  "Acre",                 -9.975,  -67.824,   11,
  "Alagoas",              -9.666,  -35.735,   11,
  "Amap\u00e1",            0.034,  -51.066,   11,
  "Amazonas",             -3.119,  -60.021,   11,
  "Bahia",               -12.971,  -38.501,   11,
  "Cear\u00e1",            -3.717,  -38.543,   11,
  "Distrito Federal",    -15.779,  -47.929,   11,
  "Esp\u00edrito Santo",  -20.319,  -40.338,   11,
  "Goi\u00e1s",           -16.686,  -49.264,   11,
  "Maranh\u00e3o",         -2.530,  -44.303,   11,
  "Mato Grosso",         -15.601,  -56.097,   11,
  "Mato Grosso do Sul",  -20.469,  -54.620,   11,
  "Minas Gerais",        -19.917,  -43.934,   11,
  "Par\u00e1",             -1.455,  -48.502,   11,
  "Para\u00edba",          -7.115,  -34.863,   11,
  "Paran\u00e1",          -25.428,  -49.273,   11,
  "Pernambuco",           -8.054,  -34.881,   11,
  "Piau\u00ed",            -5.092,  -42.803,   11,
  "Rio de Janeiro",      -22.906,  -43.173,   11,
  "Rio Grande do Norte",  -5.795,  -35.209,   11,
  "Rio Grande do Sul",   -30.033,  -51.230,   11,
  "Rond\u00f4nia",         -8.761,  -63.902,   11,
  "Roraima",               2.819,  -60.673,   11,
  "Santa Catarina",      -27.595,  -48.548,   11,
  "S\u00e3o Paulo",       -23.550,  -46.633,   11,
  "Sergipe",             -10.916,  -37.073,   11,
  "Tocantins",           -10.249,  -48.324,   11
)

# =========================================================================
# INDICATOR CATALOGUE
# =========================================================================
indicadores <- list(
  list(col = "IDS",       label = "IDS \u2014 \u00cdndice de Desenvolvimento Social",   dir = +1, group = "\u00cdndices"),
  list(col = "IDA",       label = "IDA \u2014 \u00cdndice de Acessibilidade Urbana",    dir = +1, group = "\u00cdndices"),
  list(col = "PERC_AGUA", label = "\u00c1gua encanada (%)",                             dir = +1, group = "Saneamento"),
  list(col = "PERC_ESGO", label = "Esgoto rede geral (%)",                              dir = +1, group = "Saneamento"),
  list(col = "PERC_LIXO", label = "Coleta de lixo (%)",                                dir = +1, group = "Saneamento"),
  list(col = "RENDA_SM",  label = "Renda m\u00e9dia (sal. m\u00edn.)",                  dir = +1, group = "Renda e Educa\u00e7\u00e3o"),
  list(col = "PERC_ANALF",label = "Analfabetismo 15+ (%)",                              dir = -1, group = "Renda e Educa\u00e7\u00e3o"),
  list(col = "P_VIAPAV",  label = "Via pavimentada (%)",                                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_BUEIRO",  label = "Bueiro / boca de lobo (%)",                          dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ILUM",    label = "Ilumina\u00e7\u00e3o p\u00fablica (%)",              dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ONTON",   label = "Ponto de \u00f4nibus (%)",                           dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_VIABIC",  label = "Ciclovia / ciclofaixa (%)",                          dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_CALCAD",  label = "Cal\u00e7ada (%)",                                   dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_OBSTAC",  label = "Obst\u00e1culo na cal\u00e7ada (%)",                 dir = -1, group = "Acessibilidade Urbana"),
  list(col = "P_RAMPA",   label = "Rampa para cadeirante (%)",                          dir = +1, group = "Acessibilidade Urbana"),
  list(col = "tem_risco", label = "Exposi\u00e7\u00e3o a risco natural (flag)",         dir = -1, group = "Riscos Naturais")
)

ind_choices <- setNames(sapply(indicadores, `[[`, "col"),   sapply(indicadores, `[[`, "label"))
ind_groups  <- sapply(indicadores, `[[`, "group")
ind_grouped <- split(ind_choices, ind_groups)
ind_label   <- setNames(sapply(indicadores, `[[`, "label"), sapply(indicadores, `[[`, "col"))

# =========================================================================
# GEO HIERARCHIES
# =========================================================================
ufs        <- sort(unique(na.omit(fav_df$nm_uf)))
municipios <- sort(unique(na.omit(fav_df$nm_mun)))

# Favela search: sorted by population desc, starts empty
fav_search_df <- fav_df[order(-fav_df$total_pessoas, fav_df$nm_fcu), ]
fav_search_choices <- setNames(
  fav_search_df$cd_fcu,
  paste0(fav_search_df$nm_fcu, " - ", fav_search_df$nm_mun, " - ", fav_search_df$nm_uf)
)

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

aop_row_html <- function(label, val) {
  if (is.na(val)) return("")
  sprintf("<tr><td>%s</td><td>%s</td></tr>",
          label, formatC(round(val), format = "d", big.mark = "."))
}

make_popup <- function(sf_obj) {
  fmt_pct <- function(x) ifelse(is.na(x), "\u2014", paste0(round(x, 1), "%"))
  fmt_num <- function(x) ifelse(is.na(x), "\u2014", round(x, 3))
  fmt_sm  <- function(x) ifelse(is.na(x), "\u2014", paste0(round(x, 2), " SM"))

  aop_data <- fav_df %>% select(cd_fcu, CMAET60, CMAST60, CMATT60, CMACT60)

  df_rows <- data.frame(
    nm     = sf_obj$NM_FCU,
    cd_fcu = sf_obj$CD_FCU,
    ids    = sf_obj$IDS,    ida    = sf_obj$IDA,
    agua   = sf_obj$PERC_AGUA, esgo = sf_obj$PERC_ESGO, lixo = sf_obj$PERC_LIXO,
    renda  = sf_obj$RENDA_SM,  analf = sf_obj$PERC_ANALF,
    viapav = sf_obj$P_VIAPAV, bueiro = sf_obj$P_BUEIRO, ilum = sf_obj$P_ILUM,
    onton  = sf_obj$P_ONTON,  viabic = sf_obj$P_VIABIC, calcad = sf_obj$P_CALCAD,
    obstac = sf_obj$P_OBSTAC, rampa  = sf_obj$P_RAMPA,
    risco  = sf_obj$tem_risco, pop   = sf_obj$TOT_PES,
    stringsAsFactors = FALSE
  ) %>% left_join(aop_data, by = "cd_fcu")

  unname(apply(df_rows, 1, function(r) {
    # Correct AOP labels: ET=escolas, ST=saude, TT=empregos, CT=CRAS
    r_aop <- c(
      aop_row_html("Escolas p\u00fablicas acess\u00edveis",
                   suppressWarnings(as.numeric(r["CMAET60"]))),
      aop_row_html("Hospitais/UPAs acess\u00edveis",
                   suppressWarnings(as.numeric(r["CMAST60"]))),
      aop_row_html("Empregos acess\u00edveis",
                   suppressWarnings(as.numeric(r["CMATT60"]))),
      aop_row_html("CRAS acess\u00edveis",
                   suppressWarnings(as.numeric(r["CMACT60"])))
    )
    aop_block <- if (any(r_aop != "")) {
      paste0(
        "<tr><td colspan='2' style='padding:4px 0;'></td></tr>",
        "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>",
        "Acesso a Oportunidades <small>(60 min TP)</small></td></tr>",
        paste(r_aop[r_aop != ""], collapse = "")
      )
    } else { "" }

    sprintf(
      "<div style='font-family:sans-serif;min-width:240px;'>
        <b style='font-size:1.05em;'>%s</b>
        <hr style='margin:4px 0;'>
        <table style='width:100%%;font-size:0.88em;border-collapse:collapse;'>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>\u00cdndices compostos</td></tr>
          <tr><td>\u00cdndice de Desenvolvimento Social</td><td><b>%s</b></td></tr>
          <tr><td>\u00cdndice de Acessibilidade Urbana</td><td><b>%s</b></td></tr>
          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Saneamento</td></tr>
          <tr><td>\u00c1gua encanada</td><td>%s</td></tr>
          <tr><td>Esgoto rede geral</td><td>%s</td></tr>
          <tr><td>Coleta de lixo</td><td>%s</td></tr>
          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Renda e Educa\u00e7\u00e3o</td></tr>
          <tr><td>Renda m\u00e9dia</td><td>%s</td></tr>
          <tr><td>Analfabetismo 15+</td><td>%s</td></tr>
          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Acessibilidade Urbana</td></tr>
          <tr><td>Via pavimentada</td><td>%s</td></tr>
          <tr><td>Bueiro</td><td>%s</td></tr>
          <tr><td>Ilumina\u00e7\u00e3o p\u00fablica</td><td>%s</td></tr>
          <tr><td>Ponto de \u00f4nibus</td><td>%s</td></tr>
          <tr><td>Ciclovia</td><td>%s</td></tr>
          <tr><td>Cal\u00e7ada</td><td>%s</td></tr>
          <tr><td>Obst\u00e1culo cal\u00e7ada</td><td>%s</td></tr>
          <tr><td>Rampa cadeirante</td><td>%s</td></tr>
          %s
          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Informa\u00e7\u00f5es gerais</td></tr>
          <tr><td>Popula\u00e7\u00e3o</td><td>%s</td></tr>
          <tr><td>Risco natural</td><td>%s</td></tr>
        </table>
      </div>",
      r["nm"],
      fmt_num(suppressWarnings(as.numeric(r["ids"]))),
      fmt_num(suppressWarnings(as.numeric(r["ida"]))),
      fmt_pct(suppressWarnings(as.numeric(r["agua"]))),
      fmt_pct(suppressWarnings(as.numeric(r["esgo"]))),
      fmt_pct(suppressWarnings(as.numeric(r["lixo"]))),
      fmt_sm (suppressWarnings(as.numeric(r["renda"]))),
      fmt_pct(suppressWarnings(as.numeric(r["analf"]))),
      fmt_pct(suppressWarnings(as.numeric(r["viapav"]))),
      fmt_pct(suppressWarnings(as.numeric(r["bueiro"]))),
      fmt_pct(suppressWarnings(as.numeric(r["ilum"]))),
      fmt_pct(suppressWarnings(as.numeric(r["onton"]))),
      fmt_pct(suppressWarnings(as.numeric(r["viabic"]))),
      fmt_pct(suppressWarnings(as.numeric(r["calcad"]))),
      fmt_pct(suppressWarnings(as.numeric(r["obstac"]))),
      fmt_pct(suppressWarnings(as.numeric(r["rampa"]))),
      aop_block,
      formatC(suppressWarnings(as.integer(r["pop"])), format = "d", big.mark = "."),
      ifelse(is.na(r["risco"]), "\u2014",
             ifelse(r["risco"] == "1" | r["risco"] == "TRUE", "Sim", "N\u00e3o"))
    )
  }))
}

# =========================================================================
# UI
# =========================================================================
ui <- page_navbar(
  title    = "Favela Dados",
  theme    = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  fillable = TRUE,

  sidebar = sidebar(
    width = 270,
    title = "Filtros",

    selectizeInput("sel_favela_search",
      label   = "Buscar favela",
      choices = NULL,
      options = list(
        placeholder = "Digite o nome da favela...",
        maxOptions  = 30
      )
    ),
    hr(),
    selectInput("sel_uf",  "Estado (UF)",
                choices = c("Todos" = "", ufs), selected = "", multiple = TRUE),
    selectInput("sel_mun", "Munic\u00edpio",
                choices = c("Todos" = "", municipios), selected = "", multiple = TRUE),
    hr(),
    selectInput("sel_ind", "Indicador (cor no mapa)",
                choices = ind_grouped, selected = "IDS"),
    hr(),
    p(tags$small(tags$i("Dados: Censo IBGE 2022 \u00b7 IBGE FCU 2022 \u00b7 SGB \u00b7 AOP IPEA 2019")))
  ),

  nav_panel(
    title = tagList(icon("map"), " Mapa"),
    card(full_screen = TRUE,
      card_header(textOutput("mapa_titulo")),
      uiOutput("mapa_ui")
    )
  ),

  nav_panel(
    title = tagList(icon("chart-bar"), " Descritivas"),
    layout_columns(col_widths = c(12),
      card(
        card_header(
          layout_columns(col_widths = c(8, 4),
            textOutput("desc_titulo"),
            checkboxInput("show_worst", "Mostrar piores 20", value = FALSE)
          )
        ),
        card_body(plotOutput("chart_top20", height = "480px"))
      )
    ),
    layout_columns(col_widths = c(12),
      card(
        card_header("Distribui\u00e7\u00e3o do indicador"),
        card_body(plotOutput("chart_hist", height = "260px"))
      )
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
  ),

  nav_panel(
    title = tagList(icon("info-circle"), " Metadados"),
    card(card_body(
      tags$h4("Sobre os dados"),
      tags$hr(),
      tags$h5("Pol\u00edgonos de Favelas (FCU)"),
      tags$p("Fonte: IBGE, Censo 2022 \u2014 12.348 Favelas e Comunidades Urbanas (FCUs)."),
      tags$h5("Censo IBGE 2022 \u2014 Setores Cens\u00editarios"),
      tags$ul(
        tags$li(tags$b("Saneamento:"), " % de domic\u00edlios com \u00e1gua encanada, esgoto em rede geral e coleta de lixo."),
        tags$li(tags$b("Renda:"), " renda m\u00e9dia do respons\u00e1vel em sal\u00e1rios m\u00ednimos (SM = R$ 1.212, ref. 2022)."),
        tags$li(tags$b("Educa\u00e7\u00e3o:"), " % da popula\u00e7\u00e3o de 15+ anos analfabeta."),
        tags$li(tags$b("Entorno:"), " % de faces de quadra com via pavimentada, bueiro, ilumina\u00e7\u00e3o, ponto de \u00f4nibus, ciclovia, cal\u00e7ada, obst\u00e1culo e rampa.")
      ),
      tags$h5("\u00cdndices Compostos"),
      tags$ul(
        tags$li(tags$b("IDS:"), " m\u00e9dia de 6 componentes min-max normalizados: \u00e1gua, esgoto, lixo, banheiros por morador, alfabetiza\u00e7\u00e3o e renda."),
        tags$li(tags$b("IDA:"), " m\u00e9dia de 8 componentes normalizados: via pavimentada, bueiro, ilumina\u00e7\u00e3o, ponto de \u00f4nibus, ciclovia, cal\u00e7ada, obst\u00e1culo (invertido) e rampa.")
      ),
      tags$h5("Riscos Naturais \u2014 SGB/CPRM"),
      tags$p("Vari\u00e1vel bin\u00e1ria: 1 se o pol\u00edgono FCU intersecta \u00e1rea de risco geol\u00f3gico mapeada pelo SGB."),
      tags$h5("Acesso a Oportunidades \u2014 AOP (aopdata / IPEA, 2019)"),
      tags$p("Dispon\u00edvel para 20 munic\u00edpios. Aparecem no popup ao clicar em uma favela."),
      tags$p(tags$em("Bel\u00e9m, Belo Horizonte, Bras\u00edlia, Campinas, Campo Grande, Curitiba, Duque de Caxias, Fortaleza, Goi\u00e2nia, Guarulhos, Macei\u00f3, Manaus, Natal, Porto Alegre, Recife, Rio de Janeiro, Salvador, S\u00e3o Gon\u00e7alo, S\u00e3o Lu\u00eds, S\u00e3o Paulo.")),
      tags$ul(
        tags$li(tags$b("CMAET60:"), " escolas p\u00fablicas acess\u00edveis em \u2264 60 min por TP (ET = ensino, todas as etapas)."),
        tags$li(tags$b("CMAST60:"), " hospitais e UPAs acess\u00edveis em \u2264 60 min por TP (ST = sa\u00fade, todas as complexidades)."),
        tags$li(tags$b("CMATT60:"), " empregos formais acess\u00edveis em \u2264 60 min por TP (TT = todos os empregos), pico da manh\u00e3."),
        tags$li(tags$b("CMACT60:"), " CRAS acess\u00edveis em \u2264 60 min por TP (CT = centros de assist\u00eancia social).")
      )
    ))
  )
)

# =========================================================================
# SERVER
# =========================================================================
server <- function(input, output, session) {

  # Favela search: server-side, starts empty
  updateSelectizeInput(session, "sel_favela_search",
    choices  = c("" = "", fav_search_choices),
    selected = "",
    server   = TRUE
  )
  # Update municipalities when UF filter changes
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
  # Mapa
  # -----------------------------------------------------------------------
  output$mapa_titulo <- renderText({
    search_active <- !is.null(input$sel_favela_search) &&
                     nchar(input$sel_favela_search) > 0
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")
    if (!search_active && !uf_active)
      "Mapa \u2014 selecione um Estado ou busque uma favela"
    else
      paste0("Mapa \u2014 ", ind_nome(), " | ", nrow(dados_filtrados()), " favelas")
  })

  output$mapa_ui <- renderUI({
    search_active <- !is.null(input$sel_favela_search) &&
                     nchar(input$sel_favela_search) > 0
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")
    if (!search_active && !uf_active) {
      div(
        style = "height:680px;display:flex;flex-direction:column;align-items:center;justify-content:center;background:#f8f9fa;border-radius:6px;color:#6c757d;",
        tags$i(class = "fa fa-map fa-3x", style = "margin-bottom:16px;color:#adb5bd;"),
        tags$h5("Selecione um Estado ou busque uma favela",
                style = "font-weight:500;margin-bottom:8px;"),
        tags$p("Use os filtros na barra lateral.", style = "font-size:0.9rem;")
      )
    } else {
      leafletOutput("mapa", width = "100%", height = "680px")
    }
  })

  output$mapa <- renderLeaflet({
    search_active <- !is.null(input$sel_favela_search) &&
                     nchar(input$sel_favela_search) > 0
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")
    req(search_active || uf_active)

    sf_obj <- sf_filtrado()
    col    <- ind_col()
    nome   <- ind_nome()
    vals   <- sf_obj[[col]]
    pal    <- colorNumeric(viridis(100), domain = vals, na.color = "#CCCCCC")
    popups <- make_popup(sf_obj)

    uf_sel   <- if (uf_active) input$sel_uf[1] else NULL
    cap      <- if (!is.null(uf_sel)) capitais[capitais$nm_uf == uf_sel, ] else NULL
    map_lat  <- if (!is.null(cap) && nrow(cap) > 0) cap$lat[1]  else -15
    map_lng  <- if (!is.null(cap) && nrow(cap) > 0) cap$lng[1]  else -50
    map_zoom <- if (!is.null(cap) && nrow(cap) > 0) cap$zoom[1] else  5

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
        layerId     = ~CD_FCU,
        highlightOptions = highlightOptions(
          weight = 2, color = "#FFD700",
          fillOpacity = 0.95, bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = pal, values = vals, title = nome,
                opacity = 0.9, position = "bottomright")
  })

  # UF zoom
  observeEvent(input$sel_uf, {
    req(length(input$sel_uf) > 0, !all(input$sel_uf == ""))
    cap <- capitais[capitais$nm_uf == input$sel_uf[1], ]
    if (nrow(cap) > 0)
      leafletProxy("mapa") %>%
        setView(lng = cap$lng[1], lat = cap$lat[1], zoom = cap$zoom[1])
  }, ignoreInit = TRUE)

  # Favela search: set UF filter then zoom + open popup
  observeEvent(input$sel_favela_search, {
    req(!is.null(input$sel_favela_search), nchar(input$sel_favela_search) > 0)
    cd  <- input$sel_favela_search
    row <- fav_df[fav_df$cd_fcu == cd, ]
    req(nrow(row) > 0)

    uf_of_fav   <- row$nm_uf[1]
    current_ufs <- isolate(input$sel_uf)
    if (is.null(current_ufs) || length(current_ufs) == 0 ||
        !uf_of_fav %in% current_ufs) {
      updateSelectInput(session, "sel_uf", selected = uf_of_fav)
    }

    fav_row <- fav_sf[fav_sf$CD_FCU == cd, ]
    req(nrow(fav_row) > 0)
    ctr    <- suppressWarnings(st_centroid(st_geometry(fav_row)))
    coords <- st_coordinates(ctr)
    req(nrow(coords) > 0)

    # Build popup for this single favela
    popup_html <- make_popup(fav_row)

    leafletProxy("mapa") %>%
      setView(lng = coords[1, 1], lat = coords[1, 2], zoom = 16) %>%
      addPopups(
        lng    = coords[1, 1],
        lat    = coords[1, 2],
        popup  = popup_html[1],
        options = popupOptions(maxWidth = 320, closeOnClick = TRUE)
      )
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Descritivas
  # -----------------------------------------------------------------------
  chart_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 9))

  output$desc_titulo <- renderText({
    worst_flag <- isTRUE(input$show_worst)
    prefix <- if (worst_flag) "Piores 20" else "Top 20"
    paste0(prefix, " \u2014 ", ind_nome())
  })

  output$chart_top20 <- renderPlot({
    col   <- ind_col()
    nome  <- ind_nome()
    df    <- dados_filtrados()
    worst_flag <- isTRUE(input$show_worst)
    if (!col %in% names(df)) return(NULL)

    df_clean <- df[!is.na(df[[col]]), ]
    df_ord   <- df_clean[order(df_clean[[col]], decreasing = !worst_flag), ]
    df_plot  <- df_ord[seq_len(min(20, nrow(df_ord))), ]
    df_plot$bar_label <- paste0(df_plot$nm_fcu, " (", df_plot$nm_mun, ")")
    df_plot$bar_label <- fct_reorder(df_plot$bar_label, df_plot[[col]])

    pal_opt <- if (worst_flag) "B" else "D"
    cap_txt <- if (worst_flag) "20 favelas com menor valor na sele\u00e7\u00e3o atual" else "20 favelas com maior valor na sele\u00e7\u00e3o atual"

    ggplot(df_plot, aes(x = .data[[col]], y = bar_label, fill = .data[[col]])) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(.data[[col]], 2)), hjust = -0.15, size = 3) +
      scale_fill_viridis_c(option = pal_opt) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = nome, y = NULL, caption = cap_txt) +
      chart_theme
  })

  output$chart_hist <- renderPlot({
    col  <- ind_col(); nome <- ind_nome(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    med <- median(df[[col]], na.rm = TRUE)
    ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = 40, fill = "#3B82F6", colour = "white", linewidth = 0.3) +
      geom_vline(xintercept = med, colour = "tomato", linewidth = 1, linetype = "dashed") +
      annotate("text", x = med, y = Inf, vjust = 2, hjust = -0.1,
               label = paste0("Mediana: ", round(med, 2)),
               colour = "tomato", size = 3.5) +
      labs(x = nome, y = "N\u00ba de favelas") + theme_minimal(base_size = 12)
  })

  # -----------------------------------------------------------------------
  # -----------------------------------------------------------------------
  # Dados
  # -----------------------------------------------------------------------
  output$dados_titulo <- renderText({
    paste0(nrow(dados_filtrados()), " favelas selecionadas")
  })

  tabela_export <- reactive({
    dados_filtrados() %>%
      select(cd_fcu, nm_fcu, nm_mun, nm_uf, total_pessoas, total_dp_ocupados,
             perc_agua_adequada, perc_esgoto_adequado, perc_lixo_coleta,
             renda_sm_pond, perc_analfabeto_populacao,
             perc_via_pavimentada, perc_bueiro, perc_iluminacao_publica,
             perc_ponto_onibus, perc_via_bicicleta, perc_calcada,
             perc_obstaculo_calcada, perc_rampa_cadeirante,
             IDS, IDA,
             any_of(c("CMAET60","CMAST60","CMATT60","CMACT60","tem_risco"))) %>%
      rename(
        "C\u00f3digo FCU"             = cd_fcu,
        "Nome da favela"         = nm_fcu,
        "Munic\u00edpio"              = nm_mun,
        "UF"                     = nm_uf,
        "Popula\u00e7\u00e3o"              = total_pessoas,
        "Domic\u00edlios ocupados"    = total_dp_ocupados,
        "\u00c1gua encanada (%)"      = perc_agua_adequada,
        "Esgoto rede geral (%)"  = perc_esgoto_adequado,
        "Coleta de lixo (%)"     = perc_lixo_coleta,
        "Renda m\u00e9dia (SM)"       = renda_sm_pond,
        "Analfabetismo 15+ (%)"  = perc_analfabeto_populacao,
        "Via pavimentada (%)"    = perc_via_pavimentada,
        "Bueiro (%)"             = perc_bueiro,
        "Ilumina\u00e7\u00e3o p\u00fablica (%)" = perc_iluminacao_publica,
        "Ponto de \u00f4nibus (%)"    = perc_ponto_onibus,
        "Ciclovia (%)"           = perc_via_bicicleta,
        "Cal\u00e7ada (%)"            = perc_calcada,
        "Obst\u00e1culo cal\u00e7ada (%)"  = perc_obstaculo_calcada,
        "Rampa cadeirante (%)"   = perc_rampa_cadeirante,
        "IDS"                    = IDS,
        "IDA"                    = IDA
      )
  })

  output$tabela_dados <- renderDT({
    datatable(tabela_export(), rownames = FALSE, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "frtip")) %>%
      formatRound(columns = c("IDS", "IDA", "Renda m\u00e9dia (SM)"), digits = 3) %>%
      formatRound(
        columns = c("\u00c1gua encanada (%)", "Esgoto rede geral (%)", "Coleta de lixo (%)",
                    "Analfabetismo 15+ (%)", "Via pavimentada (%)", "Bueiro (%)",
                    "Ilumina\u00e7\u00e3o p\u00fablica (%)", "Ponto de \u00f4nibus (%)", "Ciclovia (%)",
                    "Cal\u00e7ada (%)", "Obst\u00e1culo cal\u00e7ada (%)", "Rampa cadeirante (%)"),
        digits = 1) %>%
      formatCurrency(
        columns = c("Popula\u00e7\u00e3o", "Domic\u00edlios ocupados"),
        currency = "", interval = 3, mark = ".", digits = 0)
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      uf_str  <- if (length(input$sel_uf)  > 0 && !all(input$sel_uf  == ""))
        paste0("_", paste(input$sel_uf,  collapse = "-")) else ""
      mun_str <- if (length(input$sel_mun) > 0 && !all(input$sel_mun == ""))
        paste0("_", paste(input$sel_mun, collapse = "-")) else ""
      paste0("favelas_br", uf_str, mun_str, "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) { write_csv2(tabela_export(), file) }
  )
}

shinyApp(ui, server)
