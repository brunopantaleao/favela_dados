# Favelas BR — Plataforma de Análise

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

aop <- tryCatch(
  read_csv2(CSV_AOP_URL, show_col_types = FALSE) %>%
    mutate(cd_fcu = as.character(cd_fcu)) %>%
    select(cd_fcu, any_of(c("CMAET60","CMAST60","CMATT60","CMACT60",
                             "CMAET060","CMAST060","CMATT060","CMACT060"))) %>%
    rename_with(~ gsub("0$", "", .x), ends_with("060")),
  error = function(e) { message("AOP file not found — skipping."); NULL }
)

fav_df <- fav_df %>%
  left_join(riscos %>% select(cd_fcu, tem_risco), by = "cd_fcu")

if (!is.null(aop)) {
  fav_df <- fav_df %>% left_join(aop, by = "cd_fcu")
} else {
  fav_df$CMAET60 <- NA_real_; fav_df$CMAST60 <- NA_real_
  fav_df$CMATT60 <- NA_real_; fav_df$CMACT60 <- NA_real_
}

# =========================================================================
# COLUMN NAME MAPPING: GeoJSON short names -> fav_df long names
# The indicator catalogue uses GeoJSON names; fav_df uses Census long names.
# =========================================================================
ind_col_map <- c(
  IDS        = "IDS",
  IDA        = "IDA",
  PERC_AGUA  = "perc_agua_adequada",
  PERC_ESGO  = "perc_esgoto_adequado",
  PERC_LIXO  = "perc_lixo_coleta",
  RENDA_SM   = "renda_sm_pond",
  PERC_ANALF = "perc_analfabeto_populacao",
  P_VIAPAV   = "perc_via_pavimentada",
  P_BUEIRO   = "perc_bueiro",
  P_ILUM     = "perc_iluminacao_publica",
  P_ONTON    = "perc_ponto_onibus",
  P_VIABIC   = "perc_via_bicicleta",
  P_CALCAD   = "perc_calcada",
  P_OBSTAC   = "perc_obstaculo_calcada",
  P_RAMPA    = "perc_rampa_cadeirante",
  tem_risco  = "tem_risco"
)

# Radar vars use fav_df long names directly
radar_vars <- c(
  "IDS"                     = "IDS",
  "IDA"                     = "IDA",
  "perc_agua_adequada"      = "Água",
  "perc_esgoto_adequado"    = "Esgoto",
  "perc_lixo_coleta"        = "Lixo",
  "renda_sm_pond"           = "Renda",
  "perc_via_pavimentada"    = "Via Pav.",
  "perc_iluminacao_publica" = "Iluminação",
  "perc_calcada"            = "Calçada",
  "perc_ponto_onibus"       = "Ponto Ônibus",
  "perc_analfabeto_populacao" = "Analfab."
)

# =========================================================================
# MUNICIPALITY CENTROIDS (tribble — no locale/encoding issues)
# =========================================================================
mun_centroids <- tibble::tribble(
  ~nm_mun,                        ~lat,       ~lng,
  "Alta Floresta D'Oeste",   -12.47013,  -62.27466,
  "Ariquemes",                -9.95189,  -62.95726,
  "Porto Velho",              -9.15359,  -64.30681,
  "Vilhena",                 -12.09562,  -60.24842,
  "Rio Branco",              -10.06613,  -68.37106,
  "Manaus",                   -2.62592,  -60.25963,
  "Boa Vista",                 3.11792,  -60.71795,
  "Belem",                    -1.24072,  -48.45991,
  "Ananindeua",               -1.33408,  -48.38354,
  "Santarem",                 -2.67934,  -55.23846,
  "Maraba",                   -5.62980,  -50.01696,
  "Macapa",                    0.56275,  -50.69178,
  "Palmas",                  -10.22018,  -48.15209,
  "Sao Luis",                 -2.63369,  -44.28090,
  "Imperatriz",               -5.33977,  -47.57524,
  "Teresina",                 -5.10266,  -42.74061,
  "Parnaiba",                 -2.95916,  -41.75344,
  "Fortaleza",                -3.78583,  -38.52800,
  "Caucaia",                  -3.78359,  -38.80969,
  "Juazeiro do Norte",        -7.18281,  -39.28616,
  "Sobral",                   -3.81104,  -40.22787,
  "Natal",                    -5.80317,  -35.22884,
  "Mossoro",                  -5.17581,  -37.32553,
  "Joao Pessoa",              -7.16547,  -34.86953,
  "Campina Grande",           -7.26355,  -35.96566,
  "Recife",                   -8.03934,  -34.93309,
  "Caruaru",                  -8.18052,  -36.01663,
  "Olinda",                   -7.99310,  -34.86637,
  "Jaboatao dos Guararapes",  -8.15216,  -35.00335,
  "Maceio",                   -9.52239,  -35.71139,
  "Aracaju",                 -10.99420,  -37.09491,
  "Salvador",                -12.87349,  -38.51472,
  "Feira de Santana",        -12.19308,  -39.03399,
  "Vitoria da Conquista",    -15.02228,  -40.91316,
  "Belo Horizonte",          -19.90268,  -43.95998,
  "Contagem",                -19.88718,  -44.08400,
  "Juiz de Fora",            -21.74550,  -43.46473,
  "Uberlandia",              -19.02777,  -48.33174,
  "Montes Claros",           -16.62072,  -43.92882,
  "Vitoria",                 -20.30401,  -39.08796,
  "Vila Velha",              -20.43408,  -40.37826,
  "Serra",                   -20.12812,  -40.30162,
  "Cariacica",               -20.29069,  -40.44205,
  "Rio de Janeiro",          -22.92319,  -43.45099,
  "Sao Goncalo",             -22.82562,  -42.99701,
  "Duque de Caxias",         -22.63249,  -43.29961,
  "Nova Iguacu",             -22.68676,  -43.50184,
  "Belford Roxo",            -22.72873,  -43.37757,
  "Sao Paulo",               -23.65008,  -46.64810,
  "Guarulhos",               -23.40269,  -46.45488,
  "Campinas",                -22.88376,  -47.04380,
  "Sao Bernardo do Campo",   -23.81299,  -46.55079,
  "Santo Andre",             -23.72796,  -46.44159,
  "Osasco",                  -23.52875,  -46.78927,
  "Ribeirao Preto",          -21.21084,  -47.82130,
  "Sorocaba",                -23.46456,  -47.44677,
  "Sao Jose dos Campos",     -23.09057,  -45.92854,
  "Santos",                  -23.86903,  -46.29153,
  "Maua",                    -23.66616,  -46.44640,
  "Sao Jose do Rio Preto",   -20.79723,  -49.35811,
  "Mogi das Cruzes",         -23.56958,  -46.18607,
  "Diadema",                 -23.69721,  -46.61143,
  "Jundiai",                 -23.19460,  -46.91301,
  "Piracicaba",              -22.72646,  -47.78402,
  "Bauru",                   -22.25399,  -49.12613,
  "Carapicuiba",             -23.55004,  -46.84193,
  "Franca",                  -20.55522,  -47.38111,
  "Itaquaquecetuba",         -23.46146,  -46.33388,
  "Curitiba",                -25.47791,  -49.28824,
  "Londrina",                -23.51425,  -51.11038,
  "Maringa",                 -23.40095,  -51.96781,
  "Ponta Grossa",            -25.13970,  -50.08079,
  "Cascavel",                -25.02778,  -53.37955,
  "Sao Jose dos Pinhais",    -25.66436,  -49.09495,
  "Foz do Iguacu",           -25.46797,  -54.48324,
  "Colombo",                 -25.30634,  -49.18803,
  "Guarapuava",              -25.37135,  -51.49124,
  "Paranagua",               -25.52721,  -48.51766,
  "Florianopolis",           -27.57783,  -48.50820,
  "Joinville",               -26.24428,  -48.95141,
  "Blumenau",                -26.88577,  -49.09731,
  "Sao Jose",                -27.57847,  -48.65626,
  "Criciuma",                -28.71570,  -49.37972,
  "Itajai",                  -26.96901,  -48.75342,
  "Chapeco",                 -27.12514,  -52.65034,
  "Porto Alegre",            -30.09532,  -51.16453,
  "Caxias do Sul",           -29.10219,  -51.02367,
  "Pelotas",                 -31.58111,  -52.34120,
  "Canoas",                  -29.91222,  -51.17965,
  "Santa Maria",             -29.78492,  -53.82504,
  "Novo Hamburgo",           -29.73475,  -51.04904,
  "Sao Leopoldo",            -29.75533,  -51.14485,
  "Viamao",                  -30.16701,  -50.86937,
  "Alvorada",                -29.99498,  -51.03742,
  "Gravitai",                -29.88947,  -50.94705,
  "Campo Grande",            -20.91358,  -54.24946,
  "Dourados",                -22.14486,  -54.83888,
  "Corumba",                 -18.72235,  -56.72226,
  "Cuiaba",                  -15.59279,  -55.81823,
  "Varzea Grande",           -15.56248,  -56.24275,
  "Rondonopolis",            -16.56749,  -54.68480,
  "Goiania",                 -16.64355,  -49.27378,
  "Aparecida de Goiania",    -16.80999,  -49.26248,
  "Anapolis",                -16.29058,  -48.97289,
  "Brasilia",                -15.78117,  -47.79685
)

# =========================================================================
# CAPITAL COORDINATES
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
# INDICATOR CATALOGUE  (AOP not in dropdown — shown in popup only)
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
  list(col = "tem_risco", label = "Exposição a risco natural (flag)",         dir = -1, group = "Riscos Naturais")
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

# Favela search choices: sorted by population desc (with safe fallback)
fav_search_choices <- tryCatch({
  df_sorted <- if ("total_pessoas" %in% names(fav_df)) {
    fav_df[order(-fav_df$total_pessoas, fav_df$nm_fcu), ]
  } else {
    fav_df[order(fav_df$nm_fcu), ]
  }
  lbl <- paste0(df_sorted$nm_fcu, " - ", df_sorted$nm_mun, " - ", df_sorted$nm_uf)
  setNames(df_sorted$cd_fcu, lbl)
}, error = function(e) {
  message("fav_search_choices error: ", e$message)
  setNames(fav_df$cd_fcu, fav_df$nm_fcu)
})

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

# Translate GeoJSON indicator code to fav_df column name
to_df_col <- function(ind) {
  if (ind %in% names(ind_col_map)) ind_col_map[[ind]] else ind
}

aop_row_html <- function(label, val) {
  if (is.na(val)) return("")
  sprintf("<tr><td>%s</td><td>%s</td></tr>",
          label, formatC(round(val), format = "d", big.mark = "."))
}

make_popup <- function(sf_obj) {
  fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(round(x, 1), "%"))
  fmt_num <- function(x) ifelse(is.na(x), "—", round(x, 3))
  fmt_sm  <- function(x) ifelse(is.na(x), "—", paste0(round(x, 2), " SM"))

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

  apply(df_rows, 1, function(r) {
    r_aop <- c(
      aop_row_html("Escolas públicas acessíveis",  suppressWarnings(as.numeric(r["CMAET60"]))),
      aop_row_html("Hospitais/UPAs acessíveis",    suppressWarnings(as.numeric(r["CMAST60"]))),
      aop_row_html("Empregos acessíveis",          suppressWarnings(as.numeric(r["CMATT60"]))),
      aop_row_html("CRAS acessíveis",              suppressWarnings(as.numeric(r["CMACT60"])))
    )
    aop_block <- if (any(r_aop != "")) {
      paste0(
        "<tr><td colspan='2' style='padding:4px 0;'></td></tr>",
        "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>",
        "Acesso a Oportunidades <small>(60 min TP)</small></td></tr>",
        paste(r_aop[r_aop != ""], collapse = "")
      )
    } else ""

    sprintf(
      "<div style='font-family:sans-serif;min-width:240px;'>
        <b style='font-size:1.05em;'>%s</b>
        <hr style='margin:4px 0;'>
        <table style='width:100%%;font-size:0.88em;border-collapse:collapse;'>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Índices compostos</td></tr>
          <tr><td>Índice de Desenvolvimento Social</td><td><b>%s</b></td></tr>
          <tr><td>Índice de Acessibilidade Urbana</td><td><b>%s</b></td></tr>
          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Saneamento</td></tr>
          <tr><td>Água encanada</td><td>%s</td></tr>
          <tr><td>Esgoto rede geral</td><td>%s</td></tr>
          <tr><td>Coleta de lixo</td><td>%s</td></tr>
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
          %s
          <tr><td colspan='2' style='padding:4px 0;'></td></tr>
          <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Informações gerais</td></tr>
          <tr><td>População</td><td>%s</td></tr>
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
      ifelse(is.na(r["risco"]), "—", ifelse(r["risco"] == "1", "Sim", "Não"))
    )
  })
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
    selectInput("sel_mun", "Município",
                choices = c("Todos" = "", municipios), selected = "", multiple = TRUE),
    hr(),
    selectInput("sel_ind", "Indicador (cor no mapa)",
                choices = ind_grouped, selected = "IDS"),
    hr(),
    p(tags$small(tags$i("Dados: Censo IBGE 2022 · IBGE FCU 2022 · SGB · AOP IPEA 2019")))
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
        card_header("Distribuição do indicador"),
        card_body(plotOutput("chart_hist", height = "260px"))
      )
    )
  ),

  nav_panel(
    title = tagList(icon("circle-nodes"), " Comparar"),
    card(
      card_header("Comparação em radar — selecione 2 a 5 favelas"),
      card_body(
        selectizeInput("sel_favelas_radar",
          label    = "Favelas para comparar",
          choices  = NULL,
          multiple = TRUE,
          options  = list(maxItems = 5, placeholder = "Digite o nome da favela...")
        ),
        plotOutput("chart_radar", height = "520px")
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
      tags$h5("Polígonos de Favelas (FCU)"),
      tags$p("Fonte: IBGE, Censo 2022 — 12.348 Favelas e Comunidades Urbanas (FCUs) em todo o território nacional."),
      tags$h5("Censo IBGE 2022 — Setores Censitários"),
      tags$p("Dados agregados por setor censitário, vinculados aos polígonos FCU via tabela de correspondência IBGE."),
      tags$ul(
        tags$li(tags$b("Saneamento:"), " % de domicílios com água encanada, esgoto em rede geral e coleta de lixo."),
        tags$li(tags$b("Renda:"), " renda média do responsável em salários mínimos (SM = R$ 1.212, ref. 2022)."),
        tags$li(tags$b("Educação:"), " % da população de 15+ anos analfabeta."),
        tags$li(tags$b("Entorno:"), " % de faces de quadra com via pavimentada, bueiro, iluminação, ponto de ônibus, ciclovia, calçada, obstáculo e rampa para cadeirante.")
      ),
      tags$h5("Índices Compostos"),
      tags$ul(
        tags$li(tags$b("IDS:"), " média de 6 componentes min-max normalizados: água, esgoto, lixo, banheiros por morador, alfabetização e renda."),
        tags$li(tags$b("IDA:"), " média de 8 componentes normalizados: via pavimentada, bueiro, iluminação, ponto de ônibus, ciclovia, calçada, obstáculo (invertido) e rampa.")
      ),
      tags$h5("Riscos Naturais — SGB/CPRM"),
      tags$p("Variável binária: 1 se o polígono FCU intersecta área de risco geológico mapeada pelo Serviço Geológico Brasileiro."),
      tags$h5("Acesso a Oportunidades — AOP (aopdata / IPEA, 2019)"),
      tags$p("Dados disponíveis para 20 municípios. Favelas em outros municípios exibem NA. Aparecem no popup ao clicar em uma favela."),
      tags$p(tags$em("Belém, Belo Horizonte, Brasília, Campinas, Campo Grande, Curitiba, Duque de Caxias, Fortaleza, Goiânia, Guarulhos, Maceió, Manaus, Natal, Porto Alegre, Recife, Rio de Janeiro, Salvador, São Gonçalo, São Luís, São Paulo.")),
      tags$ul(
        tags$li(tags$b("CMAET60:"), " escolas públicas acessíveis em ≤ 60 min por transporte público (todas as etapas)."),
        tags$li(tags$b("CMAST60:"), " hospitais e UPAs acessíveis em ≤ 60 min por TP (todas as complexidades)."),
        tags$li(tags$b("CMATT60:"), " empregos formais acessíveis em ≤ 60 min por TP (todos os níveis), pico da manhã."),
        tags$li(tags$b("CMACT60:"), " CRAS acessíveis em ≤ 60 min por TP.")
      ),
      tags$p("Nota: para cidades com apenas transporte ativo (caminhada/bicicleta) disponível no AOP, os valores por TP podem ser NA mesmo para cidades na lista acima."),
      tags$h5("Nota metodológica"),
      tags$p("Indicadores de setor censitário foram agregados ao nível FCU por média ponderada pela população. Normalização min-max calculada sobre setores urbanos de municípios com pelo menos uma favela.")
    ))
  )
)

# =========================================================================
# SERVER
# =========================================================================
server <- function(input, output, session) {

  # Populate favela search (server-side, starts empty)
  updateSelectizeInput(session, "sel_favela_search",
    choices  = c("" = "", fav_search_choices),
    selected = "",
    server   = TRUE
  )

  # Update municipalities when UF changes
  observeEvent(input$sel_uf, {
    muns_filtrados <- if (length(input$sel_uf) > 0 && !all(input$sel_uf == "")) {
      fav_df %>% filter(nm_uf %in% input$sel_uf) %>% pull(nm_mun) %>% unique() %>% sort()
    } else { municipios }
    updateSelectInput(session, "sel_mun",
      choices  = c("Todos" = "", muns_filtrados),
      selected = input$sel_mun[input$sel_mun %in% muns_filtrados]
    )
  })

  # Populate radar favela picker (server-side, all favelas)
  observe({
    df_r  <- if ("total_pessoas" %in% names(fav_df)) {
      fav_df[order(-fav_df$total_pessoas, fav_df$nm_fcu), ]
    } else {
      fav_df[order(fav_df$nm_fcu), ]
    }
    lbl_r <- paste0(df_r$nm_fcu, " - ", df_r$nm_mun, " - ", df_r$nm_uf)
    choices_radar <- setNames(df_r$cd_fcu, lbl_r)
    updateSelectizeInput(session, "sel_favelas_radar",
      choices  = choices_radar,
      selected = NULL,
      server   = TRUE
    )
  })

  dados_filtrados <- reactive({ filter_data(fav_df, input$sel_uf, input$sel_mun) })
  sf_filtrado     <- reactive({ filter_sf(fav_sf,  input$sel_uf, input$sel_mun) })
  ind_col         <- reactive({ input$sel_ind })
  ind_nome        <- reactive({ ind_label[[input$sel_ind]] })
  # The fav_df column name corresponding to the selected indicator
  ind_df_col      <- reactive({ to_df_col(input$sel_ind) })

  # -----------------------------------------------------------------------
  # Tab: Mapa
  # -----------------------------------------------------------------------
  output$mapa_titulo <- renderText({
    search_active <- !is.null(input$sel_favela_search) &&
                     length(input$sel_favela_search) > 0 &&
                     input$sel_favela_search != ""
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")
    if (!search_active && !uf_active)
      "Mapa — selecione um Estado ou busque uma favela"
    else
      paste0("Mapa — ", ind_nome(), " | ", nrow(dados_filtrados()), " favelas")
  })

  output$mapa_ui <- renderUI({
    search_active <- !is.null(input$sel_favela_search) &&
                     length(input$sel_favela_search) > 0 &&
                     input$sel_favela_search != ""
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")

    if (!search_active && !uf_active) {
      div(
        style = "height:680px;display:flex;flex-direction:column;align-items:center;justify-content:center;background:#f8f9fa;border-radius:6px;color:#6c757d;",
        tags$i(class = "fa fa-map fa-3x", style = "margin-bottom:16px;color:#adb5bd;"),
        tags$h5("Selecione um Estado ou busque uma favela", style = "font-weight:500;margin-bottom:8px;"),
        tags$p("Use os filtros na barra lateral.", style = "font-size:0.9rem;")
      )
    } else {
      leafletOutput("mapa", width = "100%", height = "680px")
    }
  })

  output$mapa <- renderLeaflet({
    search_active <- !is.null(input$sel_favela_search) &&
                     length(input$sel_favela_search) > 0 &&
                     input$sel_favela_search != ""
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")
    req(search_active || uf_active)

    sf_obj <- sf_filtrado()
    col    <- ind_col()
    nome   <- ind_nome()
    vals   <- sf_obj[[col]]
    pal    <- colorNumeric(viridis(100), domain = vals, na.color = "#CCCCCC")
    popups <- unname(make_popup(sf_obj))

    uf_sel   <- if (uf_active) input$sel_uf[1] else NULL
    cap      <- if (!is.null(uf_sel)) capitais %>% filter(nm_uf == uf_sel) else NULL
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

  # Auto-zoom on UF change
  observeEvent(input$sel_uf, {
    req(length(input$sel_uf) > 0, !all(input$sel_uf == ""))
    cap <- capitais %>% filter(nm_uf == input$sel_uf[1])
    if (nrow(cap) > 0)
      leafletProxy("mapa") %>% setView(lng = cap$lng[1], lat = cap$lat[1], zoom = cap$zoom[1])
  }, ignoreInit = TRUE)

  # Auto-zoom on municipality change
  observeEvent(input$sel_mun, {
    req(length(input$sel_mun) > 0, !all(input$sel_mun == ""))
    mun_q  <- input$sel_mun[1]
    centro <- mun_centroids %>% filter(
      tolower(iconv(nm_mun, to="ASCII//TRANSLIT")) ==
      tolower(iconv(mun_q,  to="ASCII//TRANSLIT"))
    )
    if (nrow(centro) > 0)
      leafletProxy("mapa") %>% setView(lng = centro$lng[1], lat = centro$lat[1], zoom = 12)
  }, ignoreInit = TRUE)

  # Zoom to individual favela when search is used
  observeEvent(input$sel_favela_search, {
    req(!is.null(input$sel_favela_search),
        length(input$sel_favela_search) > 0,
        input$sel_favela_search != "")

    cd  <- input$sel_favela_search
    row <- fav_df %>% filter(cd_fcu == cd) %>% slice(1)
    req(nrow(row) > 0)

    # Ensure the correct UF is loaded so the map renders
    uf_of_fav    <- row$nm_uf[1]
    current_ufs  <- isolate(input$sel_uf)
    if (is.null(current_ufs) || !uf_of_fav %in% current_ufs)
      updateSelectInput(session, "sel_uf", selected = uf_of_fav)

    # Get favela centroid from fav_sf
    fav_row <- fav_sf %>% filter(CD_FCU == cd)
    req(nrow(fav_row) > 0)
    ctr    <- suppressWarnings(st_centroid(st_geometry(fav_row)))
    coords <- st_coordinates(ctr)
    req(nrow(coords) > 0)

    # Zoom using leafletProxy (works once map is rendered)
    leafletProxy("mapa") %>%
      setView(lng = coords[1, 1], lat = coords[1, 2], zoom = 16)
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Tab: Descritivas
  # -----------------------------------------------------------------------
  chart_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 9))

  output$desc_titulo <- renderText({
    if (isTRUE(input$show_worst))
      paste0("Piores 20 — ", ind_nome())
    else
      paste0("Top 20 — ", ind_nome())
  })

  output$chart_top20 <- renderPlot({
    df_col <- ind_df_col()
    nome   <- ind_nome()
    df     <- dados_filtrados()

    # Validate column exists in fav_df
    if (!df_col %in% names(df)) {
      plot.new(); title(paste("Coluna não encontrada:", df_col)); return(NULL)
    }

    worst   <- isTRUE(input$show_worst)
    opt_pal <- if (worst) "B" else "D"
    dir_pal <- if (worst) -1L else 1L
    cap_lbl <- if (worst) "20 favelas com menor valor na seleção atual" else "20 favelas com maior valor na seleção atual"

    df_filt <- df[!is.na(df[[df_col]]), ]
    df_plot <- if (worst) {
      df_filt[order(df_filt[[df_col]]), ][seq_len(min(20, nrow(df_filt))), ]
    } else {
      df_filt[order(-df_filt[[df_col]]), ][seq_len(min(20, nrow(df_filt))), ]
    }
    df_plot$label <- paste0(df_plot$nm_fcu, " (", df_plot$nm_mun, ")")
    df_plot$label <- fct_reorder(df_plot$label, df_plot[[df_col]])

    ggplot(df_plot, aes(x = .data[[df_col]], y = label, fill = .data[[df_col]])) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(.data[[df_col]], 2)), hjust = -0.15, size = 3) +
      scale_fill_viridis_c(option = opt_pal, direction = dir_pal) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = nome, y = NULL, caption = cap_lbl) +
      chart_theme
  })

  output$chart_hist <- renderPlot({
    df_col <- ind_df_col()
    nome   <- ind_nome()
    df     <- dados_filtrados()
    if (!df_col %in% names(df)) return(NULL)
    med <- median(df[[df_col]], na.rm = TRUE)
    ggplot(df, aes(x = .data[[df_col]])) +
      geom_histogram(bins = 40, fill = "#3B82F6", colour = "white", linewidth = 0.3) +
      geom_vline(xintercept = med, colour = "tomato", linewidth = 1, linetype = "dashed") +
      annotate("text", x = med, y = Inf, vjust = 2, hjust = -0.1,
               label = paste0("Mediana: ", round(med, 2)),
               colour = "tomato", size = 3.5) +
      labs(x = nome, y = "Nº de favelas") + theme_minimal(base_size = 12)
  })

  # -----------------------------------------------------------------------
  # Tab: Comparar — Radar
  # -----------------------------------------------------------------------
  output$chart_radar <- renderPlot({
    req(length(input$sel_favelas_radar) >= 2)

    sel     <- input$sel_favelas_radar
    vnames  <- names(radar_vars)
    vlabels <- unname(radar_vars)
    n_vars  <- length(vnames)

    # Only keep vars that actually exist in fav_df
    vnames  <- vnames[vnames %in% names(fav_df)]
    vlabels <- radar_vars[vnames]
    n_vars  <- length(vnames)
    req(n_vars >= 3)

    df_sel <- fav_df %>%
      filter(cd_fcu %in% sel) %>%
      select(cd_fcu, nm_fcu, nm_mun, nm_uf, all_of(vnames)) %>%
      mutate(favela = paste0(nm_fcu, "\n(", nm_mun, ", ", nm_uf, ")"))

    req(nrow(df_sel) >= 2)

    # Normalise 0-1 globally (lapply avoids for-loop parser issues)
    df_sel[vnames] <- lapply(vnames, function(v) {
      mn  <- min(fav_df[[v]], na.rm = TRUE)
      mx  <- max(fav_df[[v]], na.rm = TRUE)
      val <- df_sel[[v]]
      nrm <- if (mx > mn) { (val - mn) / (mx - mn) } else { rep(0.5, length(val)) }
      nrm[is.na(nrm)] <- 0
      nrm
    })

    angles <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars]

    # Polygon coords (closed)
    polys <- do.call(rbind, lapply(seq_len(nrow(df_sel)), function(i) {
      vals  <- as.numeric(df_sel[i, vnames])
      vals_c <- c(vals, vals[1])
      ang_c  <- c(angles, angles[1])
      data.frame(x = vals_c * cos(ang_c), y = vals_c * sin(ang_c),
                 favela = df_sel$favela[i], stringsAsFactors = FALSE)
    }))

    label_df <- data.frame(
      x = 1.22 * cos(angles), y = 1.22 * sin(angles),
      label = vlabels, stringsAsFactors = FALSE
    )

    grid_df <- do.call(rbind, lapply(c(0.25, 0.5, 0.75, 1.0), function(r) {
      th <- seq(0, 2 * pi, length.out = 200)
      data.frame(x = r * cos(th), y = r * sin(th), r = as.character(r))
    }))

    spoke_df <- data.frame(
      x = cos(angles), y = sin(angles), xend = 0, yend = 0
    )

    pal_colors <- scales::hue_pal()(nrow(df_sel))
    names(pal_colors) <- df_sel$favela

    ggplot() +
      geom_path(data = grid_df,
                aes(x = x, y = y, group = r), colour = "grey82", linewidth = 0.3) +
      geom_segment(data = spoke_df,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   colour = "grey72", linewidth = 0.4) +
      geom_polygon(data = polys,
                   aes(x = x, y = y, colour = favela, fill = favela, group = favela),
                   alpha = 0.15, linewidth = 0.9) +
      geom_point(data = polys %>% group_by(favela) %>% filter(row_number() < n()),
                 aes(x = x, y = y, colour = favela), size = 2.5) +
      geom_text(data = label_df,
                aes(x = x, y = y, label = label),
                size = 3.2, colour = "grey20", fontface = "bold", lineheight = 0.85) +
      annotate("text",
               x = 0.5 * cos(angles[1]) + 0.03, y = 0.5 * sin(angles[1]) - 0.05,
               label = "0.5", size = 2.6, colour = "grey60") +
      annotate("text",
               x = 1.0 * cos(angles[1]) + 0.03, y = 1.0 * sin(angles[1]) - 0.05,
               label = "1.0", size = 2.6, colour = "grey60") +
      coord_fixed(xlim = c(-1.4, 1.4), ylim = c(-1.4, 1.4)) +
      scale_colour_manual(values = pal_colors) +
      scale_fill_manual(values = pal_colors) +
      labs(colour = NULL, fill = NULL,
           caption = "Valores normalizados 0–1 em relação ao universo nacional de favelas") +
      theme_void(base_size = 12) +
      theme(legend.position  = "bottom",
            legend.text      = element_text(size = 9),
            plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0.5),
            plot.margin      = margin(12, 12, 12, 12))
  })

  # -----------------------------------------------------------------------
  # Tab: Dados
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
        "Código FCU"             = cd_fcu,
        "Nome da favela"         = nm_fcu,
        "Município"              = nm_mun,
        "UF"                     = nm_uf,
        "População"              = total_pessoas,
        "Domicílios ocupados"    = total_dp_ocupados,
        "Água encanada (%)"      = perc_agua_adequada,
        "Esgoto rede geral (%)"  = perc_esgoto_adequado,
        "Coleta de lixo (%)"     = perc_lixo_coleta,
        "Renda média (SM)"       = renda_sm_pond,
        "Analfabetismo 15+ (%)"  = perc_analfabeto_populacao,
        "Via pavimentada (%)"    = perc_via_pavimentada,
        "Bueiro (%)"             = perc_bueiro,
        "Iluminação pública (%)" = perc_iluminacao_publica,
        "Ponto de ônibus (%)"    = perc_ponto_onibus,
        "Ciclovia (%)"           = perc_via_bicicleta,
        "Calçada (%)"            = perc_calcada,
        "Obstáculo calçada (%)"  = perc_obstaculo_calcada,
        "Rampa cadeirante (%)"   = perc_rampa_cadeirante,
        "IDS"                    = IDS,
        "IDA"                    = IDA
      )
  })

  output$tabela_dados <- renderDT({
    datatable(tabela_export(), rownames = FALSE, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "frtip")) %>%
      formatRound(columns = c("IDS", "IDA", "Renda média (SM)"), digits = 3) %>%
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
