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
# MUNICIPALITY CENTROIDS  (from uploaded spreadsheet)
# Columns: NM_MUN, SIGLA_UF, xcoord (lng), ycoord (lat)
# =========================================================================
mun_centroids_raw <- read.delim(
  textConnection(
"NM_MUN\tSIGLA_UF\txcoord\tycoord
Alta Floresta D'Oeste\tRO\t-62.274661\t-12.47013228
Ariquemes\tRO\t-62.95725522\t-9.951890007
Porto Velho\tRO\t-64.30681433\t-9.153592827
Vilhena\tRO\t-60.24842048\t-12.09562189
Rio Branco\tAC\t-68.37106195\t-10.06613495
Manaus\tAM\t-60.25962801\t-2.625919383
Boa Vista\tRR\t-60.71795309\t3.117915001
Belém\tPA\t-48.45991077\t-1.240718656
Ananindeua\tPA\t-48.38354433\t-1.334076858
Santarém\tPA\t-55.23845547\t-2.679336294
Marabá\tPA\t-50.01696035\t-5.629799735
Macapá\tAP\t-50.69178036\t0.562753202
Palmas\tTO\t-48.15209202\t-10.22018287
São Luís\tMA\t-44.28090422\t-2.633690649
Imperatriz\tMA\t-47.57523827\t-5.339771433
Teresina\tPI\t-42.74060768\t-5.102658079
Parnaíba\tPI\t-41.7534448\t-2.959159128
Fortaleza\tCE\t-38.52800065\t-3.785832855
Caucaia\tCE\t-38.80969126\t-3.783590266
Juazeiro do Norte\tCE\t-39.28616477\t-7.1828142
Sobral\tCE\t-40.22786986\t-3.811040804
Natal\tRN\t-35.22884144\t-5.803174653
Mossoró\tRN\t-37.32552954\t-5.17581018
João Pessoa\tPB\t-34.86953143\t-7.165465402
Campina Grande\tPB\t-35.96565887\t-7.263553431
Recife\tPE\t-34.93308627\t-8.039344937
Caruaru\tPE\t-36.01663484\t-8.18052296
Olinda\tPE\t-34.86636518\t-7.993100441
Jaboatão dos Guararapes\tPE\t-35.00334988\t-8.152158991
Maceió\tAL\t-35.7113905\t-9.522394447
Aracaju\tSE\t-37.09491352\t-10.99420227
Salvador\tBA\t-38.51472192\t-12.8734914
Feira de Santana\tBA\t-39.03398953\t-12.19307644
Vitória da Conquista\tBA\t-40.91316377\t-15.02228444
Belo Horizonte\tMG\t-43.95998317\t-19.90268448
Contagem\tMG\t-44.08400013\t-19.88717723
Juiz de Fora\tMG\t-43.46473273\t-21.74549947
Uberlândia\tMG\t-48.33173702\t-19.02777147
Montes Claros\tMG\t-43.92881683\t-16.62071806
Vitória\tES\t-39.08796187\t-20.3040098
Vila Velha\tES\t-40.37825542\t-20.43408374
Serra\tES\t-40.30162039\t-20.12811892
Cariacica\tES\t-40.44204913\t-20.290687
Rio de Janeiro\tRJ\t-43.45099032\t-22.92319262
São Gonçalo\tRJ\t-42.99700535\t-22.82561799
Duque de Caxias\tRJ\t-43.29960647\t-22.63249169
Nova Iguaçu\tRJ\t-43.5018375\t-22.68676081
Belford Roxo\tRJ\t-43.37756526\t-22.72872849
São Paulo\tSP\t-46.64809661\t-23.6500802
Guarulhos\tSP\t-46.45487601\t-23.40269396
Campinas\tSP\t-47.04379961\t-22.88376008
São Bernardo do Campo\tSP\t-46.5507918\t-23.81298884
Santo André\tSP\t-46.44158662\t-23.7279603
Osasco\tSP\t-46.78926738\t-23.52874927
Ribeirão Preto\tSP\t-47.82130302\t-21.21084262
Sorocaba\tSP\t-47.44676543\t-23.46455569
São José dos Campos\tSP\t-45.92853599\t-23.09056971
Santos\tSP\t-46.29152919\t-23.86903498
Mauá\tSP\t-46.44639514\t-23.66616287
São José do Rio Preto\tSP\t-49.3581065\t-20.79723367
Mogi das Cruzes\tSP\t-46.1860719\t-23.56957819
Diadema\tSP\t-46.61142844\t-23.69721428
Jundiaí\tSP\t-46.91301193\t-23.19460203
Piracicaba\tSP\t-47.78402389\t-22.72646401
Bauru\tSP\t-49.12613468\t-22.25399189
Carapicuíba\tSP\t-46.84192849\t-23.55004064
Franca\tSP\t-47.38111227\t-20.55522364
Itaquaquecetuba\tSP\t-46.33387993\t-23.46146369
Curitiba\tPR\t-49.28824442\t-25.47790954
Londrina\tPR\t-51.11037658\t-23.51425196
Maringá\tPR\t-51.9678126\t-23.40094933
Ponta Grossa\tPR\t-50.08079333\t-25.13969857
Cascavel\tPR\t-53.37955477\t-25.02777714
São José dos Pinhais\tPR\t-49.0949456\t-25.66436316
Foz do Iguaçu\tPR\t-54.48323924\t-25.46796831
Colombo\tPR\t-49.18802662\t-25.30633595
Guarapuava\tPR\t-51.49123908\t-25.37135466
Paranaguá\tPR\t-48.51766216\t-25.52720821
Florianópolis\tSC\t-48.50819805\t-27.57783391
Joinville\tSC\t-48.95140521\t-26.24428223
Blumenau\tSC\t-49.09730925\t-26.88576667
São José\tSC\t-48.65625577\t-27.5784711
Criciúma\tSC\t-49.37971559\t-28.71569532
Itajaí\tSC\t-48.75341716\t-26.96901297
Chapecó\tSC\t-52.6503387\t-27.12514376
Porto Alegre\tRS\t-51.16453236\t-30.09531647
Caxias do Sul\tRS\t-51.02367386\t-29.10219315
Pelotas\tRS\t-52.34120067\t-31.581114
Canoas\tRS\t-51.17964922\t-29.91222062
Santa Maria\tRS\t-53.82503688\t-29.7849214
Novo Hamburgo\tRS\t-51.0490435\t-29.73475152
São Leopoldo\tRS\t-51.14485\t-29.75533029
Viamão\tRS\t-50.86937274\t-30.16700825
Alvorada\tRS\t-51.03742385\t-29.99498429
Gravataí\tRS\t-50.94704648\t-29.8894729
Campo Grande\tMS\t-54.24946376\t-20.91358432
Dourados\tMS\t-54.83888266\t-22.14486424
Corumbá\tMS\t-56.72225875\t-18.72235068
Cuiabá\tMT\t-55.81823179\t-15.59279233
Várzea Grande\tMT\t-56.24275107\t-15.5624785
Rondonópolis\tMT\t-54.6847935\t-16.56748514
Goiânia\tGO\t-49.27378452\t-16.64355088
Aparecida de Goiânia\tGO\t-49.26248206\t-16.80998738
Anápolis\tGO\t-48.97288603\t-16.29057977
Brasília\tDF\t-47.79685087\t-15.78116622"
  ),
  stringsAsFactors = FALSE, sep = "\t"
)

# Normalise: keep only NM_MUN, lat, lng
mun_centroids <- mun_centroids_raw %>%
  transmute(nm_mun = NM_MUN, lng = xcoord, lat = ycoord)

# =========================================================================
# CAPITAL COORDINATES (for UF zoom)
# =========================================================================
capitais <- tibble::tribble(
  ~nm_uf,                  ~lat,     ~lng,  ~zoom,
  "Acre",                 -9.975,  -67.824,   11,
  "Alagoas",              -9.666,  -35.735,   11,
  "Amap\u00e1",                 0.034,  -51.066,   11,
  "Amazonas",             -3.119,  -60.021,   11,
  "Bahia",               -12.971,  -38.501,   11,
  "Cear\u00e1",                -3.717,  -38.543,   11,
  "Distrito Federal",    -15.779,  -47.929,   11,
  "Esp\u00edrito Santo",      -20.319,  -40.338,   11,
  "Goi\u00e1s",               -16.686,  -49.264,   11,
  "Maranh\u00e3o",             -2.530,  -44.303,   11,
  "Mato Grosso",         -15.601,  -56.097,   11,
  "Mato Grosso do Sul",  -20.469,  -54.620,   11,
  "Minas Gerais",        -19.917,  -43.934,   11,
  "Par\u00e1",                 -1.455,  -48.502,   11,
  "Para\u00edba",              -7.115,  -34.863,   11,
  "Paran\u00e1",              -25.428,  -49.273,   11,
  "Pernambuco",           -8.054,  -34.881,   11,
  "Piau\u00ed",                -5.092,  -42.803,   11,
  "Rio de Janeiro",      -22.906,  -43.173,   11,
  "Rio Grande do Norte",  -5.795,  -35.209,   11,
  "Rio Grande do Sul",   -30.033,  -51.230,   11,
  "Rond\u00f4nia",             -8.761,  -63.902,   11,
  "Roraima",               2.819,  -60.673,   11,
  "Santa Catarina",      -27.595,  -48.548,   11,
  "S\u00e3o Paulo",           -23.550,  -46.633,   11,
  "Sergipe",             -10.916,  -37.073,   11,
  "Tocantins",           -10.249,  -48.324,   11
)

# =========================================================================
# INDICATOR CATALOGUE  (AOP excluded from dropdown - shown in popup only)
# =========================================================================
indicadores <- list(
  list(col = "IDS",       label = "IDS - Indice de Desenvolvimento Social",   dir = +1, group = "Indices"),
  list(col = "IDA",       label = "IDA - Indice de Acessibilidade Urbana",    dir = +1, group = "Indices"),
  list(col = "PERC_AGUA", label = "\u00c1gua encanada (%)",                        dir = +1, group = "Saneamento"),
  list(col = "PERC_ESGO", label = "Esgoto rede geral (%)",                    dir = +1, group = "Saneamento"),
  list(col = "PERC_LIXO", label = "Coleta de lixo (%)",                       dir = +1, group = "Saneamento"),
  list(col = "RENDA_SM",  label = "Renda m\u00e9dia (sal. m\u00edn.)",                  dir = +1, group = "Renda e Educa\u00e7\u00e3o"),
  list(col = "PERC_ANALF",label = "Analfabetismo 15+ (%)",                    dir = -1, group = "Renda e Educa\u00e7\u00e3o"),
  list(col = "P_VIAPAV",  label = "Via pavimentada (%)",                      dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_BUEIRO",  label = "Bueiro / boca de lobo (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ILUM",    label = "Ilumina\u00e7\u00e3o p\u00fablica (%)",                   dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ONTON",   label = "Ponto de \u00f4nibus (%)",                      dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_VIABIC",  label = "Ciclovia / ciclofaixa (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_CALCAD",  label = "Cal\u00e7ada (%)",                              dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_OBSTAC",  label = "Obst\u00e1culo na cal\u00e7ada (%)",                 dir = -1, group = "Acessibilidade Urbana"),
  list(col = "P_RAMPA",   label = "Rampa para cadeirante (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "tem_risco", label = "Exposi\u00e7\u00e3o a risco natural (flag)",         dir = -1, group = "Riscos Naturais")
)

ind_choices <- setNames(sapply(indicadores, `[[`, "col"), sapply(indicadores, `[[`, "label"))
ind_groups  <- sapply(indicadores, `[[`, "group")
ind_grouped <- split(ind_choices, ind_groups)
ind_label   <- setNames(sapply(indicadores, `[[`, "label"), sapply(indicadores, `[[`, "col"))

# Map GeoJSON short names -> fav_df long column names
ind_df_col <- c(
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
 

# =========================================================================
# GEO HIERARCHIES
# =========================================================================
ufs        <- sort(unique(na.omit(fav_df$nm_uf)))
municipios <- sort(unique(na.omit(fav_df$nm_mun)))

# Favela search choices: sorted by population desc
fav_ord <- fav_df[order(-fav_df$total_pessoas, fav_df$nm_fcu), ]
fav_search_choices <- setNames(
  fav_ord$cd_fcu,
  paste0(fav_ord$nm_fcu, " - ", fav_ord$nm_mun)
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
  sprintf("<tr><td>%s</td><td>%s</td></tr>", label, formatC(round(val), format="d", big.mark="."))
}

make_popup <- function(sf_obj) {
  fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(round(x, 1), "%"))
  fmt_num <- function(x) ifelse(is.na(x), "—", round(x, 3))
  fmt_sm  <- function(x) ifelse(is.na(x), "—", paste0(round(x, 2), " SM"))

  # Pull AOP from fav_df (not in fav_sf)
  aop_data <- fav_df %>%
    select(cd_fcu, CMAET60, CMAST60, CMATT60, CMACT60)

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
      aop_row_html("Empregos acessíveis",           suppressWarnings(as.numeric(r["CMATT60"]))),
      aop_row_html("CRAS acessíveis",               suppressWarnings(as.numeric(r["CMACT60"])))
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
      formatC(suppressWarnings(as.integer(r["pop"])), format="d", big.mark="."),
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
    selectInput("sel_uf", "Estado (UF)", choices = c("Todos" = "", ufs), selected = "", multiple = TRUE),
    hr(),
    p(tags$small(tags$i("Dados: Censo IBGE 2022 · IBGE FCU 2022 · SGB · AOP IPEA 2019")))
  ),

  # -----------------------------------------------------------------------
  nav_panel(
    title = tagList(icon("map"), " Mapa"),
    card(full_screen = TRUE,
      card_header(textOutput("mapa_titulo")),
      uiOutput("mapa_ui")
    )
  ),

  # -----------------------------------------------------------------------
  nav_panel(
    title = tagList(icon("chart-bar"), " Indicadores"),
    layout_columns(col_widths = c(12),
      card(
     card_header(
          layout_columns(col_widths = c(8, 4),
            selectInput("sel_ind", "Indicador", choices = ind_grouped, selected = "IDS"),
            checkboxInput("show_worst", "Piores 20", value = FALSE)
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

   # -----------------------------------------------------------------------
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

  # -----------------------------------------------------------------------
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
        tags$li(tags$b("IDS — Índice de Desenvolvimento Social:"), " média de 6 componentes min-max normalizados: água, esgoto, lixo, banheiros por morador, alfabetização e renda."),
        tags$li(tags$b("IDA — Índice de Acessibilidade Urbana:"), " média de 8 componentes normalizados: via pavimentada, bueiro, iluminação, ponto de ônibus, ciclovia, calçada, obstáculo (invertido) e rampa.")
      ),

      tags$h5("Riscos Naturais — SGB/CPRM"),
      tags$p("Variável binária: 1 se o polígono FCU intersecta área de risco geológico mapeada pelo Serviço Geológico Brasileiro."),

      tags$h5("Acesso a Oportunidades — AOP (aopdata / IPEA, 2019)"),
      tags$p("Dados disponíveis para 20 municípios com maior população. Favelas em outros municípios exibem NA nessas variáveis."),
      tags$p("Municípios cobertos:"),
      tags$p(tags$em("Belém, Belo Horizonte, Brasília, Campinas, Campo Grande, Curitiba, Duque de Caxias, Fortaleza, Goiânia, Guarulhos, Maceió, Manaus, Natal, Porto Alegre, Recife, Rio de Janeiro, Salvador, São Gonçalo, São Luís, São Paulo.")),
      tags$ul(
      tags$li(tags$b("CMAET60:"), " escolas públicas acessíveis em ≤ 60 min por TP (ET = ensino)."),
        tags$li(tags$b("CMAST60:"), " hospitais/UPAs acessíveis em ≤ 60 min por TP (ST = saúde)."),
        tags$li(tags$b("CMATT60:"), " empregos formais acessíveis em ≤ 60 min por TP (TT = trabalho)."),
        tags$li(tags$b("CMACT60:"), " CRAS acessíveis em ≤ 60 min por TP (CT = assistência social).")
      ),
      tags$p("As variáveis AOP aparecem no popup ao clicar em uma favela, somente quando disponíveis. Elas não estão disponíveis no mapa de cores nem nas Descritivas por cobertura parcial."),

      tags$h5("Nota metodológica"),
      tags$p("Indicadores de setor censitário foram agregados ao nível FCU por média ponderada pela população. Normalização min-max calculada sobre setores urbanos de municípios com pelo menos uma favela.")
    ))
  )
)

# =========================================================================
# SERVER
# =========================================================================
server <- function(input, output, session) {
  
 

  # Update municipalities when UF changes
  # (no municipality filter needed - removed from UI)

  dados_filtrados <- reactive({ filter_data(fav_df, input$sel_uf, NULL) })
  sf_filtrado     <- reactive({ filter_sf(fav_sf,  input$sel_uf, NULL) })
  ind_col         <- reactive({ input$sel_ind })
  ind_nome        <- reactive({ ind_label[[input$sel_ind]] })
  ind_col_df      <- reactive({ ind_df_col[[input$sel_ind]] })

  # -----------------------------------------------------------------------
  # Tab: Mapa
  # -----------------------------------------------------------------------
  output$mapa_titulo <- renderText({
if (length(input$sel_uf) == 0 || all(input$sel_uf == "")) {
      "Mapa — selecione um Estado (UF)"
    } else {
      paste0("Clique em uma favela pra saber mais sobre ela | ", nrow(dados_filtrados()), " favelas")
    }
  })

  output$mapa_ui <- renderUI({
 uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")

    if (!uf_active) {
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
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")
    req(uf_active)

    sf_obj <- sf_filtrado()
    col    <- "IDS"
    nome   <- "IDS"
    vals   <- sf_obj[[col]]
    pal    <- colorNumeric(viridis(100), domain = vals, na.color = "#CCCCCC")
    popups <- unname(make_popup(sf_obj))

    # Default view: capital of first selected UF
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
    uf_sel <- input$sel_uf[1]
    cap    <- capitais %>% filter(nm_uf == uf_sel)
    if (nrow(cap) > 0) {
      leafletProxy("mapa") %>%
        setView(lng = cap$lng[1], lat = cap$lat[1], zoom = cap$zoom[1])
    }
  }, ignoreInit = TRUE)

 
  # -----------------------------------------------------------------------
  # Tab: Descritivas
  # -----------------------------------------------------------------------
  chart_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 9),
          plot.title  = element_blank())

output$chart_top20 <- renderPlot({
    col   <- ind_col_df(); nome <- ind_nome(); df <- dados_filtrados()    
    if (!col %in% names(df)) return(NULL)
    worst <- isTRUE(input$show_worst)
    df_c  <- df[!is.na(df[[col]]), ]
    df_o  <- df_c[order(df_c[[col]], decreasing = !worst), ]
    df_p  <- df_o[seq_len(min(20, nrow(df_o))), ]
    df_p$lbl <- fct_reorder(paste0(df_p$nm_fcu, " (", df_p$nm_mun, ")"), df_p[[col]])
    pal_opt  <- if (worst) { "B" } else { "D" }
    cap_txt  <- if (worst) { "20 piores na selecao atual" } else { "Top 20 na selecao atual" }
    ggplot(df_p, aes(x = .data[[col]], y = lbl, fill = .data[[col]])) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(.data[[col]], 2)), hjust = -0.15, size = 3) +
      scale_fill_viridis_c(option = pal_opt) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = nome, y = NULL, caption = cap_txt) +
      chart_theme
  })

  output$chart_hist <- renderPlot({
    col <- ind_col_df(); nome <- ind_nome(); df <- dados_filtrados()
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
             IDS, IDA, any_of(c("CMAET60","CMAST60","CMATT60","CMACT60","tem_risco"))) %>%
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
      u <- input$sel_uf
      m <- input$sel_mun
      uf_str  <- if (length(u) > 0 && !all(u == "")) {
        paste0("_", paste(u, collapse = "-"))
      } else { "" }
      mun_str <- if (length(m) > 0 && !all(m == "")) {
        paste0("_", paste(m, collapse = "-"))
      } else { "" }
      paste0("favelas_br", uf_str, mun_str, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) { write_csv2(tabela_export(), file) }
  )
}

# Custom JS: receive zoom_to_favela message from server
ui_with_js <- tagList(
  tags$head(tags$script(HTML(
    "Shiny.addCustomMessageHandler('zoom_to_favela', function(msg) {
      if (window.HTMLWidgets && window.HTMLWidgets.find('#mapa')) {
        var map = window.HTMLWidgets.find('#mapa').getMap();
        if (map) { map.setView([msg.lat, msg.lng], msg.zoom); }
      }
    });"
  ))),
  ui
)

shinyApp(ui_with_js, server)
