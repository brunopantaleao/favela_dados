# Favelas BR — Plataforma de Análise

GEOJSON_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_br_simplified.geojson"
CSV_IDS_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_ids_ida.csv"
CSV_RISK_URL <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_riscos.csv"
CSV_AOP_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_acesso_oportunidades.csv"
CSV_DEMO_URL <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_demograficos_2022.csv"

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

# Pre-compute favela centroids for search zoom
fav_centroids <- fav_sf %>%
  st_centroid() %>%
  mutate(
    lng = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(CD_FCU, NM_FCU, NM_MUN, NM_UF, lng, lat) %>%
  mutate(CD_FCU = as.character(CD_FCU))

# --- Main table ---
fav_df <- read_csv2(CSV_IDS_URL, show_col_types = FALSE) %>%
  mutate(cd_fcu = as.character(cd_fcu))

# --- Risk table ---
riscos <- tryCatch(
  read_csv(CSV_RISK_URL, show_col_types = FALSE),
  error = function(e) read_csv2(CSV_RISK_URL, show_col_types = FALSE)
) %>%
  mutate(cd_fcu = as.character(cd_fcu))

fav_df <- fav_df %>%
  left_join(
    riscos %>% select(
      cd_fcu,
      tem_risco, tem_perigo, tem_inundacao, tem_enxurrada, tem_corrida_massa,
      classe_risco,
      n_setores_risco, n_setores_perigo, n_setores_inundacao,
      n_setores_enxurrada, n_setores_corrida
    ),
    by = "cd_fcu"
  )

# --- AOP table ---
aop <- tryCatch(
  read_csv2(CSV_AOP_URL, show_col_types = FALSE) %>%
    mutate(cd_fcu = as.character(cd_fcu)) %>%
    select(cd_fcu, any_of(c(
      "CMAET60",  "CMAST60",  "CMATT60",  "CMACT60",
      "CMAET060", "CMAST060", "CMATT060", "CMACT060"
    ))) %>%
    rename_with(~ gsub("0$", "", .x), ends_with("060")),
  error = function(e) { message("AOP file not found — skipping."); NULL }
)

if (!is.null(aop)) {
  fav_df <- fav_df %>% left_join(aop, by = "cd_fcu")
} else {
  fav_df$CMAET60 <- NA_real_
  fav_df$CMAST60 <- NA_real_
  fav_df$CMATT60 <- NA_real_
  fav_df$CMACT60 <- NA_real_
}

# --- Demographics table ---
demo <- tryCatch(
  read_csv(CSV_DEMO_URL, show_col_types = FALSE) %>%
    mutate(cd_fcu = as.character(cd_fcu)),
  error = function(e) { message("Demographics file not found — skipping."); NULL }
)

demo_cols <- c("pct_pretos_pardos", "pct_indigenas",
               "pct_under5", "pct_under19", "pct_under30", "pct_idoso", "pct_chefe_mulher")

if (!is.null(demo)) {
  # Only select columns that actually exist in the file
  available <- intersect(demo_cols, names(demo))
  fav_df <- fav_df %>%
    left_join(demo %>% select(cd_fcu, all_of(available)), by = "cd_fcu")
  # Fill any expected columns that were absent in the file with NA
  for (col in setdiff(demo_cols, available)) {
    fav_df[[col]] <- NA_real_
    message("  Demographics: column '", col, "' not found in CSV — set to NA")
  }
} else {
  for (col in demo_cols) fav_df[[col]] <- NA_real_
}

message("  fav_df ready: ", nrow(fav_df), " rows | ",
        sum(!is.na(fav_df$tem_risco)),        " FCUs with risk data | ",
        sum(!is.na(fav_df$CMAET60)),          " FCUs with AOP data | ",
        sum(!is.na(fav_df$pct_pretos_pardos))," FCUs with demographic data")

# =========================================================================
# MUNICIPALITY CENTROIDS
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

mun_centroids <- mun_centroids_raw %>%
  transmute(nm_mun = NM_MUN, lng = xcoord, lat = ycoord)

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
# INDICATOR CATALOGUE
# =========================================================================
indicadores <- list(
  list(col = "IDS",              label = "IDS - Índice de Desenvolvimento Social",   dir = +1, group = "Índices"),
  list(col = "IDA",              label = "IDA - Índice de Acessibilidade Urbana",    dir = +1, group = "Índices"),
  list(col = "PERC_AGUA",        label = "Água encanada (%)",                        dir = +1, group = "Saneamento"),
  list(col = "PERC_ESGO",        label = "Esgoto rede geral (%)",                    dir = +1, group = "Saneamento"),
  list(col = "PERC_LIXO",        label = "Coleta de lixo (%)",                       dir = +1, group = "Saneamento"),
  list(col = "RENDA_SM",         label = "Renda média (sal. mín.)",                  dir = +1, group = "Renda e Educação"),
  list(col = "PERC_ANALF",       label = "Analfabetismo 15+ (%)",                    dir = -1, group = "Renda e Educação"),
  list(col = "P_VIAPAV",         label = "Via pavimentada (%)",                      dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_BUEIRO",         label = "Bueiro / boca de lobo (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ILUM",           label = "Iluminação pública (%)",                   dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_ONTON",          label = "Ponto de ônibus (%)",                      dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_VIABIC",         label = "Ciclovia / ciclofaixa (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_CALCAD",         label = "Calçada (%)",                              dir = +1, group = "Acessibilidade Urbana"),
  list(col = "P_OBSTAC",         label = "Obstáculo na calçada (%)",                 dir = -1, group = "Acessibilidade Urbana"),
  list(col = "P_RAMPA",          label = "Rampa para cadeirante (%)",                dir = +1, group = "Acessibilidade Urbana"),
  list(col = "tem_risco",        label = "Exposição a risco natural (flag)",         dir = -1, group = "Riscos Geológicos (SGB)"),
  list(col = "PCT_PRETOS_PARDOS",label = "Pretos e pardos (%)",                      dir =  0, group = "Demografia"),
  list(col = "PCT_INDIGENAS",    label = "Indígenas (%)",                            dir =  0, group = "Demografia"),
  list(col = "PCT_UNDER5",       label = "Crianças menores de 5 anos (%)",           dir =  0, group = "Demografia"),
  list(col = "PCT_UNDER19",      label = "Jovens menores de 19 anos (%)",            dir =  0, group = "Demografia"),
  list(col = "PCT_UNDER30",      label = "Pessoas menores de 30 anos (%)",           dir =  0, group = "Demografia"),
  list(col = "PCT_IDOSO",        label = "Idosos 60+ anos (%)",                      dir =  0, group = "Demografia"),
  list(col = "PCT_CHEFE_MULHER",label = "Domicílios chefiados por mulher (%)",       dir =  0, group = "Demografia")
)

ind_choices <- setNames(sapply(indicadores, `[[`, "col"), sapply(indicadores, `[[`, "label"))
ind_groups  <- sapply(indicadores, `[[`, "group")
ind_grouped <- split(ind_choices, ind_groups)
ind_label   <- setNames(sapply(indicadores, `[[`, "label"), sapply(indicadores, `[[`, "col"))

# Map GeoJSON short names -> fav_df column names
ind_df_col <- c(
  IDS               = "IDS",
  IDA               = "IDA",
  PERC_AGUA         = "perc_agua_adequada",
  PERC_ESGO         = "perc_esgoto_adequado",
  PERC_LIXO         = "perc_lixo_coleta",
  RENDA_SM          = "renda_sm_pond",
  PERC_ANALF        = "perc_analfabeto_populacao",
  P_VIAPAV          = "perc_via_pavimentada",
  P_BUEIRO          = "perc_bueiro",
  P_ILUM            = "perc_iluminacao_publica",
  P_ONTON           = "perc_ponto_onibus",
  P_VIABIC          = "perc_via_bicicleta",
  P_CALCAD          = "perc_calcada",
  P_OBSTAC          = "perc_obstaculo_calcada",
  P_RAMPA           = "perc_rampa_cadeirante",
  tem_risco         = "tem_risco",
  PCT_PRETOS_PARDOS = "pct_pretos_pardos",
  PCT_INDIGENAS     = "pct_indigenas",
  PCT_UNDER5        = "pct_under5",
  PCT_UNDER19       = "pct_under19",
  PCT_UNDER30       = "pct_under30",
  PCT_IDOSO         = "pct_idoso",
  PCT_CHEFE_MULHER  = "pct_chefe_mulher"
)

# =========================================================================
# GEO HIERARCHIES
# =========================================================================
ufs        <- sort(unique(na.omit(fav_df$nm_uf)))
municipios <- sort(unique(na.omit(fav_df$nm_mun)))

# Search choices: named vector  "nm_fcu - nm_mun" -> cd_fcu, sorted by pop desc
fav_ord <- fav_df[order(-fav_df$total_pessoas, fav_df$nm_fcu), ]
fav_search_choices <- setNames(
  as.character(fav_ord$cd_fcu),
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

make_popup <- function(sf_obj) {
  fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(round(x, 1), "%"))
  fmt_num <- function(x) ifelse(is.na(x), "—", as.character(round(x, 3)))
  fmt_sm  <- function(x) ifelse(is.na(x), "—", paste0(round(x, 2), " SM"))
  fmt_n   <- function(x) ifelse(is.na(x) | x == 0,  "—", as.character(x))
  flag    <- function(x) ifelse(is.na(x) | x == 0,  "Não", "Sim")

  aop_data  <- fav_df %>%
    select(cd_fcu, CMAET60, CMAST60, CMATT60, CMACT60)

  risk_data <- fav_df %>%
    select(cd_fcu, tem_risco, tem_perigo, tem_inundacao, tem_enxurrada,
           tem_corrida_massa, classe_risco,
           n_setores_risco, n_setores_perigo, n_setores_inundacao,
           n_setores_enxurrada, n_setores_corrida)

  df_rows <- tibble(
    nm     = sf_obj$NM_FCU,
    cd_fcu = sf_obj$CD_FCU,
    ids    = sf_obj$IDS,
    ida    = sf_obj$IDA,
    agua   = sf_obj$PERC_AGUA,
    esgo   = sf_obj$PERC_ESGO,
    lixo   = sf_obj$PERC_LIXO,
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
    pop    = sf_obj$TOT_PES
  ) %>%
    left_join(aop_data,  by = "cd_fcu") %>%
    left_join(risk_data, by = "cd_fcu") %>%
    left_join(
      fav_df %>% select(cd_fcu, pct_pretos_pardos, pct_indigenas,
                        pct_under5, pct_under19, pct_under30, pct_idoso, pct_chefe_mulher),
      by = "cd_fcu"
    )

  classe_label <- ifelse(is.na(df_rows$classe_risco) | df_rows$classe_risco == "",
                         "—", df_rows$classe_risco)
  badge_col <- dplyr::case_when(
    classe_label == "Alto"       ~ "#f59e0b",
    classe_label == "Muito alto" ~ "#ef4444",
    TRUE                         ~ "#6b7280"
  )
  badge_html <- ifelse(
    classe_label == "—",
    "—",
    sprintf("<span style='background:%s;color:white;border-radius:3px;padding:1px 5px;font-size:0.82em;'>%s</span>",
            badge_col, classe_label)
  )

  mapply(function(nm, cd_fcu,
                  ids, ida, agua, esgo, lixo, renda, analf,
                  viapav, bueiro, ilum, onton, viabic, calcad, obstac, rampa,
                  pop,
                  CMAET60, CMAST60, CMATT60, CMACT60,
                  tem_risco, tem_inundacao, tem_enxurrada, tem_corrida_massa, tem_perigo,
                  n_setores_risco, n_setores_inundacao, n_setores_enxurrada,
                  n_setores_corrida, n_setores_perigo,
                  pct_pretos_pardos, pct_indigenas,
                  pct_under5, pct_under19, pct_under30, pct_idoso, pct_chefe_mulher,
                  badge) {

    aop_rows <- paste0(
      if (!is.na(CMAET60)) sprintf("<tr><td>Escolas públicas acessíveis</td><td>%s</td></tr>", formatC(round(CMAET60), format="d", big.mark=".")) else "",
      if (!is.na(CMAST60)) sprintf("<tr><td>Hospitais/UPAs acessíveis</td><td>%s</td></tr>",    formatC(round(CMAST60), format="d", big.mark=".")) else "",
      if (!is.na(CMATT60)) sprintf("<tr><td>Empregos acessíveis</td><td>%s</td></tr>",           formatC(round(CMATT60), format="d", big.mark=".")) else "",
      if (!is.na(CMACT60)) sprintf("<tr><td>CRAS acessíveis</td><td>%s</td></tr>",               formatC(round(CMACT60), format="d", big.mark=".")) else ""
    )
    aop_block <- if (nchar(aop_rows) > 0) paste0(
      "<tr><td colspan='2' style='padding:4px 0;'></td></tr>",
      "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>",
      "Oportunidades <small>(60min/transporte público)</small></td></tr>",
      aop_rows
    ) else ""

    sprintf(
      "<div style='font-family:sans-serif;min-width:500px;'>
        <b style='font-size:1.05em;'>%s</b>
        <hr style='margin:4px 0;'>
        <table style='width:100%%;font-size:1em;'><tr>
          <td style='width:50%%;vertical-align:top;padding-right:20px;'>
            <table style='width:100%%;border-collapse:collapse;'>
              <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Índices compostos</td></tr>
              <tr><td>IDS</td><td><b>%s</b></td></tr>
              <tr><td>IDA</td><td><b>%s</b></td></tr>
              <tr><td colspan='2' style='padding:3px 0;'></td></tr>
              <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Saneamento</td></tr>
              <tr><td>Água encanada</td><td>%s</td></tr>
              <tr><td>Esgoto rede geral</td><td>%s</td></tr>
              <tr><td>Coleta de lixo</td><td>%s</td></tr>
              <tr><td colspan='2' style='padding:3px 0;'></td></tr>
              <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Renda e Educação</td></tr>
              <tr><td>Renda média</td><td>%s</td></tr>
              <tr><td>Analfabetismo 15+</td><td>%s</td></tr>
              <tr><td colspan='2' style='padding:3px 0;'></td></tr>
              <tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>Geral</td></tr>
              <tr><td>População</td><td>%s</td></tr>
              <tr><td colspan='2' style='padding:4px 0;'></td></tr>
              <tr><td colspan='2' style='background:#fef2f2;padding:2px 4px;font-weight:600;color:#991b1b;'>Riscos Geológicos (SGB)</td></tr>
              <tr><td>Exposto a risco</td><td>%s</td></tr>
              <tr><td>Classe de risco</td><td>%s</td></tr>
              <tr><td>Setores c/ risco geológico</td><td>%s</td></tr>
              <tr><td>Setores c/ inundação</td><td>%s</td></tr>
              <tr><td>Setores c/ enxurrada</td><td>%s</td></tr>
              <tr><td>Setores c/ corrida de massa</td><td>%s</td></tr>
              <tr><td>Setores c/ perigo</td><td>%s</td></tr>
            </table>
          </td>
          <td style='width:50%%;vertical-align:top;padding-left:20px;padding-right:10px;border-left:1px solid #eee;'>
            <table style='width:100%%;border-collapse:collapse;'>
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
              <tr><td colspan='2' style='background:#eff6ff;padding:2px 4px;font-weight:600;color:#1e40af;'>Demografia (Censo 2022)</td></tr>
              <tr><td>Pretos e pardos</td><td>%s</td></tr>
              <tr><td>Indígenas</td><td>%s</td></tr>
              <tr><td>Menores de 5 anos</td><td>%s</td></tr>
              <tr><td>Menores de 19 anos</td><td>%s</td></tr>
              <tr><td>Menores de 30 anos</td><td>%s</td></tr>
              <tr><td>Idosos 60+ anos</td><td>%s</td></tr>
              <tr><td>Domicílios c/ chefe mulher</td><td>%s</td></tr>
            </table>
          </td>
        </tr></table>
      </div>",
      nm,
      fmt_num(ids), fmt_num(ida),
      fmt_pct(agua), fmt_pct(esgo), fmt_pct(lixo),
      fmt_sm(renda), fmt_pct(analf),
      formatC(as.integer(pop), format="d", big.mark="."),
      flag(tem_risco), badge,
      fmt_n(n_setores_risco), fmt_n(n_setores_inundacao),
      fmt_n(n_setores_enxurrada), fmt_n(n_setores_corrida),
      fmt_n(n_setores_perigo),
      fmt_pct(viapav), fmt_pct(bueiro), fmt_pct(ilum), fmt_pct(onton),
      fmt_pct(viabic), fmt_pct(calcad), fmt_pct(obstac), fmt_pct(rampa),
      aop_block,
      fmt_pct(pct_pretos_pardos), fmt_pct(pct_indigenas),
      fmt_pct(pct_under5), fmt_pct(pct_under19), fmt_pct(pct_under30), fmt_pct(pct_idoso), fmt_pct(pct_chefe_mulher)
    )
  },
  df_rows$nm, df_rows$cd_fcu,
  df_rows$ids, df_rows$ida,
  df_rows$agua, df_rows$esgo, df_rows$lixo,
  df_rows$renda, df_rows$analf,
  df_rows$viapav, df_rows$bueiro, df_rows$ilum, df_rows$onton,
  df_rows$viabic, df_rows$calcad, df_rows$obstac, df_rows$rampa,
  df_rows$pop,
  df_rows$CMAET60, df_rows$CMAST60, df_rows$CMATT60, df_rows$CMACT60,
  df_rows$tem_risco, df_rows$tem_inundacao, df_rows$tem_enxurrada,
  df_rows$tem_corrida_massa, df_rows$tem_perigo,
  df_rows$n_setores_risco, df_rows$n_setores_inundacao,
  df_rows$n_setores_enxurrada, df_rows$n_setores_corrida,
  df_rows$n_setores_perigo,
  df_rows$pct_pretos_pardos, df_rows$pct_indigenas,
  df_rows$pct_under5, df_rows$pct_under19, df_rows$pct_under30, df_rows$pct_idoso, df_rows$pct_chefe_mulher,
  badge_html,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)
}

# =========================================================================
# UI
# =========================================================================
ui <- page_navbar(
  title    = "Favela Dados",
theme = bs_theme(
  bootswatch  = "flatly",
  base_font   = font_google("IBM Plex Sans"),
  heading_font = font_google("IBM Plex Sans"),
  primary     = "#611ce3",   # favelas.br violet (lead colour, p.11)
  success     = "#47d9ba"    # teal accent — e.g. the download button
),
  fillable = TRUE,

  sidebar = sidebar(
    width = 270,
    title = "Filtros",
    # Favela search — server-side selectize, activates after 3 chars
    selectizeInput(
      "search_fav", "Buscar favela",
      choices  = NULL,
      selected = NULL,
      options  = list(
        placeholder        = "Digite o nome da favela...",
        minimumInputLength = 3,
        maxOptions         = 20,
        # Garante que nenhuma favela venha selecionada ao abrir o app
        onInitialize       = I('function() { this.setValue(""); }')
      )
    ),
    hr(),
    selectInput("sel_uf", "Estado (UF)", choices = c("Todos" = "", ufs), selected = "", multiple = TRUE),
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

  nav_panel(
    title = tagList(icon("table"), " Dados"),
    card(
      card_header(layout_columns(col_widths = c(8, 4),
        textOutput("dados_titulo"),
        # Começa desabilitado; reabilitado via JS quando a tabela desenha (ponto B)
        tagAppendAttributes(
          downloadButton("download_csv", "Baixar CSV", class = "btn-sm btn-success"),
          class = "disabled", `aria-disabled` = "true"
        )
      )),
      card_body(
        uiOutput("download_note"),
        DTOutput("tabela_dados")
      )
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

      tags$h5("Demografia (Censo 2022)"),
      tags$p("Calculado a partir dos Agregados por Setores Censitários do Censo 2022 (resultados do universo), via FTP público do IBGE."),
      tags$ul(
        tags$li(tags$b("Pretos e pardos (%):"), " soma das categorias preta e parda sobre a população total do setor."),
        tags$li(tags$b("Indígenas (%):"), " categoria indígena sobre a população total."),
        tags$li(tags$b("Menores de 5 anos (%):"), " faixa 0–4 anos (exata)."),
        tags$li(tags$b("Menores de 19 anos (%):"), " faixas 0–4 + 5–9 + 10–14 + 15–19 (exata — inclui todo o grupo 15–19)."),
        tags$li(tags$b("Menores de 30 anos (%):"), " faixas 0–4 até 25–29 (exata)."),
        tags$li(tags$b("Idosos 60+ anos (%):"), " faixas 60–69 + 70+ (exata)."),
        tags$li(tags$b("Domicílios c/ chefe mulher (%):"), " domicílios particulares permanentes ocupados com mulher como pessoa responsável (Basico, v0007 / v0002).")
      ),

      tags$h5("Índices Compostos"),
      tags$ul(
        tags$li(tags$b("IDS — Índice de Desenvolvimento Social:"), " média de 6 componentes min-max normalizados: água, esgoto, lixo, banheiros por morador, alfabetização e renda."),
        tags$li(tags$b("IDA — Índice de Acessibilidade Urbana:"), " média de 8 componentes normalizados: via pavimentada, bueiro, iluminação, ponto de ônibus, ciclovia, calçada, obstáculo (invertido) e rampa.")
      ),

      tags$h5("Riscos Geológicos (SGB) — SGB/CPRM"),
      tags$p("Variável binária: 1 se o polígono FCU intersecta área de risco geológico mapeada pelo Serviço Geológico Brasileiro."),

      tags$h5("Acesso a Oportunidades — AOP (aopdata / IPEA, 2019)"),
      tags$p("Dados disponíveis para 20 municípios com maior população."),
      tags$ul(
        tags$li(tags$b("CMAET60:"), " escolas públicas acessíveis em ≤ 60 min por TP."),
        tags$li(tags$b("CMAST60:"), " hospitais/UPAs acessíveis em ≤ 60 min por TP."),
        tags$li(tags$b("CMATT60:"), " empregos formais acessíveis em ≤ 60 min por TP."),
        tags$li(tags$b("CMACT60:"), " CRAS acessíveis em ≤ 60 min por TP.")
      ),

      tags$h5("Nota metodológica"),
      tags$p("Indicadores de setor censitário foram agregados ao nível FCU por média ponderada pela população. Normalização min-max calculada sobre setores urbanos de municípios com pelo menos uma favela.")
    ))
  )
)

# =========================================================================
# SERVER
# =========================================================================
server <- function(input, output, session) {

  # -----------------------------------------------------------------------
  # Favela search
  # -----------------------------------------------------------------------

  # Load all choices server-side on session start (keeps HTML payload small)
  updateSelectizeInput(session, "search_fav",
                       choices  = fav_search_choices,
                       selected = NULL,
                       server   = TRUE)

  # When user picks a favela: load its UF and zoom to it
  observeEvent(input$search_fav, {
    req(input$search_fav, input$search_fav != "")

    fcu <- as.character(input$search_fav)
    row <- fav_centroids %>% filter(CD_FCU == fcu)
    if (nrow(row) == 0) return()

    # 1. Set UF filter so the map renders
    updateSelectInput(session, "sel_uf", selected = row$NM_UF[1])

    # 2. Zoom map to favela after a short delay (map needs to render first)
    session$sendCustomMessage("zoom_to_favela", list(
      lat  = row$lat[1],
      lng  = row$lng[1],
      zoom = 15,
      cd_fcu = fcu
    ))
  }, ignoreInit = TRUE)

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
    vals   <- sf_obj[[col]]
    pal <- colorQuantile(viridis(100), domain = vals, n = 5, na.color = "#CCCCCC")
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
        layerId     = ~CD_FCU,
        highlightOptions = highlightOptions(
          weight = 2, color = "#FFD700",
          fillOpacity = 0.95, bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = pal, values = vals, title = col,
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
  # Tab: Indicadores
  # -----------------------------------------------------------------------
  chart_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 9),
          plot.title  = element_blank())

  output$chart_top20 <- renderPlot({
    col <- ind_col_df(); nome <- ind_nome(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    worst <- isTRUE(input$show_worst)
    df_c  <- df[!is.na(df[[col]]), ]
    df_o  <- df_c[order(df_c[[col]], decreasing = !worst), ]
    df_p  <- df_o[seq_len(min(20, nrow(df_o))), ]
    df_p$lbl <- fct_reorder(paste0(df_p$nm_fcu, " (", df_p$nm_mun, ")"), df_p[[col]])
    pal_opt <- if (worst) "B" else "D"
    cap_txt <- if (worst) "20 piores na seleção atual" else "Top 20 na seleção atual"
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
      geom_histogram(bins = 10, fill = "#3B82F6", colour = "white", linewidth = 0.3) +
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
             IDS, IDA,
             any_of(c("pct_pretos_pardos", "pct_indigenas",
                      "pct_under5", "pct_under19", "pct_under30", "pct_idoso", "pct_chefe_mulher",
                      "CMAET60", "CMAST60", "CMATT60", "CMACT60", "tem_risco"))) %>%
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
      ) %>%
      rename_with(~ case_match(.x,
        "pct_pretos_pardos" ~ "Pretos e pardos (%)",
        "pct_indigenas"     ~ "Indígenas (%)",
        "pct_under5"        ~ "Menores de 5 anos (%)",
        "pct_under19"       ~ "Menores de 19 anos (%)",
        "pct_under30"       ~ "Menores de 30 anos (%)",
        "pct_idoso"         ~ "Idosos 60+ anos (%)",
        "pct_chefe_mulher"  ~ "Domicílios c/ chefe mulher (%)",
        .default = .x
      ))
  })

  # Estima quantas favelas e o tamanho aproximado do CSV da seleção atual.
  # O tamanho é estimado a partir de uma amostra de linhas (barato no reativo).
  download_info <- reactive({
    df <- tabela_export()
    n  <- nrow(df)
    if (n == 0) return(list(n = 0, bytes = 0))
    samp_n   <- min(200L, n)
    samp_txt <- readr::format_csv2(df[seq_len(samp_n), , drop = FALSE])
    bytes_pr <- nchar(samp_txt, type = "bytes") / samp_n
    list(n = n, bytes = bytes_pr * n)
  })

  output$download_note <- renderUI({
    info <- download_info()
    fmt_size <- function(b) {
      if (b <= 0)          "—"
      else if (b < 1024)   paste0(round(b), " B")
      else if (b < 1024^2) paste0(round(b / 1024), " KB")
      else                 paste0(round(b / 1024^2, 1), " MB")
    }
    uf <- input$sel_uf
    escopo <- if (length(uf) > 0 && !all(uf == "")) {
      paste0("UF selecionada(s): ", paste(uf, collapse = ", "))
    } else {
      "todas as favelas do Brasil (nenhum filtro aplicado)"
    }
    n_fmt <- formatC(info$n, format = "d", big.mark = ".")
    div(
      class = "alert alert-info",
      style = "padding:8px 12px;margin-bottom:12px;font-size:0.9rem;",
      tags$i(class = "fa fa-circle-info", style = "margin-right:6px;"),
      HTML(sprintf(
        "O bot\u00e3o <b>Baixar CSV</b> exporta exatamente a sele\u00e7\u00e3o atual desta tabela: <b>%s favelas</b> (%s), com todas as colunas exibidas. Tamanho aproximado do arquivo: <b>~%s</b>.",
        n_fmt, escopo, fmt_size(info$bytes)
      ))
    )
  })

  output$tabela_dados <- renderDT({
    datatable(tabela_export(), rownames = FALSE, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "frtip",
        # Avisa o servidor quando o DataTables termina de desenhar (ponto B)
        drawCallback = htmlwidgets::JS(
          "function(settings) { Shiny.setInputValue('table_drawn', new Date().getTime()); }"
        ))) %>%
      formatRound(columns = c("IDS", "IDA", "Renda média (SM)"), digits = 3) %>%
      formatRound(columns = intersect(
        c("Água encanada (%)", "Esgoto rede geral (%)", "Coleta de lixo (%)",
          "Analfabetismo 15+ (%)", "Via pavimentada (%)", "Bueiro (%)",
          "Iluminação pública (%)", "Ponto de ônibus (%)", "Ciclovia (%)",
          "Calçada (%)", "Obstáculo calçada (%)", "Rampa cadeirante (%)",
          "Pretos e pardos (%)", "Indígenas (%)",
          "Menores de 5 anos (%)", "Menores de 19 anos (%)", "Menores de 30 anos (%)", "Idosos 60+ anos (%)",
          "Domicílios c/ chefe mulher (%)"),
        names(tabela_export())
      ), digits = 1) %>%
      formatCurrency(columns = c("População", "Domicílios ocupados"),
                     currency = "", interval = 3, mark = ".", digits = 0)
  })

  # -----------------------------------------------------------------------
  # Estado de carregamento do botão de download (ponto B):
  # desabilita assim que o filtro muda (tabela vai ser recalculada) e
  # reabilita somente quando o DataTables efetivamente desenha no cliente.
  # O botão já nasce desabilitado (classe .disabled na UI), então só fica
  # ativo após o primeiro desenho da tabela.
  # -----------------------------------------------------------------------
  observeEvent(input$sel_uf, {
    session$sendCustomMessage("toggle_download", list(enabled = FALSE))
  }, ignoreInit = TRUE)

  observeEvent(input$table_drawn, {
    session$sendCustomMessage("toggle_download", list(enabled = TRUE))
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      u <- input$sel_uf
      uf_str <- if (length(u) > 0 && !all(u == "")) paste0("_", paste(u, collapse = "-")) else ""
      paste0("favelas_br", uf_str, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) { write_csv2(tabela_export(), file) }
  )
}

# =========================================================================
# JS / CSS
# =========================================================================
ui_with_js <- tagList(
  tags$head(
    tags$style(HTML("
      .leaflet-popup-content {
        width: 850px !important;
        max-width: 1200px !important;
      }
      /* Limita a largura maxima do app e centraliza em telas muito grandes */
      body {
        max-width: 1600px;
        margin-left: auto !important;
        margin-right: auto !important;
      }
      /* Spinner de carregamento sobre a tabela enquanto recalcula (ponto B) */
      #tabela_dados { position: relative; min-height: 140px; }
      #tabela_dados.recalculating::after {
        content: '';
        position: absolute; top: 60px; left: 50%;
        width: 46px; height: 46px; margin-left: -23px;
        border: 5px solid #d1d5db; border-top-color: #18BC9C;
        border-radius: 50%; animation: favspin 0.8s linear infinite;
        z-index: 1000;
      }
      @keyframes favspin { to { transform: rotate(360deg); } }
    ")),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('zoom_to_favela', function(msg) {
        // Retry until the map widget is ready (it may still be rendering)
        var attempts = 0;
        var tryZoom = function() {
          var widget = window.HTMLWidgets ? window.HTMLWidgets.find('#mapa') : null;
          if (widget) {
            var map = widget.getMap();
            if (map) {
              map.setView([msg.lat, msg.lng], msg.zoom);
              // Open popup for the target layer if it exists
              map.eachLayer(function(layer) {
                if (layer.options && layer.options.layerId === msg.cd_fcu) {
                  layer.openPopup();
                }
              });
              return;
            }
          }
          if (++attempts < 20) setTimeout(tryZoom, 150);
        };
        setTimeout(tryZoom, 400);
      });"
    )),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('toggle_download', function(msg) {
        var btn = document.getElementById('download_csv');
        if (!btn) return;
        if (msg.enabled) {
          btn.classList.remove('disabled');
          btn.removeAttribute('aria-disabled');
        } else {
          btn.classList.add('disabled');
          btn.setAttribute('aria-disabled', 'true');
        }
      });"
    ))
  ),
  ui
)

shinyApp(ui_with_js, server)
