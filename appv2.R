# Favelas BR — Plataforma de Análise
# Versões: Desktop (PT) · Mobile (PT, sem mapa/popup/download) · English (Census lingo)
# O usuário escolhe a versão na página de boas-vindas.

GEOJSON_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_br_simplified.geojson"
CSV_IDS_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_ids_ida.csv"
CSV_RISK_URL <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_riscos.csv"
CSV_AOP_URL  <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_acesso_oportunidades.csv"
CSV_DEMO_URL <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas_demograficos_2022.csv"
LOGO_URL     <- "https://raw.githubusercontent.com/brunopantaleao/favela_dados/main/favelas-logo-2%20(5).png"

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
# INDICATOR CATALOGUE (PT — original) + ENGLISH LABELS (Census lingo)
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

ind_cols    <- sapply(indicadores, `[[`, "col")
ind_choices <- setNames(ind_cols, sapply(indicadores, `[[`, "label"))
ind_groups  <- sapply(indicadores, `[[`, "group")
ind_grouped <- split(ind_choices, ind_groups)
ind_label   <- setNames(sapply(indicadores, `[[`, "label"), ind_cols)

# --- English labels (IBGE / Census terminology) ---
ind_label_en <- c(
  IDS               = "IDS - Social Development Index",
  IDA               = "IDA - Urban Accessibility Index",
  PERC_AGUA         = "Piped water (%)",
  PERC_ESGO         = "Sewage disposal via general network (%)",
  PERC_LIXO         = "Garbage collected (%)",
  RENDA_SM          = "Mean income (minimum wages)",
  PERC_ANALF        = "Illiteracy rate, persons 15+ (%)",
  P_VIAPAV          = "Paved street (%)",
  P_BUEIRO          = "Storm drain (%)",
  P_ILUM            = "Public lighting (%)",
  P_ONTON           = "Bus stop (%)",
  P_VIABIC          = "Bicycle lane (%)",
  P_CALCAD          = "Sidewalk (%)",
  P_OBSTAC          = "Sidewalk obstruction (%)",
  P_RAMPA           = "Wheelchair ramp (%)",
  tem_risco         = "Exposure to natural hazard (flag)",
  PCT_PRETOS_PARDOS = "Black and Brown population (%)",
  PCT_INDIGENAS     = "Indigenous population (%)",
  PCT_UNDER5        = "Persons under 5 years of age (%)",
  PCT_UNDER19       = "Persons under 19 years of age (%)",
  PCT_UNDER30       = "Persons under 30 years of age (%)",
  PCT_IDOSO         = "Persons 60 years of age and over (%)",
  PCT_CHEFE_MULHER  = "Female-headed households (%)"
)

group_map_en <- c(
  "Índices"                 = "Composite Indices",
  "Saneamento"              = "Sanitation",
  "Renda e Educação"        = "Income and Education",
  "Acessibilidade Urbana"   = "Urban Accessibility",
  "Riscos Geológicos (SGB)" = "Geological Hazards (SGB)",
  "Demografia"              = "Demographics"
)

ind_choices_en <- setNames(ind_cols, unname(ind_label_en[ind_cols]))
ind_grouped_en <- split(ind_choices_en, unname(group_map_en[ind_groups]))

ind_label_lang   <- list(pt = ind_label,   en = ind_label_en)
ind_grouped_lang <- list(pt = ind_grouped, en = ind_grouped_en)

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
# I18N — interface strings
# =========================================================================
i18n <- list(
  pt = list(
    filters        = "Filtros",
    search_label   = "Buscar favela",
    search_ph      = "Digite o nome da favela...",
    uf_label       = "Estado (UF)",
    uf_all         = "Todos",
    sources        = "Dados: Censo IBGE 2022 · IBGE FCU 2022 · Serviço Geológico Brasileiro · IPEA 2019",
    nav_home       = "Início",
    nav_map        = " Mapa",
    nav_ind        = " Indicadores",
    nav_data       = " Dados",
    nav_meta       = " Metadados",
    switch_version = "⇄ Trocar versão",
    hero_title     = "Favela Dados",
    hero_sub       = "Um repositório público de dados quantitativos sobre favelas e comunidades urbanas do Brasil — em um mapa interativo, fácil de usar, sem precisar programar.",
    card1_t = "O que é",
    card1_p = "12.348 Favelas e Comunidades Urbanas (FCUs) mapeadas em todo o território nacional, com indicadores de saneamento, renda, acessibilidade e risco.",
    card2_t = "De onde vêm os dados",
    card2_p = "Censo IBGE 2022, risco geológico (Serviço Geológico Brasileiro) e acesso a oportunidades (Instituto de Pesquisa Econômica Aplicada (IPEA)), integrados por favela. Os dados foram organizados por Bruno Pantaleão e Isabella Montini.",
    card3_t = "Para quem é",
    card3_p = "Pesquisadores, gestores públicos, jornalistas, movimentos sociais e moradores — filtre, visualize e baixe os dados sem precisar programar.",
    cite_t  = "Como Citar?",
    cite_p  = "Pantaleão, B. and Montini, I. (2026). Presenting the Favelas Dados dataset, a bird’s-eye view of Brazilian favelas and urban communities. Available at: https://doi.org/10.31235/osf.io/97yb5_v1",
    go_map  = "Explorar o mapa →",
    map_title_empty = "Mapa — selecione um Estado (UF)",
    map_title_fmt   = "Clique em uma favela pra saber mais sobre ela | %s favelas",
    map_empty_h     = "Selecione um Estado ou busque uma favela",
    map_empty_p     = "Use os filtros na barra lateral.",
    ind_sel_label   = "Indicador",
    worst_label     = "Piores 20",
    dist_header     = "Distribuição do indicador",
    cap_top         = "Top 20 na seleção atual",
    cap_worst       = "20 piores na seleção atual",
    hist_y          = "Nº de favelas",
    median_lbl      = "Mediana: ",
    data_title_fmt  = "%s favelas selecionadas",
    dl_btn          = "Baixar CSV",
    dl_note_fmt     = "O bot\u00e3o <b>Baixar CSV</b> exporta exatamente a sele\u00e7\u00e3o atual desta tabela: <b>%s favelas</b> (%s), com todas as colunas exibidas. Tamanho aproximado do arquivo: <b>~%s</b>.",
    scope_uf        = "UF selecionada(s): ",
    scope_all       = "todas as favelas do Brasil (nenhum filtro aplicado)"
  ),
  en = list(
    filters        = "Filters",
    search_label   = "Search favela",
    search_ph      = "Type the favela name...",
    uf_label       = "State (UF)",
    uf_all         = "All",
    sources        = "Data: IBGE 2022 Population Census · IBGE FCU 2022 · Geological Survey of Brazil · IPEA 2019",
    nav_home       = "Home",
    nav_map        = " Map",
    nav_ind        = " Indicators",
    nav_data       = " Data",
    nav_meta       = " Metadata",
    switch_version = "⇄ Switch version",
    hero_title     = "Favela Dados",
    hero_sub       = "A public repository of quantitative data on Brazil's favelas and urban communities — in an interactive, easy-to-use map, no coding required.",
    card1_t = "What it is",
    card1_p = "12,348 Favelas and Urban Communities (FCUs) mapped across the national territory, with indicators on sanitation, income, accessibility and hazard exposure.",
    card2_t = "Where the data come from",
    card2_p = "IBGE 2022 Population Census, geological hazard mapping (Geological Survey of Brazil) and access to opportunities (Institute for Applied Economic Research — IPEA), integrated at the favela level. Data organized by Bruno Pantaleão and Isabella Montini.",
    card3_t = "Who it is for",
    card3_p = "Researchers, public managers, journalists, social movements and residents — filter, visualize and download the data without coding.",
    cite_t  = "How to cite",
    cite_p  = "Pantaleão, B. and Montini, I. (2026). Presenting the Favelas Dados dataset, a bird’s-eye view of Brazilian favelas and urban communities. Available at: https://doi.org/10.31235/osf.io/97yb5_v1",
    go_map  = "Explore the map →",
    map_title_empty = "Map — select a State (UF)",
    map_title_fmt   = "Click a favela to learn more about it | %s favelas",
    map_empty_h     = "Select a State or search for a favela",
    map_empty_p     = "Use the filters in the sidebar.",
    ind_sel_label   = "Indicator",
    worst_label     = "Bottom 20",
    dist_header     = "Indicator distribution",
    cap_top         = "Top 20 in current selection",
    cap_worst       = "Bottom 20 in current selection",
    hist_y          = "Number of favelas",
    median_lbl      = "Median: ",
    data_title_fmt  = "%s favelas selected",
    dl_btn          = "Download CSV",
    dl_note_fmt     = "The <b>Download CSV</b> button exports exactly the current selection in this table: <b>%s favelas</b> (%s), with all displayed columns. Approximate file size: <b>~%s</b>.",
    scope_uf        = "Selected UF(s): ",
    scope_all       = "all favelas in Brazil (no filter applied)"
  )
)

# Pretty column names for the data table / CSV, per language
col_pretty <- list(
  pt = c(
    cd_fcu                    = "Código FCU",
    nm_fcu                    = "Nome da favela",
    nm_mun                    = "Município",
    nm_uf                     = "UF",
    total_pessoas             = "População",
    total_dp_ocupados         = "Domicílios ocupados",
    perc_agua_adequada        = "Água encanada (%)",
    perc_esgoto_adequado      = "Esgoto rede geral (%)",
    perc_lixo_coleta          = "Coleta de lixo (%)",
    renda_sm_pond             = "Renda média (SM)",
    perc_analfabeto_populacao = "Analfabetismo 15+ (%)",
    perc_via_pavimentada      = "Via pavimentada (%)",
    perc_bueiro               = "Bueiro (%)",
    perc_iluminacao_publica   = "Iluminação pública (%)",
    perc_ponto_onibus         = "Ponto de ônibus (%)",
    perc_via_bicicleta        = "Ciclovia (%)",
    perc_calcada              = "Calçada (%)",
    perc_obstaculo_calcada    = "Obstáculo calçada (%)",
    perc_rampa_cadeirante     = "Rampa cadeirante (%)",
    IDS                       = "IDS",
    IDA                       = "IDA",
    pct_pretos_pardos         = "Pretos e pardos (%)",
    pct_indigenas             = "Indígenas (%)",
    pct_under5                = "Menores de 5 anos (%)",
    pct_under19               = "Menores de 19 anos (%)",
    pct_under30               = "Menores de 30 anos (%)",
    pct_idoso                 = "Idosos 60+ anos (%)",
    pct_chefe_mulher          = "Domicílios c/ chefe mulher (%)"
  ),
  en = c(
    cd_fcu                    = "FCU code",
    nm_fcu                    = "Favela name",
    nm_mun                    = "Municipality",
    nm_uf                     = "State (UF)",
    total_pessoas             = "Population",
    total_dp_ocupados         = "Occupied housing units",
    perc_agua_adequada        = "Piped water (%)",
    perc_esgoto_adequado      = "Sewage via general network (%)",
    perc_lixo_coleta          = "Garbage collected (%)",
    renda_sm_pond             = "Mean income (min. wages)",
    perc_analfabeto_populacao = "Illiteracy 15+ (%)",
    perc_via_pavimentada      = "Paved street (%)",
    perc_bueiro               = "Storm drain (%)",
    perc_iluminacao_publica   = "Public lighting (%)",
    perc_ponto_onibus         = "Bus stop (%)",
    perc_via_bicicleta        = "Bicycle lane (%)",
    perc_calcada              = "Sidewalk (%)",
    perc_obstaculo_calcada    = "Sidewalk obstruction (%)",
    perc_rampa_cadeirante     = "Wheelchair ramp (%)",
    IDS                       = "IDS",
    IDA                       = "IDA",
    pct_pretos_pardos         = "Black and Brown (%)",
    pct_indigenas             = "Indigenous (%)",
    pct_under5                = "Under 5 years (%)",
    pct_under19               = "Under 19 years (%)",
    pct_under30               = "Under 30 years (%)",
    pct_idoso                 = "60 years and over (%)",
    pct_chefe_mulher          = "Female-headed households (%)"
  )
)

apply_pretty <- function(df, lang) {
  m  <- col_pretty[[lang]]
  nm <- names(df)
  hit <- nm %in% names(m)
  nm[hit] <- unname(m[nm[hit]])
  names(df) <- nm
  df
}

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

# Popup labels per language
popup_labels <- list(
  pt = list(
    sec_idx = "Índices compostos", sec_san = "Saneamento",
    water = "Água encanada", sewer = "Esgoto rede geral", garb = "Coleta de lixo",
    sec_inc = "Renda e Educação", income = "Renda média", illit = "Analfabetismo 15+",
    sec_gen = "Geral", pop = "População",
    sec_risk = "Riscos Geológicos (SGB)", exposed = "Exposto a risco", rclass = "Classe de risco",
    s_geo = "Setores c/ risco geológico", s_flood = "Setores c/ inundação",
    s_flash = "Setores c/ enxurrada", s_mass = "Setores c/ corrida de massa", s_danger = "Setores c/ perigo",
    sec_acc = "Acessibilidade Urbana",
    paved = "Via pavimentada", drain = "Bueiro", light = "Iluminação pública",
    bus = "Ponto de ônibus", bike = "Ciclovia", walk = "Calçada",
    obst = "Obstáculo calçada", ramp = "Rampa cadeirante",
    sec_opp = "Oportunidades <small>(60min/transporte público)</small>",
    opp_school = "Escolas públicas acessíveis", opp_health = "Hospitais/UPAs acessíveis",
    opp_jobs = "Empregos acessíveis", opp_cras = "CRAS acessíveis",
    sec_demo = "Demografia (Censo 2022)",
    blk = "Pretos e pardos", indi = "Indígenas",
    u5 = "Menores de 5 anos", u19 = "Menores de 19 anos", u30 = "Menores de 30 anos",
    old = "Idosos 60+ anos", fem = "Domicílios c/ chefe mulher",
    yes = "Sim", no = "Não", sm_unit = " SM"
  ),
  en = list(
    sec_idx = "Composite indices", sec_san = "Sanitation",
    water = "Piped water", sewer = "Sewage via general network", garb = "Garbage collected",
    sec_inc = "Income and Education", income = "Mean income", illit = "Illiteracy 15+",
    sec_gen = "General", pop = "Population",
    sec_risk = "Geological Hazards (SGB)", exposed = "Exposed to hazard", rclass = "Hazard class",
    s_geo = "Tracts w/ geological hazard", s_flood = "Tracts w/ flooding",
    s_flash = "Tracts w/ flash flood", s_mass = "Tracts w/ mass movement", s_danger = "Tracts w/ danger",
    sec_acc = "Urban Accessibility",
    paved = "Paved street", drain = "Storm drain", light = "Public lighting",
    bus = "Bus stop", bike = "Bicycle lane", walk = "Sidewalk",
    obst = "Sidewalk obstruction", ramp = "Wheelchair ramp",
    sec_opp = "Opportunities <small>(60min/public transit)</small>",
    opp_school = "Accessible public schools", opp_health = "Accessible hospitals/UPAs",
    opp_jobs = "Accessible jobs", opp_cras = "Accessible CRAS",
    sec_demo = "Demographics (2022 Census)",
    blk = "Black and Brown", indi = "Indigenous",
    u5 = "Under 5 years", u19 = "Under 19 years", u30 = "Under 30 years",
    old = "60 years and over", fem = "Female-headed households",
    yes = "Yes", no = "No", sm_unit = " MW"
  )
)

make_popup <- function(sf_obj, lang = "pt") {
  P <- popup_labels[[lang]]

  fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(round(x, 1), "%"))
  fmt_num <- function(x) ifelse(is.na(x), "—", as.character(round(x, 3)))
  fmt_sm  <- function(x) ifelse(is.na(x), "—", paste0(round(x, 2), P$sm_unit))
  fmt_n   <- function(x) ifelse(is.na(x) | x == 0,  "—", as.character(x))
  flag    <- function(x) ifelse(is.na(x) | x == 0,  P$no, P$yes)

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

  # Template with labels baked in, data slots as %s
  tpl <- paste0(
    "<div style='font-family:sans-serif;min-width:500px;'>",
    "<b style='font-size:1.05em;'>%s</b>",
    "<hr style='margin:4px 0;'>",
    "<table style='width:100%%;font-size:1em;'><tr>",
    "<td style='width:50%%;vertical-align:top;padding-right:20px;'>",
    "<table style='width:100%%;border-collapse:collapse;'>",
    "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>", P$sec_idx, "</td></tr>",
    "<tr><td>IDS</td><td><b>%s</b></td></tr>",
    "<tr><td>IDA</td><td><b>%s</b></td></tr>",
    "<tr><td colspan='2' style='padding:3px 0;'></td></tr>",
    "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>", P$sec_san, "</td></tr>",
    "<tr><td>", P$water, "</td><td>%s</td></tr>",
    "<tr><td>", P$sewer, "</td><td>%s</td></tr>",
    "<tr><td>", P$garb,  "</td><td>%s</td></tr>",
    "<tr><td colspan='2' style='padding:3px 0;'></td></tr>",
    "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>", P$sec_inc, "</td></tr>",
    "<tr><td>", P$income, "</td><td>%s</td></tr>",
    "<tr><td>", P$illit,  "</td><td>%s</td></tr>",
    "<tr><td colspan='2' style='padding:3px 0;'></td></tr>",
    "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>", P$sec_gen, "</td></tr>",
    "<tr><td>", P$pop, "</td><td>%s</td></tr>",
    "<tr><td colspan='2' style='padding:4px 0;'></td></tr>",
    "<tr><td colspan='2' style='background:#fef2f2;padding:2px 4px;font-weight:600;color:#991b1b;'>", P$sec_risk, "</td></tr>",
    "<tr><td>", P$exposed, "</td><td>%s</td></tr>",
    "<tr><td>", P$rclass,  "</td><td>%s</td></tr>",
    "<tr><td>", P$s_geo,   "</td><td>%s</td></tr>",
    "<tr><td>", P$s_flood, "</td><td>%s</td></tr>",
    "<tr><td>", P$s_flash, "</td><td>%s</td></tr>",
    "<tr><td>", P$s_mass,  "</td><td>%s</td></tr>",
    "<tr><td>", P$s_danger,"</td><td>%s</td></tr>",
    "</table></td>",
    "<td style='width:50%%;vertical-align:top;padding-left:20px;padding-right:10px;border-left:1px solid #eee;'>",
    "<table style='width:100%%;border-collapse:collapse;'>",
    "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>", P$sec_acc, "</td></tr>",
    "<tr><td>", P$paved, "</td><td>%s</td></tr>",
    "<tr><td>", P$drain, "</td><td>%s</td></tr>",
    "<tr><td>", P$light, "</td><td>%s</td></tr>",
    "<tr><td>", P$bus,   "</td><td>%s</td></tr>",
    "<tr><td>", P$bike,  "</td><td>%s</td></tr>",
    "<tr><td>", P$walk,  "</td><td>%s</td></tr>",
    "<tr><td>", P$obst,  "</td><td>%s</td></tr>",
    "<tr><td>", P$ramp,  "</td><td>%s</td></tr>",
    "%s",
    "<tr><td colspan='2' style='padding:4px 0;'></td></tr>",
    "<tr><td colspan='2' style='background:#eff6ff;padding:2px 4px;font-weight:600;color:#1e40af;'>", P$sec_demo, "</td></tr>",
    "<tr><td>", P$blk,  "</td><td>%s</td></tr>",
    "<tr><td>", P$indi, "</td><td>%s</td></tr>",
    "<tr><td>", P$u5,   "</td><td>%s</td></tr>",
    "<tr><td>", P$u19,  "</td><td>%s</td></tr>",
    "<tr><td>", P$u30,  "</td><td>%s</td></tr>",
    "<tr><td>", P$old,  "</td><td>%s</td></tr>",
    "<tr><td>", P$fem,  "</td><td>%s</td></tr>",
    "</table></td></tr></table></div>"
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
      if (!is.na(CMAET60)) sprintf("<tr><td>%s</td><td>%s</td></tr>", P$opp_school, formatC(round(CMAET60), format="d", big.mark=".")) else "",
      if (!is.na(CMAST60)) sprintf("<tr><td>%s</td><td>%s</td></tr>", P$opp_health, formatC(round(CMAST60), format="d", big.mark=".")) else "",
      if (!is.na(CMATT60)) sprintf("<tr><td>%s</td><td>%s</td></tr>", P$opp_jobs,   formatC(round(CMATT60), format="d", big.mark=".")) else "",
      if (!is.na(CMACT60)) sprintf("<tr><td>%s</td><td>%s</td></tr>", P$opp_cras,   formatC(round(CMACT60), format="d", big.mark=".")) else ""
    )
    aop_block <- if (nchar(aop_rows) > 0) paste0(
      "<tr><td colspan='2' style='padding:4px 0;'></td></tr>",
      "<tr><td colspan='2' style='background:#f0f0f0;padding:2px 4px;font-weight:600;'>",
      P$sec_opp, "</td></tr>",
      aop_rows
    ) else ""

    sprintf(
      tpl,
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
# METADATA PAGES (PT + EN)
# =========================================================================
build_metadata_pt <- function() {
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
}

build_metadata_en <- function() {
  card(card_body(
    tags$h4("About the data"),
    tags$hr(),

    tags$h5("Favela polygons (FCU)"),
    tags$p("Source: IBGE, 2022 Population Census — 12,348 Favelas and Urban Communities (FCUs) across the national territory."),

    tags$h5("IBGE 2022 Population Census — Census Tracts"),
    tags$p("Data aggregated at the census-tract level, linked to FCU polygons via the IBGE correspondence table."),
    tags$ul(
      tags$li(tags$b("Sanitation:"), " % of housing units with piped water, sewage disposal via the general network, and garbage collection."),
      tags$li(tags$b("Income:"), " mean income of the household head in minimum wages (MW = R$ 1,212, 2022 reference)."),
      tags$li(tags$b("Education:"), " % of the population aged 15 and over that is illiterate."),
      tags$li(tags$b("Surroundings:"), " % of block faces with paved street, storm drain, public lighting, bus stop, bicycle lane, sidewalk, sidewalk obstruction, and wheelchair ramp.")
    ),

    tags$h5("Demographics (2022 Census)"),
    tags$p("Computed from the 2022 Census Aggregates by Census Tract (universe results), via IBGE's public FTP."),
    tags$ul(
      tags$li(tags$b("Black and Brown (%):"), " sum of the Black (preta) and Brown (parda) categories over the total tract population."),
      tags$li(tags$b("Indigenous (%):"), " Indigenous category over the total population."),
      tags$li(tags$b("Under 5 years (%):"), " ages 0–4 (exact)."),
      tags$li(tags$b("Under 19 years (%):"), " age groups 0–4 + 5–9 + 10–14 + 15–19 (exact — includes the entire 15–19 group)."),
      tags$li(tags$b("Under 30 years (%):"), " age groups 0–4 through 25–29 (exact)."),
      tags$li(tags$b("Persons 60 years and over (%):"), " age groups 60–69 + 70 and over (exact)."),
      tags$li(tags$b("Female-headed households (%):"), " occupied permanent private households with a woman as the person responsible for the household (Basico, v0007 / v0002).")
    ),

    tags$h5("Composite Indices"),
    tags$ul(
      tags$li(tags$b("IDS — Social Development Index:"), " mean of 6 min-max normalized components: water, sewage, garbage, bathrooms per resident, literacy and income."),
      tags$li(tags$b("IDA — Urban Accessibility Index:"), " mean of 8 normalized components: paved street, storm drain, public lighting, bus stop, bicycle lane, sidewalk, obstruction (inverted) and ramp.")
    ),

    tags$h5("Geological Hazards — SGB/CPRM"),
    tags$p("Binary variable: 1 if the FCU polygon intersects a geological hazard area mapped by the Geological Survey of Brazil."),

    tags$h5("Access to Opportunities — AOP (aopdata / IPEA, 2019)"),
    tags$p("Data available for the 20 most populous municipalities."),
    tags$ul(
      tags$li(tags$b("CMAET60:"), " public schools accessible within ≤ 60 min by public transit."),
      tags$li(tags$b("CMAST60:"), " hospitals/UPAs accessible within ≤ 60 min by public transit."),
      tags$li(tags$b("CMATT60:"), " formal jobs accessible within ≤ 60 min by public transit."),
      tags$li(tags$b("CMACT60:"), " CRAS (social assistance centers) accessible within ≤ 60 min by public transit.")
    ),

    tags$h5("Methodological note"),
    tags$p("Census-tract indicators were aggregated to the FCU level using population-weighted means. Min-max normalization was computed over urban tracts of municipalities with at least one favela.")
  ))
}

# =========================================================================
# UI BUILDERS
# =========================================================================

# --- Welcome / version selector page -------------------------------------
build_home_ui <- function() {
  tagList(
    div(
      style = "text-align:center;background:linear-gradient(135deg, #f76338 0%, #611ce3 100%);color:white;padding:56px 20px;border-radius:8px;margin:16px 0 24px 0;",
      tags$img(src = LOGO_URL, height = "84px", style = "margin-bottom:16px;"),
      tags$h2("Favela Dados", style = "font-weight:700;"),
      tags$p(i18n$pt$hero_sub, style = "font-size:1.05rem;max-width:640px;margin:0 auto;"),
      tags$p(i18n$en$hero_sub, style = "font-size:0.9rem;max-width:640px;margin:8px auto 0 auto;opacity:0.85;")
    ),
    tags$h5("Escolha a versão · Choose your version",
            style = "text-align:center;color:#611ce3;font-weight:600;margin-bottom:16px;"),
    layout_columns(col_widths = c(4, 4, 4),
      card(class = "version-card", card_body(
        tags$div(tags$i(class = "fa fa-desktop fa-2x", style = "color:#611ce3;"), style = "margin-bottom:8px;"),
        tags$h5("Versão Completa", style = "color:#611ce3;font-weight:700;"),
        tags$p("Mapa interativo, gráficos, tabela completa e download dos dados em CSV.", class = "text-muted",
               style = "min-height:64px;"),
        actionButton("btn_desktop_pt", "Entrar →", class = "btn-lg w-100",
                     style = "background:#611ce3;color:white;border:none;font-weight:600;")
      )),
      card(class = "version-card", card_body(
        tags$div(tags$i(class = "fa fa-mobile-screen fa-2x", style = "color:#f76338;"), style = "margin-bottom:8px;"),
        tags$h5("Versão Mobile", style = "color:#f76338;font-weight:700;"),
        tags$p("Leve e direta ao ponto: ficha completa da favela, gráficos e tabela — sem mapa.", class = "text-muted",
               style = "min-height:64px;"),
        actionButton("btn_mobile", "Abrir versão mobile →", class = "btn-lg w-100",
                     style = "background:#f76338;color:white;border:none;font-weight:600;")
      )),
      card(class = "version-card", card_body(
        tags$div(tags$i(class = "fa fa-globe fa-2x", style = "color:#3a2a00;"), style = "margin-bottom:8px;"),
        tags$h5("English Version", style = "color:#3a2a00;font-weight:700;"),
        tags$p("The full application in English, using IBGE / Census terminology.", class = "text-muted",
               style = "min-height:64px;"),
        actionButton("btn_desktop_en", "Open English version →", class = "btn-lg w-100",
                     style = "background:#fad142;color:#3a2a00;border:none;font-weight:600;")
      ))
    ),
    card(card_body(
      tags$h5(paste0(i18n$pt$cite_t, " · ", i18n$en$cite_t), style = "color:#611ce3;"),
      tags$p(i18n$pt$cite_p)
    ))
  )
}

# --- Desktop app (PT or EN) ----------------------------------------------
build_desktop_ui <- function(lang) {
  L <- i18n[[lang]]

  navset_bar(
    id       = "main_nav",
    selected = "home",
    title    = tagList(
      tags$img(src = LOGO_URL, height = "32px", style = "margin-right:8px;vertical-align:middle;")
    ),
    bg = "#f76338",   # favelas.br orange — top navbar

    sidebar = sidebar(
      width = 270,
      title = L$filters,
      conditionalPanel(
        condition = "input.main_nav != 'home'",
        # Favela search — server-side selectize, activates after 3 chars
        selectizeInput(
          "search_fav", L$search_label,
          choices  = NULL,
          selected = NULL,
          options  = list(
            placeholder        = L$search_ph,
            minimumInputLength = 3,
            maxOptions         = 20,
            # Garante que nenhuma favela venha selecionada ao abrir o app
            onInitialize       = I('function() { this.setValue(""); }')
          )
        ),
        hr(),
        selectInput("sel_uf", L$uf_label, choices = setNames(c("", ufs), c(L$uf_all, ufs)),
                    selected = "", multiple = TRUE),
        hr(),
        p(tags$small(tags$i(L$sources)))
      )
    ),

    nav_panel(
      title = L$nav_home,
      value = "home",
      div(
        style = "text-align:center;background:linear-gradient(135deg, #f76338 0%, #611ce3 100%);color:white;padding:56px 20px;border-radius:8px;margin-bottom:24px;",
        tags$img(src = LOGO_URL, height = "84px", style = "margin-bottom:16px;"),
        tags$h2(L$hero_title, style = "font-weight:700;"),
        tags$p(L$hero_sub, style = "font-size:1.1rem;max-width:640px;margin:0 auto;")
      ),
      layout_columns(col_widths = c(4, 4, 4),
        card(card_body(
          tags$h5(L$card1_t, style = "color:#611ce3;"),
          tags$p(L$card1_p)
        )),
        card(card_body(
          tags$h5(L$card2_t, style = "color:#611ce3;"),
          tags$p(L$card2_p)
        )),
        card(card_body(
          tags$h5(L$card3_t, style = "color:#611ce3;"),
          tags$p(L$card3_p)
        ))
      ),
      card(card_body(
        tags$h5(L$cite_t, style = "color:#611ce3;"),
        tags$p(L$cite_p)
      )),
      div(
        style = "text-align:center;margin-top:8px;",
        actionButton(
          "go_to_map", L$go_map,
          class = "btn-lg",
          style = "background:#fad142;color:#3a2a00;border:none;font-weight:600;padding:10px 28px;"
        )
      )
    ),

    nav_panel(
      title = tagList(icon("map"), L$nav_map),
      value = "map",
      card(full_screen = TRUE,
        card_header(textOutput("mapa_titulo")),
        uiOutput("mapa_ui")
      )
    ),

    nav_panel(
      title = tagList(icon("chart-bar"), L$nav_ind),
      value = "ind",
      layout_columns(col_widths = c(12),
        card(
          card_header(
            layout_columns(col_widths = c(8, 4),
              selectInput("sel_ind", L$ind_sel_label, choices = ind_grouped_lang[[lang]], selected = "IDS"),
              checkboxInput("show_worst", L$worst_label, value = FALSE)
            )
          ),
          card_body(plotOutput("chart_top20", height = "480px"))
        )
      ),
      layout_columns(col_widths = c(12),
        card(
          card_header(L$dist_header),
          card_body(plotOutput("chart_hist", height = "260px"))
        )
      )
    ),

    nav_panel(
      title = tagList(icon("table"), L$nav_data),
      value = "data",
      card(
        card_header(layout_columns(col_widths = c(8, 4),
          textOutput("dados_titulo"),
          # Começa desabilitado; reabilitado via JS quando a tabela desenha (ponto B)
          tagAppendAttributes(
            downloadButton("download_csv", L$dl_btn, class = "btn-sm btn-success"),
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
      title = tagList(icon("info-circle"), L$nav_meta),
      value = "meta",
      if (lang == "en") build_metadata_en() else build_metadata_pt()
    ),

    nav_spacer(),
    nav_item(
      actionLink("back_home", L$switch_version,
                 style = "color:white;font-weight:600;text-decoration:none;")
    )
  )
}

# --- Mobile app (PT) — sem mapa, sem popup, sem download -----------------
build_mobile_ui <- function() {
  div(class = "mobile-wrap",
    div(
      style = "text-align:center;background:linear-gradient(135deg, #f76338 0%, #611ce3 100%);color:white;padding:22px 14px;border-radius:8px;margin:12px 0 14px 0;",
      tags$img(src = LOGO_URL, height = "48px", style = "margin-bottom:8px;"),
      tags$h5("Favela Dados — Mobile", style = "font-weight:700;margin-bottom:4px;"),
      actionLink("back_home", "⇄ Trocar versão",
                 style = "color:#fad142;font-weight:600;text-decoration:none;font-size:0.85rem;")
    ),
    card(card_body(
      selectizeInput(
        "search_fav", "Buscar favela",
        choices  = NULL,
        selected = NULL,
        width    = "100%",
        options  = list(
          placeholder        = "Digite o nome da favela...",
          minimumInputLength = 3,
          maxOptions         = 20,
          onInitialize       = I('function() { this.setValue(""); }')
        )
      ),
      selectInput("sel_uf", "Estado (UF)", choices = c("Todos" = "", ufs),
                  selected = "", multiple = TRUE, width = "100%"),
      p(tags$small(tags$i("Dados: Censo IBGE 2022 · IBGE FCU 2022 · Serviço Geológico Brasileiro · IPEA 2019")))
    )),
    navset_pill(
      id = "mobile_nav",
      nav_panel(
        title = "Favela", value = "fav",
        div(style = "margin-top:12px;", uiOutput("fav_profile"))
      ),
      nav_panel(
        title = "Indicadores", value = "ind",
        div(style = "margin-top:12px;",
          card(card_body(
            selectInput("sel_ind", "Indicador", choices = ind_grouped, selected = "IDS", width = "100%"),
            checkboxInput("show_worst", "Piores 20", value = FALSE),
            plotOutput("chart_top20", height = "440px"),
            hr(),
            tags$b("Distribuição do indicador"),
            plotOutput("chart_hist", height = "220px")
          ))
        )
      ),
      nav_panel(
        title = "Dados", value = "data",
        div(style = "margin-top:12px;",
          card(card_body(
            textOutput("dados_titulo"),
            DTOutput("tabela_mobile")
          ))
        )
      )
    )
  )
}

# Ficha da favela (mobile) — substitui o popup do mapa
fav_profile_ui <- function(fcu) {
  row <- fav_df %>% filter(cd_fcu == as.character(fcu))
  if (nrow(row) == 0) return(NULL)
  r <- as.list(row[1, ])

  fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(round(x, 1), "%"))
  fmt_num <- function(x) ifelse(is.na(x), "—", as.character(round(x, 3)))
  fmt_sm  <- function(x) ifelse(is.na(x), "—", paste0(round(x, 2), " SM"))
  fmt_n   <- function(x) ifelse(is.na(x) | x == 0, "—", as.character(x))
  fmt_int <- function(x) ifelse(is.na(x), "—", formatC(as.integer(x), format = "d", big.mark = "."))
  flag    <- function(x) ifelse(is.na(x) | x == 0, "Não", "Sim")

  item <- function(lbl, val) {
    div(class = "fav-item", tags$span(lbl), tags$span(class = "fav-val", val))
  }
  sec <- function(title, ..., hcolor = "#611ce3", hbg = "#f0f0f0") {
    itens <- list(...)
    itens <- itens[!sapply(itens, is.null)]
    if (length(itens) == 0) return(NULL)
    div(class = "fav-sec",
        div(class = "fav-sec-h", title,
            style = paste0("color:", hcolor, ";background:", hbg, ";")),
        itens)
  }

  classe_label <- if (is.na(r$classe_risco) || r$classe_risco == "") "—" else r$classe_risco
  badge_col <- if (classe_label == "Alto") "#f59e0b" else if (classe_label == "Muito alto") "#ef4444" else "#6b7280"
  badge <- if (classe_label == "—") "—" else
    tags$span(classe_label,
              style = paste0("background:", badge_col, ";color:white;border-radius:3px;padding:1px 6px;font-size:0.82em;"))

  has_aop <- any(!is.na(c(r$CMAET60, r$CMAST60, r$CMATT60, r$CMACT60)))

  tagList(
    card(card_body(
      tags$h4(r$nm_fcu, style = "font-weight:700;margin-bottom:2px;color:#611ce3;"),
      tags$p(paste0(r$nm_mun, " · ", r$nm_uf), class = "text-muted", style = "margin-bottom:12px;"),
      layout_columns(col_widths = c(4, 4, 4),
        div(class = "fav-kpi", div(class = "fav-kpi-v", fmt_int(r$total_pessoas)), div(class = "fav-kpi-l", "População")),
        div(class = "fav-kpi", div(class = "fav-kpi-v", fmt_num(r$IDS)),           div(class = "fav-kpi-l", "IDS")),
        div(class = "fav-kpi", div(class = "fav-kpi-v", fmt_num(r$IDA)),           div(class = "fav-kpi-l", "IDA"))
      ),
      sec("Saneamento",
          item("Água encanada",     fmt_pct(r$perc_agua_adequada)),
          item("Esgoto rede geral", fmt_pct(r$perc_esgoto_adequado)),
          item("Coleta de lixo",    fmt_pct(r$perc_lixo_coleta))),
      sec("Renda e Educação",
          item("Renda média",       fmt_sm(r$renda_sm_pond)),
          item("Analfabetismo 15+", fmt_pct(r$perc_analfabeto_populacao))),
      sec("Acessibilidade Urbana",
          item("Via pavimentada",    fmt_pct(r$perc_via_pavimentada)),
          item("Bueiro",             fmt_pct(r$perc_bueiro)),
          item("Iluminação pública", fmt_pct(r$perc_iluminacao_publica)),
          item("Ponto de ônibus",    fmt_pct(r$perc_ponto_onibus)),
          item("Ciclovia",           fmt_pct(r$perc_via_bicicleta)),
          item("Calçada",            fmt_pct(r$perc_calcada)),
          item("Obstáculo calçada",  fmt_pct(r$perc_obstaculo_calcada)),
          item("Rampa cadeirante",   fmt_pct(r$perc_rampa_cadeirante))),
      if (has_aop) sec("Oportunidades (60min/transporte público)",
          if (!is.na(r$CMAET60)) item("Escolas públicas acessíveis", fmt_int(round(r$CMAET60))) else NULL,
          if (!is.na(r$CMAST60)) item("Hospitais/UPAs acessíveis",   fmt_int(round(r$CMAST60))) else NULL,
          if (!is.na(r$CMATT60)) item("Empregos acessíveis",         fmt_int(round(r$CMATT60))) else NULL,
          if (!is.na(r$CMACT60)) item("CRAS acessíveis",             fmt_int(round(r$CMACT60))) else NULL),
      sec("Riscos Geológicos (SGB)", hcolor = "#991b1b", hbg = "#fef2f2",
          item("Exposto a risco",             flag(r$tem_risco)),
          item("Classe de risco",             badge),
          item("Setores c/ risco geológico",  fmt_n(r$n_setores_risco)),
          item("Setores c/ inundação",        fmt_n(r$n_setores_inundacao)),
          item("Setores c/ enxurrada",        fmt_n(r$n_setores_enxurrada)),
          item("Setores c/ corrida de massa", fmt_n(r$n_setores_corrida)),
          item("Setores c/ perigo",           fmt_n(r$n_setores_perigo))),
      sec("Demografia (Censo 2022)", hcolor = "#1e40af", hbg = "#eff6ff",
          item("Pretos e pardos",            fmt_pct(r$pct_pretos_pardos)),
          item("Indígenas",                  fmt_pct(r$pct_indigenas)),
          item("Menores de 5 anos",          fmt_pct(r$pct_under5)),
          item("Menores de 19 anos",         fmt_pct(r$pct_under19)),
          item("Menores de 30 anos",         fmt_pct(r$pct_under30)),
          item("Idosos 60+ anos",            fmt_pct(r$pct_idoso)),
          item("Domicílios c/ chefe mulher", fmt_pct(r$pct_chefe_mulher)))
    ))
  )
}

# =========================================================================
# TOP-LEVEL UI
# =========================================================================
ui <- page_fluid(
  theme = bs_theme(
    bootswatch   = "flatly",
    base_font    = font_google("IBM Plex Sans"),
    heading_font = font_google("IBM Plex Sans"),
    primary      = "#611ce3",   # favelas.br violet (lead colour, p.11)
    success      = "#fad142"    # favelas.br yellow accent — e.g. the download button
  ),
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
      /* Contraste do botao Baixar CSV (agora amarelo, fundo claro) */
      .btn-success {
        color: #3a2a00 !important;
        font-weight: 600;
      }
      /* Spinner de carregamento sobre a tabela enquanto recalcula (ponto B) */
      #tabela_dados { position: relative; min-height: 140px; }
      #tabela_dados.recalculating::after {
        content: '';
        position: absolute; top: 60px; left: 50%;
        width: 46px; height: 46px; margin-left: -23px;
        border: 5px solid #d1d5db; border-top-color: #611ce3;
        border-radius: 50%; animation: favspin 0.8s linear infinite;
        z-index: 1000;
      }
      @keyframes favspin { to { transform: rotate(360deg); } }

      /* --- Página inicial de seleção de versão --- */
      .version-card { transition: transform .12s ease, box-shadow .12s ease; }
      .version-card:hover { transform: translateY(-3px); box-shadow: 0 8px 20px rgba(97,28,227,0.12); }

      /* --- Versão mobile --- */
      .mobile-wrap { max-width: 520px; margin: 0 auto; padding: 0 6px 24px 6px; }
      .mobile-wrap .nav-pills .nav-link.active { background-color: #611ce3; }
      .fav-sec { margin-top: 14px; }
      .fav-sec-h { font-weight: 600; padding: 3px 8px; border-radius: 4px; margin-bottom: 4px; font-size: 0.92rem; }
      .fav-item { display: flex; justify-content: space-between; padding: 5px 8px; border-bottom: 1px solid #f1f1f1; font-size: 0.92rem; }
      .fav-val { font-weight: 600; text-align: right; }
      .fav-kpi { text-align: center; background: #f8f7fc; border-radius: 6px; padding: 10px 4px; }
      .fav-kpi-v { font-weight: 700; font-size: 1.05rem; color: #611ce3; }
      .fav-kpi-l { font-size: 0.78rem; color: #6c757d; }
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
  uiOutput("app_ui")
)

# =========================================================================
# SERVER
# =========================================================================
server <- function(input, output, session) {

  # -----------------------------------------------------------------------
  # Version / language state
  # -----------------------------------------------------------------------
  mode <- reactiveVal("home_select")
  lang <- reactive({ if (identical(mode(), "desktop_en")) "en" else "pt" })
  Ltxt <- reactive({ i18n[[lang()]] })

  observeEvent(input$btn_desktop_pt, mode("desktop_pt"))
  observeEvent(input$btn_desktop_en, mode("desktop_en"))
  observeEvent(input$btn_mobile,     mode("mobile"))
  observeEvent(input$back_home,      mode("home_select"))

  output$app_ui <- renderUI({
    switch(mode(),
      home_select = build_home_ui(),
      desktop_pt  = build_desktop_ui("pt"),
      desktop_en  = build_desktop_ui("en"),
      mobile      = build_mobile_ui()
    )
  })

  is_desktop <- reactive({ mode() %in% c("desktop_pt", "desktop_en") })

  # Load search choices server-side whenever an app version is entered
  observeEvent(mode(), {
    if (mode() != "home_select") {
      updateSelectizeInput(session, "search_fav",
                           choices  = fav_search_choices,
                           selected = NULL,
                           server   = TRUE)
    }
  })

  # Landing-page CTA (desktop): jump to the map tab
  observeEvent(input$go_to_map, {
    nav_select("main_nav", selected = "map", session = session)
  })

  # -----------------------------------------------------------------------
  # Favela search
  # -----------------------------------------------------------------------
  observeEvent(input$search_fav, {
    req(input$search_fav, input$search_fav != "")

    fcu <- as.character(input$search_fav)
    row <- fav_centroids %>% filter(CD_FCU == fcu)
    if (nrow(row) == 0) return()

    # 1. Set UF filter so the map/tables render
    updateSelectInput(session, "sel_uf", selected = row$NM_UF[1])

    if (is_desktop()) {
      # 2. Zoom map to favela after a short delay (map needs to render first)
      session$sendCustomMessage("zoom_to_favela", list(
        lat  = row$lat[1],
        lng  = row$lng[1],
        zoom = 15,
        cd_fcu = fcu
      ))
    } else if (identical(mode(), "mobile")) {
      # Mobile: open the favela profile tab (no map, no popup)
      nav_select("mobile_nav", selected = "fav", session = session)
    }
  }, ignoreInit = TRUE)

  dados_filtrados <- reactive({ filter_data(fav_df, input$sel_uf, NULL) })
  sf_filtrado     <- reactive({ filter_sf(fav_sf,  input$sel_uf, NULL) })
  ind_col         <- reactive({ input$sel_ind })
  ind_nome        <- reactive({ req(input$sel_ind); ind_label_lang[[lang()]][[input$sel_ind]] })
  ind_col_df      <- reactive({ req(input$sel_ind); ind_df_col[[input$sel_ind]] })

  # -----------------------------------------------------------------------
  # Tab: Mapa (desktop only)
  # -----------------------------------------------------------------------
  output$mapa_titulo <- renderText({
    L <- Ltxt()
    if (length(input$sel_uf) == 0 || all(input$sel_uf == "")) {
      L$map_title_empty
    } else {
      sprintf(L$map_title_fmt, nrow(dados_filtrados()))
    }
  })

  output$mapa_ui <- renderUI({
    req(is_desktop())
    L <- Ltxt()
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")
    if (!uf_active) {
      div(
        style = "height:680px;display:flex;flex-direction:column;align-items:center;justify-content:center;background:#f8f9fa;border-radius:6px;color:#6c757d;",
        tags$i(class = "fa fa-map fa-3x", style = "margin-bottom:16px;color:#adb5bd;"),
        tags$h5(L$map_empty_h, style = "font-weight:500;margin-bottom:8px;"),
        tags$p(L$map_empty_p, style = "font-size:0.9rem;")
      )
    } else {
      leafletOutput("mapa", width = "100%", height = "680px")
    }
  })

  output$mapa <- renderLeaflet({
    req(is_desktop())
    uf_active <- length(input$sel_uf) > 0 && !all(input$sel_uf == "")
    req(uf_active)

    sf_obj <- sf_filtrado()
    col    <- "IDS"
    vals   <- sf_obj[[col]]
    pal    <- colorQuantile(viridis(100), domain = vals, n = 5, na.color = "#CCCCCC")
    popups <- unname(make_popup(sf_obj, lang()))

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
          weight = 2, color = "#ebde03",
          fillOpacity = 0.95, bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = pal, values = vals, title = col,
                opacity = 0.9, position = "bottomright")
  })

  observeEvent(input$sel_uf, {
    req(is_desktop())
    req(length(input$sel_uf) > 0, !all(input$sel_uf == ""))
    uf_sel <- input$sel_uf[1]
    cap    <- capitais %>% filter(nm_uf == uf_sel)
    if (nrow(cap) > 0) {
      leafletProxy("mapa") %>%
        setView(lng = cap$lng[1], lat = cap$lat[1], zoom = cap$zoom[1])
    }
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Tab: Indicadores (desktop + mobile)
  # -----------------------------------------------------------------------
  chart_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 9),
          plot.title  = element_blank())

  output$chart_top20 <- renderPlot({
    req(input$sel_ind)
    L <- Ltxt()
    col <- ind_col_df(); nome <- ind_nome(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    worst <- isTRUE(input$show_worst)
    df_c  <- df[!is.na(df[[col]]), ]
    df_o  <- df_c[order(df_c[[col]], decreasing = !worst), ]
    df_p  <- df_o[seq_len(min(20, nrow(df_o))), ]
    df_p$lbl <- fct_reorder(paste0(df_p$nm_fcu, " (", df_p$nm_mun, ")"), df_p[[col]])
    pal_opt <- if (worst) "B" else "D"
    cap_txt <- if (worst) L$cap_worst else L$cap_top
    ggplot(df_p, aes(x = .data[[col]], y = lbl, fill = .data[[col]])) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(.data[[col]], 2)), hjust = -0.15, size = 3) +
      scale_fill_viridis_c(option = pal_opt) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = nome, y = NULL, caption = cap_txt) +
      chart_theme
  })

  output$chart_hist <- renderPlot({
    req(input$sel_ind)
    L <- Ltxt()
    col <- ind_col_df(); nome <- ind_nome(); df <- dados_filtrados()
    if (!col %in% names(df)) return(NULL)
    med <- median(df[[col]], na.rm = TRUE)
    ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = 10, fill = "#611ce3", colour = "white", linewidth = 0.3) +
      geom_vline(xintercept = med, colour = "#f76338", linewidth = 1, linetype = "dashed") +
      annotate("text", x = med, y = Inf, vjust = 2, hjust = -0.1,
               label = paste0(L$median_lbl, round(med, 2)),
               colour = "#f76338", size = 3.5) +
      labs(x = nome, y = L$hist_y) + theme_minimal(base_size = 12)
  })

  # -----------------------------------------------------------------------
  # Tab: Dados
  # -----------------------------------------------------------------------
  output$dados_titulo <- renderText({
    sprintf(Ltxt()$data_title_fmt, nrow(dados_filtrados()))
  })

  # Internal (untranslated) export table — renamed per language on display/export
  tabela_export_raw <- reactive({
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
                      "CMAET60", "CMAST60", "CMATT60", "CMACT60", "tem_risco")))
  })

  tabela_export <- reactive({ apply_pretty(tabela_export_raw(), lang()) })

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
    req(is_desktop())
    L    <- Ltxt()
    info <- download_info()
    fmt_size <- function(b) {
      if (b <= 0)          "—"
      else if (b < 1024)   paste0(round(b), " B")
      else if (b < 1024^2) paste0(round(b / 1024), " KB")
      else                 paste0(round(b / 1024^2, 1), " MB")
    }
    uf <- input$sel_uf
    escopo <- if (length(uf) > 0 && !all(uf == "")) {
      paste0(L$scope_uf, paste(uf, collapse = ", "))
    } else {
      L$scope_all
    }
    n_fmt <- formatC(info$n, format = "d", big.mark = ".")
    div(
      class = "alert alert-info",
      style = "padding:8px 12px;margin-bottom:12px;font-size:0.9rem;",
      tags$i(class = "fa fa-circle-info", style = "margin-right:6px;"),
      HTML(sprintf(L$dl_note_fmt, n_fmt, escopo, fmt_size(info$bytes)))
    )
  })

  # Percentage columns (pretty names, per language) for 1-digit rounding
  pct_internal <- c("perc_agua_adequada", "perc_esgoto_adequado", "perc_lixo_coleta",
                    "perc_analfabeto_populacao", "perc_via_pavimentada", "perc_bueiro",
                    "perc_iluminacao_publica", "perc_ponto_onibus", "perc_via_bicicleta",
                    "perc_calcada", "perc_obstaculo_calcada", "perc_rampa_cadeirante",
                    "pct_pretos_pardos", "pct_indigenas",
                    "pct_under5", "pct_under19", "pct_under30", "pct_idoso", "pct_chefe_mulher")

  output$tabela_dados <- renderDT({
    req(is_desktop())
    lg <- lang()
    m  <- col_pretty[[lg]]
    df <- tabela_export()
    datatable(df, rownames = FALSE, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "frtip",
        # Avisa o servidor quando o DataTables termina de desenhar (ponto B)
        drawCallback = htmlwidgets::JS(
          "function(settings) { Shiny.setInputValue('table_drawn', new Date().getTime()); }"
        ))) %>%
      formatRound(columns = intersect(
        unname(m[c("IDS", "IDA", "renda_sm_pond")]), names(df)), digits = 3) %>%
      formatRound(columns = intersect(unname(m[pct_internal]), names(df)), digits = 1) %>%
      formatCurrency(columns = intersect(
        unname(m[c("total_pessoas", "total_dp_ocupados")]), names(df)),
        currency = "", interval = 3, mark = ".", digits = 0)
  })

  # Mobile compact data table — no download, focused on conveying the data
  output$tabela_mobile <- renderDT({
    req(identical(mode(), "mobile"))
    df <- dados_filtrados() %>%
      select(nm_fcu, nm_mun, nm_uf, total_pessoas, IDS, IDA) %>%
      rename(
        "Nome da favela" = nm_fcu,
        "Município"      = nm_mun,
        "UF"             = nm_uf,
        "População"      = total_pessoas,
        "IDS"            = IDS,
        "IDA"            = IDA
      )
    datatable(df, rownames = FALSE, filter = "top",
      options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")) %>%
      formatRound(columns = c("IDS", "IDA"), digits = 3) %>%
      formatCurrency(columns = "População",
                     currency = "", interval = 3, mark = ".", digits = 0)
  })

  # -----------------------------------------------------------------------
  # Ficha da favela (mobile)
  # -----------------------------------------------------------------------
  output$fav_profile <- renderUI({
    req(identical(mode(), "mobile"))
    fcu <- input$search_fav
    if (is.null(fcu) || fcu == "") {
      return(div(
        style = "text-align:center;padding:48px 16px;background:#f8f9fa;border-radius:8px;color:#6c757d;",
        tags$i(class = "fa fa-magnifying-glass fa-2x", style = "margin-bottom:12px;color:#adb5bd;"),
        tags$h6("Busque uma favela para ver a ficha completa", style = "font-weight:500;"),
        tags$p("Todos os indicadores de saneamento, renda, acessibilidade, riscos e demografia em um só lugar.",
               style = "font-size:0.85rem;")
      ))
    }
    fav_profile_ui(fcu)
  })

  # -----------------------------------------------------------------------
  # Estado de carregamento do botão de download (ponto B):
  # desabilita assim que o filtro muda (tabela vai ser recalculada) e
  # reabilita somente quando o DataTables efetivamente desenha no cliente.
  # O botão já nasce desabilitado (classe .disabled na UI), então só fica
  # ativo após o primeiro desenho da tabela. (Somente versões desktop.)
  # -----------------------------------------------------------------------
  observeEvent(input$sel_uf, {
    req(is_desktop())
    session$sendCustomMessage("toggle_download", list(enabled = FALSE))
  }, ignoreInit = TRUE)

  observeEvent(input$table_drawn, {
    req(is_desktop())
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

shinyApp(ui, server)
