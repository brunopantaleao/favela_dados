# Favela Dados

**A public repository of quantitative data on Brazil's favelas and urban communities — in an interactive, easy-to-use map, no coding required.**

Favela Dados is a Shiny/Leaflet web application that maps **12,348 Favelas e Comunidades Urbanas (FCUs)** across the entire Brazilian territory, integrating 2022 Census data, geological hazard mapping, and access-to-opportunities metrics at the favela level. The goal is to turn public and private data into accessible knowledge about Brazilian favelas — usable by researchers, public managers, journalists, social movements, and residents who cannot (and should not need to) program or handle large datasets.

🔗 **Live app:** https://connect.posit.cloud/brunopanta/content/019f8490-1ad8-fdc3-5bb3-359255ee6fb8

---

## The three versions

On the welcome page, the user chooses one of three versions:

| Version | Description |
| --- | --- |
| **Versão Completa** (Desktop, PT) | Interactive choropleth map, per-favela popups, indicator charts, full data table, and CSV download. |
| **Versão Mobile** (PT) | Lightweight, no map / no popup / no download — focused on conveying the data through a per-favela profile card, charts, and a compact table. |
| **English Version** | The full application translated using IBGE / Census terminology. |

All three run from a single `app.R` and share one data-loading pipeline. Language and mode are held in server state; the UI is rendered dynamically based on the user's choice.

---

## Indicators & data sources

Data are joined at the census-tract level and aggregated to the FCU level (population-weighted means).

**Composite indices**
- **IDS — Social Development Index:** mean of 6 min-max normalized components (water, sewage, garbage, bathrooms per resident, literacy, income).
- **IDA — Urban Accessibility Index:** mean of 8 normalized components (paved street, storm drain, public lighting, bus stop, bicycle lane, sidewalk, obstruction [inverted], ramp).

**Indicator groups**
- **Sanitation** — piped water, sewage via general network, garbage collection
- **Income & Education** — mean income (minimum wages), illiteracy rate (15+)
- **Urban Accessibility** — paved street, storm drain, lighting, bus stop, bicycle lane, sidewalk, obstruction, wheelchair ramp
- **Geological Hazards** — exposure flag and hazard class, with per-hazard tract counts
- **Demographics (2022 Census)** — Black & Brown %, Indigenous %, age structure (under 5 / 19 / 30, 60+), female-headed households

**Sources**
- **IBGE, 2022 Population Census** — census-tract aggregates (via IBGE's public FTP) and FCU polygons
- **Geological Survey of Brazil (SGB/CPRM)** — natural hazard layers
- **IPEA — Access to Opportunities Project (aopdata, 2019)** — schools, health facilities, jobs and CRAS reachable within 60 min by public transit (available for the 20 most populous municipalities)

---

## Repository structure

```
favela_dados/
├── app.R                              # the application (must be at repo root)
├── manifest.json                      # Posit Connect Cloud deployment manifest
├── favelas_br_simplified.geojson      # FCU polygons (simplified for the map)
├── favelas_ids_ida.csv                # main indicator table (IDS, IDA, sanitation, income, accessibility)
├── favelas_riscos.csv                 # geological hazard indicators
├── favelas_acesso_oportunidades.csv   # IPEA access-to-opportunities (AOP) metrics
├── favelas_demograficos_2022.csv      # demographic indicators from the 2022 Census
├── favelas-logo-2 (5).png             # project logo
└── README.md
```

> The app loads its data files directly from this repository via raw GitHub URLs, so `app.R` is self-contained and needs no local data to run.

---

## Running locally

Requires **R (≥ 4.5)**. Install the packages and run:

```r
install.packages(c(
  "shiny", "bslib", "sf", "dplyr", "readr", "leaflet",
  "DT", "ggplot2", "forcats", "viridisLite", "scales", "tidyr"
))

shiny::runApp("app.R")
```

---

## Data pipeline

The datasets consumed by the app are produced by a set of sequentially numbered R scripts. Each source dataset is joined to the FCU crosswalk (`tabela_link.rds`) at the census-tract level, then aggregated to the FCU level, with intermediate `.rds`/`.csv` outputs cached for reuse.

- **Hazard layers** (`06a`–`06d`): process five hazard layers (`enxurrada`, `inundacao`, `perigo`, `risco`, `corrida_de_massa`) → `setores_hazards_favela.csv` → `favelas_riscos.csv`.
- **Demographics** (`08_build_demographics_ftp.R`): extract demographic variables from the IBGE 2022 Census FTP (sector-level aggregates), join to the FCU crosswalk → `favelas_demograficos_2022.csv`.

> Note: IBGE FTP column names differ from the documentation conventions, and suppressed census cells are coded `"X"` and must be converted to `NA` explicitly.

---

## Deployment

The app is deployed to **Posit Connect Cloud** from this GitHub repository.

- `app.R` **must be named exactly `app.R` and sit at the repository root** — Connect looks for `app.R` (or `server.R`) in the deployed directory.
- Dependencies are declared in `manifest.json`. To regenerate it after changing packages:

  ```r
  rsconnect::writeManifest()
  ```

- Push to the branch Connect is configured to track, then **Republish** (or rely on automatic redeploy). If a rebuild fails, Connect keeps serving the last successful build — check the **Logs** panel for the actual error.

---

## How to cite

> Pantaleão, B. and Montini, I. (2026). *Presenting the Favelas Dados dataset, a bird's-eye view of Brazilian favelas and urban communities.* https://doi.org/10.31235/osf.io/97yb5_v1

---

## Team & acknowledgements

Favela Dados is a research project coordinated by **PI Rodrigo Bonciani (UNIFESP)**, with associated researchers  **Bruno Pantaleão**(FGV-Analytics) **Pierre Moutier**, **Isabella Montini (UC Berkeley)**, and **Emily Souza** (Favelas BR). The application and dataset were organized by **Bruno Pantaleão** and **Isabella Montini**.

The project is developed by Favelas BR. https://www.favelas.org.br/

---

## License

No license file is included yet. Until a `LICENSE` is added, all rights are reserved by the authors. Consider adding an open data / open source license appropriate to the project's goals.
