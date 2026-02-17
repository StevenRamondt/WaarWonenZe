# Waar wonen ze?

Choropleth kaarten voor visualisatie van studentenherkomst per gemeente of wijk. Gebouwd voor Nederlandse onderwijsinstellingen (mbo, hbo, wo).

## Vereisten

### R packages

```r
install.packages(c("shiny", "sf", "ggplot2", "dplyr", "readxl", "yaml", "gt", "RColorBrewer", "purrr", "here"))
```

### Bestanden

| Bestand | Locatie | Bron |
|---------|---------|------|
| `WijkBuurtkaart_2025_v0.gpkg` | `data/01_raw/` | [CBS Wijk- en Buurtkaart](https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/wijk-en-buurtkaart-2025) |
| `Gemeenten alfabetisch 2023.xlsx` | `data/reference/` | [CBS](https://www.cbs.nl/nl-nl/maatwerk/2023/03/gemeenten-alfabetisch-2023) |
| `pc6hnr20250801_gwb.csv` | `data/reference/` | [CBS PC6 koppeltabel](https://www.cbs.nl/nl-nl/maatwerk/2024/35/buurt-wijk-en-gemeente-2024-voor-postcode-huisnummer) |

### Studentendata

Upload via de app als `.xlsx`. Minimale kolommen:

| Kolom | Gebruikt voor | Voorbeeld |
|-------|---------------|-----------|
| `Gemeente` | Gemeente niveau | `'s-Gravenhage` |
| `Postcode` | Wijk niveau (via CBS koppeltabel) | `2612PD` |

## Gebruik

### Shiny app

```r
shiny::runApp()
```

Upload studentendata, kies een profiel en klik op **Genereer kaart & tabel**.

### Script (main.R)

```r
PROFIEL <- "haaglanden"
source("main.R")
```

Output verschijnt in `output/plot/` en `output/tabel/`.

## Profielen

| Profiel | Gebied | Niveau |
|---------|--------|--------|
| `den_haag` | Den Haag | wijk |
| `haaglanden` | Haaglanden (9 gemeenten) | gemeente |
| `zuid_holland` | Zuid-Holland | gemeente |
| `nederland` | Nederland | gemeente |

Custom profielen toevoegen via `config.yml`.

## Licentie

MIT
