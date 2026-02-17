# =============================================================================
# main.R
# Hoofdscript voor het genereren van choropleth kaarten
# =============================================================================

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------

# Laad benodigde packages
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(yaml)
library(gt)
library(RColorBrewer)
library(purrr)
library(here)

# Laad functies
source(here::here("R", "functions.R"))

# -----------------------------------------------------------------------------
# CONFIGURATIE
# -----------------------------------------------------------------------------

# Kies profiel: "den_haag", "haaglanden", "zuid_holland", "nederland"
# Of een custom profiel uit config.yml
PROFIEL <- "haaglanden"

# Laad configuratie
config <- laad_config(PROFIEL)

# Print actieve configuratie
message("=== Actieve configuratie ===")
message(paste("Profiel:", PROFIEL))
message(paste("Gebied:", config$gebied$naam))
message(paste("Niveau:", config$niveau))
message(paste("Titel:", config$titel))
message("============================")

# -----------------------------------------------------------------------------
# DATA LADEN
# -----------------------------------------------------------------------------

message("\n[1/6] Data laden...")

# Laad geo data
geo <- laad_geo_data(
  gpkg_path = here::here(config$data$gpkg),
  niveau = config$niveau
)
message(paste("  - Geo data geladen:", nrow(geo), "records"))

# Laad studenten data
studenten <- laad_studenten_data(
  path = here::here(config$data$studenten)
)
message(paste("  - Studenten data geladen:", nrow(studenten), "records"))

# Laad gemeente referentie (voor provincie filter)
gemeente_ref <- laad_gemeente_ref(
  path = here::here(config$data$gemeenten_ref)
)
message(paste("  - Gemeente referentie geladen:", nrow(gemeente_ref), "gemeenten"))

# -----------------------------------------------------------------------------
# DATA FILTEREN
# -----------------------------------------------------------------------------

message("\n[2/6] Data filteren...")

# Filter geo data op basis van config
geo_filtered <- filter_geo_data(
  geo = geo,
  config = config,
  gemeente_ref = gemeente_ref
)
message(paste("  - Geo data gefilterd:", nrow(geo_filtered), "records"))

# Verwijder dubbele geometrieën (strand/zee)
code_kolom <- if (config$niveau == "gemeente") "GM_CODE" else "WK_CODE"
geo_filtered <- verwijder_dubbele_geometrie(geo_filtered, code_kolom)
message(paste("  - Dubbele geometrieën verwijderd:", nrow(geo_filtered), "records"))

# -----------------------------------------------------------------------------
# DATA AGGREGEREN
# -----------------------------------------------------------------------------

message("\n[3/6] Studenten aggregeren...")

# Laad PC6-wijk mapping indien wijk niveau
pc6_wijk_mapping <- NULL
if (config$niveau == "wijk") {
  message("  - PC6-wijk mapping laden...")
  pc6_wijk_mapping <- maak_postcode_wijk_mapping(
    path = here::here(config$data$pc6_wijk_ref)
  )
  message(paste("  - PC6-wijk mapping geladen:", nrow(pc6_wijk_mapping), "postcodes"))
}

# Aggregeer studenten per gebied
studenten_agg <- aggregeer_studenten(
  df = studenten,
  niveau = config$niveau,
  pc6_wijk_mapping = pc6_wijk_mapping
)
message(paste("  - Geaggregeerd naar", nrow(studenten_agg), "gebieden"))

# Merge met geo data
data <- merge_studenten_geo(
  geo = geo_filtered,
  studenten_agg = studenten_agg,
  niveau = config$niveau
)
message(paste("  - Totaal studenten in gebied:", sum(data$aantal, na.rm = TRUE)))

# -----------------------------------------------------------------------------
# CATEGORISEREN
# -----------------------------------------------------------------------------

message("\n[4/6] Aantallen categoriseren...")

# Bepaal breaks
breaks_info <- bepaal_breaks(
  data = data$aantal,
  preset = config$breaks_preset
)
message(paste("  - Breaks preset:", config$breaks_preset))
message(paste("  - Categorieën:", paste(breaks_info$labels, collapse = ", ")))

# Voeg categorieën toe
data <- categoriseer_aantallen(data, breaks_info)

# -----------------------------------------------------------------------------
# PLOT MAKEN
# -----------------------------------------------------------------------------

message("\n[5/6] Choropleth maken...")

# Maak locaties sf object (indien geconfigureerd)
locaties_sf <- maak_locaties_sf(config, sf::st_crs(data))

# Maak plot
plot <- maak_choropleth(
  data = data,
  config = config,
  breaks_info = breaks_info,
  locaties_sf = locaties_sf,
  niveau = config$niveau
)

# Genereer bestandsnaam en sla op
plot_bestand <- genereer_bestandsnaam(config, config$niveau, "plot")
plot_path <- here::here("output", "plot", plot_bestand)

# Maak output directory indien nodig
dir.create(here::here("output", "plot"), recursive = TRUE, showWarnings = FALSE)

sla_plot_op(plot, plot_path)

# -----------------------------------------------------------------------------
# TABEL MAKEN
# -----------------------------------------------------------------------------

message("\n[6/6] Tabel maken...")

# Maak tabel
tabel <- maak_tabel(
  data = data,
  config = config,
  niveau = config$niveau,
  min_aantal = 10
)

# Genereer bestandsnaam en sla op
tabel_bestand <- genereer_bestandsnaam(config, config$niveau, "tabel")
tabel_path <- here::here("output", "tabel", tabel_bestand)

# Maak output directory indien nodig
dir.create(here::here("output", "tabel"), recursive = TRUE, showWarnings = FALSE)

sla_tabel_op(tabel, tabel_path)

# -----------------------------------------------------------------------------
# KLAAR
# -----------------------------------------------------------------------------

message("\n=== Klaar! ===")
message(paste("Plot:", plot_path))
message(paste("Tabel:", tabel_path))
