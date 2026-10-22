# =============================================================================
# main.R
# Hoofdscript voor het genereren van choropleth kaarten (zonder app)
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
if (!exists("PROFIEL")) PROFIEL <- "haaglanden"

# Laad configuratie
config <- laad_config(PROFIEL)

# Print actieve configuratie
message("=== Actieve configuratie ===")
message(paste("Profiel:", PROFIEL))
message(paste("Gebied:", config$gebied$naam))
message(paste("Niveau:", config$niveau))
message(paste("Titel:", config$titel))
message("============================")

ontbrekend <- ontbrekende_referentiedata(config)
if (length(ontbrekend) > 0) {
  stop("Referentiedata ontbreekt: ", paste(ontbrekend, collapse = ", "),
       ". Run eerst: Rscript scripts/prepare_data.R")
}

# -----------------------------------------------------------------------------
# DATA LADEN
# -----------------------------------------------------------------------------

studenten_pad <- here::here(config$data$studenten)
if (!file.exists(studenten_pad)) {
  message("Geen ", config$data$studenten, " gevonden: demodata wordt gebruikt.")
  studenten_pad <- here::here(config$data$demo)
}

studenten <- laad_studenten_data(studenten_pad)
message(paste("Studenten data geladen:", nrow(studenten), "records"))

# -----------------------------------------------------------------------------
# KAART EN TABEL MAKEN
# -----------------------------------------------------------------------------

resultaat <- genereer_kaart(
  studenten = studenten,
  config = config,
  voortgang = function(fractie, detail) message(detail)
)

message(beschrijf_koppelrapport(resultaat$rapport))
message(paste("Categorieën:", paste(resultaat$breaks_info$labels, collapse = ", ")))

# -----------------------------------------------------------------------------
# OPSLAAN
# -----------------------------------------------------------------------------

dir.create(here::here("output", "plot"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output", "tabel"), recursive = TRUE, showWarnings = FALSE)

plot_path <- here::here("output", "plot",
                        genereer_bestandsnaam(config, config$niveau, "plot"))
sla_plot_op(resultaat$plot, plot_path)

tabel_path <- here::here("output", "tabel",
                         genereer_bestandsnaam(config, config$niveau, "tabel"))
tryCatch(
  sla_tabel_op(resultaat$tabel, tabel_path),
  error = function(e) {
    tabel_path <<- sub("\\.png$", ".csv", tabel_path)
    utils::write.csv(resultaat$tabel_data, tabel_path, row.names = FALSE)
    message("PNG van tabel mislukt (webshot2/Chrome nodig); CSV opgeslagen: ", tabel_path)
  }
)

# -----------------------------------------------------------------------------
# KLAAR
# -----------------------------------------------------------------------------

message("\n=== Klaar! ===")
message(paste("Plot:", plot_path))
message(paste("Tabel:", tabel_path))
