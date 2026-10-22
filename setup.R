# =============================================================================
# setup.R
# Eenmalig: installeer ontbrekende packages en controleer de referentiedata
#
# Gebruik: open dit bestand in RStudio en klik op "Source",
#          of run in de terminal: Rscript setup.R
# =============================================================================

PACKAGES <- c(
  # app en pipeline
  "shiny", "sf", "ggplot2", "dplyr", "readxl", "yaml", "gt",
  "RColorBrewer", "purrr", "here",
  # tabel als PNG (vereist daarnaast Chrome of Chromium)
  "webshot2",
  # ontwikkelen: tests en demodata
  "testthat", "writexl"
)

ontbrekend <- PACKAGES[!vapply(PACKAGES, requireNamespace, logical(1), quietly = TRUE)]

if (length(ontbrekend) > 0) {
  message("Installeren: ", paste(ontbrekend, collapse = ", "))
  install.packages(ontbrekend, repos = "https://cloud.r-project.org")
} else {
  message("Alle packages zijn al geïnstalleerd.")
}

source(here::here("R", "functions.R"))
missend <- ontbrekende_referentiedata(laad_config("default"))

if (length(missend) > 0) {
  message("Referentiedata ontbreekt (", paste(missend, collapse = ", "), "). ",
          "Downloaden en voorbereiden...")
  source(here::here("scripts", "prepare_data.R"))
} else {
  message("Referentiedata is aanwezig.")
}

message("\nKlaar! Start de app met: shiny::runApp()")
