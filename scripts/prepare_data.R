# =============================================================================
# prepare_data.R
# Download CBS-referentiedata voor een jaargang en maak compacte bestanden
#
# Gebruik (vanuit de projectmap):
#   pixi run prepare-data 2026
#   Rscript scripts/prepare_data.R 2026 ~/cbs   # met eigen downloadmap
#
# Dit kan ook vanuit de app: kies een jaargang die nog niet aanwezig is.
# Output in data/reference/: gemeenten_<jaar>.rds, wijken_<jaar>.rds, pc6_<jaar>.rds
# Bron: CBS (CC BY 4.0). Zie maak_referentiedata() in R/functions.R.
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

source(here::here("R", "functions.R"))

args <- commandArgs(trailingOnly = TRUE)
jaar <- if (length(args) >= 1) as.integer(args[1]) else as.integer(format(Sys.Date(), "%Y"))
download_dir <- if (length(args) >= 2) path.expand(args[2]) else file.path(tempdir(), "cbs")

paden <- maak_referentiedata(jaar, laad_config("default"), download_dir)

for (pad in paden) {
  message(sprintf("  %s: %.1f MB", basename(pad), file.size(pad) / 1e6))
}
message("Klaar.")
