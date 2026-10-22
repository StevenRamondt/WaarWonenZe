# =============================================================================
# prepare_data.R
# Download CBS-referentiedata en maak compacte bestanden voor de app
#
# Gebruik (vanuit de projectmap):
#   Rscript scripts/prepare_data.R            # download naar tijdelijke map
#   Rscript scripts/prepare_data.R ~/cbs      # gebruik/vul een eigen downloadmap
#
# Output in data/reference/ (wordt meegeleverd in de repo):
#   gemeenten_<jaar>.rds   sf: GM_CODE, GM_NAAM, PROVINCIE, geometry
#   wijken_<jaar>.rds      sf: WK_CODE, WK_NAAM, GM_CODE, GM_NAAM, PROVINCIE, geometry
#   pc6_<jaar>.rds         data.frame: PC6, WK_CODE, GM_CODE
#
# Bron: CBS (CC BY 4.0). Bij een nieuwe jaargang: pas JAAR en de URL's aan.
# =============================================================================

library(sf)
library(dplyr)
library(readxl)

JAAR <- 2025

URLS <- list(
  kaart     = "https://download.cbs.nl/regionale-kaarten/wijkbuurtkaart_2025_v0.zip",
  pc6       = "https://download.cbs.nl/postcode/2025-cbs-pc6huisnr20250801_buurt.zip",
  gemeenten = "https://www.cbs.nl/-/media/cbs/onze-diensten/methoden/classificaties/overig/gemeenten-alfabetisch-2025.xlsx"
)

# Vereenvoudiging in meters (RD): klein genoeg om op een kaart van een
# gemeente of regio geen zichtbare naden tussen buurgebieden te geven.
TOLERANTIE <- c(gemeente = 25, wijk = 10)

args <- commandArgs(trailingOnly = TRUE)
download_dir <- if (length(args) > 0) path.expand(args[1]) else file.path(tempdir(), "cbs")
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

out_dir <- here::here("data", "reference")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

options(timeout = 3600)


# -----------------------------------------------------------------------------
# Download (overslaan als bestand al bestaat)
# -----------------------------------------------------------------------------

download <- function(url) {
  bestand <- file.path(download_dir, basename(url))
  if (!file.exists(bestand)) {
    message("Downloaden: ", url)
    utils::download.file(url, bestand, mode = "wb", quiet = TRUE)
  }
  bestand
}

zoek_of_unzip <- function(zip, patroon) {
  gevonden <- list.files(download_dir, pattern = patroon, full.names = TRUE)
  if (length(gevonden) > 0) return(gevonden[1])
  utils::unzip(zip, exdir = download_dir)
  list.files(download_dir, pattern = patroon, full.names = TRUE)[1]
}

gpkg_path <- zoek_of_unzip(download(URLS$kaart), "\\.gpkg$")
pc6_path  <- zoek_of_unzip(download(URLS$pc6), "^pc6hnr.*\\.csv$")
gem_path  <- download(URLS$gemeenten)


# -----------------------------------------------------------------------------
# Gemeenten (met provincie)
# -----------------------------------------------------------------------------

message("Gemeenten verwerken...")

provincies <- readxl::read_excel(gem_path, sheet = "Gemeenten_alfabetisch") |>
  dplyr::select(GM_CODE = GemeentecodeGM, PROVINCIE = Provincienaam)

# water: NEE = land, JA = zee/meer-deel van een gemeente, B = buitenland
gemeenten <- sf::st_read(gpkg_path, layer = "gemeenten_v0", quiet = TRUE) |>
  dplyr::filter(water == "NEE") |>
  dplyr::select(GM_CODE = gm_code, GM_NAAM = gm_naam) |>
  dplyr::left_join(provincies, by = "GM_CODE") |>
  sf::st_simplify(preserveTopology = TRUE, dTolerance = TOLERANTIE[["gemeente"]])

sf::st_geometry(gemeenten) <- "geometry"
stopifnot(!anyDuplicated(gemeenten$GM_CODE), !anyNA(gemeenten$PROVINCIE))


# -----------------------------------------------------------------------------
# Wijken
# -----------------------------------------------------------------------------

message("Wijken verwerken...")

wijken <- sf::st_read(gpkg_path, layer = "wijken_v0", quiet = TRUE) |>
  dplyr::filter(water == "NEE") |>
  dplyr::select(WK_CODE = wk_code, WK_NAAM = wk_naam,
                GM_CODE = gm_code, GM_NAAM = gm_naam) |>
  dplyr::left_join(provincies, by = "GM_CODE") |>
  sf::st_simplify(preserveTopology = TRUE, dTolerance = TOLERANTIE[["wijk"]])

sf::st_geometry(wijken) <- "geometry"
stopifnot(!anyDuplicated(wijken$WK_CODE))


# -----------------------------------------------------------------------------
# PC6 -> wijk/gemeente
# -----------------------------------------------------------------------------

message("PC6-koppeltabel verwerken (8 mln regels, even geduld)...")

wijk_kolom <- paste0("Wijk", JAAR)
gem_kolom <- paste0("Gemeente", JAAR)

pc6_raw <- utils::read.csv2(
  pc6_path,
  colClasses = "character",
  col.names = c("PC6", "Huisnummer", "Buurt", "Wijk", "Gemeente"),
  header = TRUE
)

# Meerdere huisnummers per PC6: de meest voorkomende wijk wint
pc6 <- pc6_raw |>
  dplyr::count(PC6, Wijk, Gemeente) |>
  dplyr::arrange(PC6, dplyr::desc(n)) |>
  dplyr::distinct(PC6, .keep_all = TRUE) |>
  dplyr::transmute(
    PC6,
    WK_CODE = paste0("WK", Wijk),
    GM_CODE = paste0("GM", Gemeente)
  ) |>
  as.data.frame()

rm(pc6_raw)


# -----------------------------------------------------------------------------
# Opslaan
# -----------------------------------------------------------------------------

opslaan <- function(obj, naam) {
  pad <- file.path(out_dir, paste0(naam, "_", JAAR, ".rds"))
  saveRDS(obj, pad, compress = "xz")
  message(sprintf("  %s: %d rijen, %.1f MB", basename(pad), nrow(obj), file.size(pad) / 1e6))
}

message("Opslaan in ", out_dir)
opslaan(gemeenten, "gemeenten")
opslaan(wijken, "wijken")
opslaan(pc6, "pc6")
message("Klaar.")
