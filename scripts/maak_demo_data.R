# =============================================================================
# maak_demo_data.R
# Maak synthetische demodata op basis van een echte inschrijvingsexport
#
# Gebruik (vanuit de projectmap):
#   Rscript scripts/maak_demo_data.R pad/naar/inschrijvingen.xlsx   # of .csv (;)
#
# Output (wordt meegeleverd in de repo):
#   data/voorbeeld/studenten_demo.xlsx   fictieve studenten, cube-structuur
#   data/voorbeeld/template.xlsx         minimale template voor eigen data
#
# Privacy: er wordt GEEN enkele rij uit de bron overgenomen. Het script
# gebruikt alleen geaggregeerde aantallen en verbergt kleine groepen:
#   - Woonplek: PC4-gebieden met >= K studenten worden gewogen naar aantal,
#     daarbinnen een willekeurige bestaande PC6 (CBS). Studenten in kleinere
#     PC4's worden op gemeenteniveau verdeeld (gemeente >= K) of anders
#     willekeurig over Nederland.
#   - Gemeente wordt afgeleid uit de gekozen PC6, niet uit de bron.
#   - School/niveau/leerweg: alleen combinaties met >= K_ORG studenten.
#   - Geslacht en leeftijdscategorie: onafhankelijk getrokken (marginalen).
#   - Geen studentnummers, namen of geboortedata.
# =============================================================================

library(dplyr)

source(here::here("R", "functions.R"))

N_DEMO <- 15000
K <- 5        # minimale groepsgrootte voor woonplek
K_ORG <- 10   # minimale groepsgrootte voor school/niveau/leerweg
set.seed(20251001)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Geef het pad naar de inschrijvingsexport (.xlsx of .csv) op.")
bron_pad <- args[1]

bron <- if (grepl("\\.csv$", bron_pad, ignore.case = TRUE)) {
  df <- utils::read.csv2(bron_pad, colClasses = "character", check.names = FALSE)
  colnames(df) <- gsub("\\[|\\]", "", colnames(df))
  df
} else {
  laad_studenten_data(bron_pad)
}

gemeenten <- sf::st_drop_geometry(readRDS(here::here("data", "reference", "gemeenten_2025.rds")))
pc6_ref <- readRDS(here::here("data", "reference", "pc6_2025.rds")) |>
  dplyr::mutate(PC4 = substr(PC6, 1, 4))


# -----------------------------------------------------------------------------
# Woonplek: geaggregeerde aantallen per PC4 en gemeente
# -----------------------------------------------------------------------------

bron <- bron |>
  dplyr::mutate(
    PC6 = normaliseer_postcode(Postcode),
    PC4 = ifelse(is.na(PC6), NA, substr(PC6, 1, 4))
  ) |>
  dplyr::left_join(dplyr::select(gemeenten, GM_NAAM, GM_CODE),
                   by = c("Gemeente" = "GM_NAAM"))

pc4_telling <- bron |>
  dplyr::filter(!is.na(PC4), PC4 %in% pc6_ref$PC4) |>
  dplyr::count(PC4)
pc4_groot <- dplyr::filter(pc4_telling, n >= K)

rest <- bron |> dplyr::filter(!PC4 %in% pc4_groot$PC4 | is.na(PC4))
gm_telling <- rest |> dplyr::filter(!is.na(GM_CODE)) |> dplyr::count(GM_CODE)
gm_groot <- dplyr::filter(gm_telling, n >= K)

n_a <- sum(pc4_groot$n)
n_b <- sum(gm_groot$n)
n_c <- nrow(bron) - n_a - n_b
aandeel <- c(a = n_a, b = n_b, c = n_c) / nrow(bron)
n_per_laag <- round(aandeel * N_DEMO)
n_per_laag[["a"]] <- N_DEMO - n_per_laag[["b"]] - n_per_laag[["c"]]

message(sprintf("Lagen: PC4 %.1f%%, gemeente %.1f%%, verspreid %.1f%%",
                100 * aandeel[["a"]], 100 * aandeel[["b"]], 100 * aandeel[["c"]]))

trek_pc6 <- function(groepen, kolom) {
  pc6_per_groep <- split(pc6_ref$PC6, pc6_ref[[kolom]])
  vapply(groepen, function(g) sample(pc6_per_groep[[g]], 1), character(1))
}

pc6_demo <- c(
  trek_pc6(sample(pc4_groot$PC4, n_per_laag[["a"]], replace = TRUE, prob = pc4_groot$n), "PC4"),
  trek_pc6(sample(gm_groot$GM_CODE, n_per_laag[["b"]], replace = TRUE, prob = gm_groot$n), "GM_CODE"),
  trek_pc6(sample(gemeenten$GM_CODE, n_per_laag[["c"]], replace = TRUE), "GM_CODE")
)
pc6_demo <- sample(pc6_demo)

gemeente_demo <- pc6_ref$GM_CODE[match(pc6_demo, pc6_ref$PC6)] |>
  (\(x) gemeenten$GM_NAAM[match(x, gemeenten$GM_CODE)])()


# -----------------------------------------------------------------------------
# Overige kenmerken
# -----------------------------------------------------------------------------

org_kolommen <- c("ScholenGroep", "SchoolNaamKort", "CampusOms", "Niveau", "Leerweg")
org <- bron |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(org_kolommen), ~ !is.na(.x) & .x != "")) |>
  dplyr::count(dplyr::across(dplyr::all_of(org_kolommen))) |>
  dplyr::filter(n >= K_ORG)
org_demo <- org[sample(nrow(org), N_DEMO, replace = TRUE, prob = org$n), org_kolommen]

trek_marginaal <- function(x) {
  x <- x[!is.na(x) & x != ""]
  freq <- table(x)
  sample(names(freq), N_DEMO, replace = TRUE, prob = as.numeric(freq))
}

demo <- data.frame(
  StudentNr = sprintf("DEMO%05d", seq_len(N_DEMO)),
  Peildatum = as.Date("2025-10-01"),
  IsHoofdinschrijving = TRUE,
  IsBekostigd = stats::runif(N_DEMO) > 0.06,
  org_demo,
  Postcode = pc6_demo,
  Gemeente = gemeente_demo,
  Geslacht = trek_marginaal(bron$Geslacht),
  LeeftijdCat = trek_marginaal(bron$LeeftijdCat),
  row.names = NULL
)


# -----------------------------------------------------------------------------
# Realistische vervuiling (om validatie in de app te kunnen tonen)
# -----------------------------------------------------------------------------

kies <- function(aandeel) which(stats::runif(N_DEMO) < aandeel)

i <- kies(0.03); demo$Postcode[i] <- sub("^(\\d{4})", "\\1 ", demo$Postcode[i])
i <- kies(0.01); demo$Postcode[i] <- tolower(demo$Postcode[i])
i <- kies(0.002); demo$Postcode[i] <- substr(demo$Postcode[i], 1, 4)
i <- kies(0.001); demo$Postcode[i] <- NA
i <- kies(0.004); demo$Postcode[i] <- NA; demo$Gemeente[i] <- "Onbekend"

# Kolomnamen zoals in de cube-export: [Kolom]
colnames(demo) <- paste0("[", colnames(demo), "]")


# -----------------------------------------------------------------------------
# Opslaan
# -----------------------------------------------------------------------------

out_dir <- here::here("data", "voorbeeld")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

writexl::write_xlsx(demo, file.path(out_dir, "studenten_demo.xlsx"))
writexl::write_xlsx(
  data.frame(
    Postcode = c("2612PD", "2711AB"),
    Gemeente = c("Delft", "Zoetermeer")
  ),
  file.path(out_dir, "template.xlsx")
)

message("Demodata: ", N_DEMO, " fictieve studenten -> ", file.path(out_dir, "studenten_demo.xlsx"))
