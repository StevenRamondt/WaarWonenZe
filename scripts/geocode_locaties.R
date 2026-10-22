# =============================================================================
# geocode_locaties.R
# Vul ontbrekende coördinaten aan in data/locaties_roc_mondriaan.csv
#
# Voeg een rij toe met code, adres, plaats en campus (lat/lon leeg laten) en
# run: Rscript scripts/geocode_locaties.R
# Coördinaten komen van de PDOK Locatieserver (BAG). Controleer het resultaat:
# bij dubbelzinnige adressen wint de beste match binnen de opgegeven plaats.
# =============================================================================

source(here::here("R", "functions.R"))

pad <- here::here(laad_config("default")$data$locaties)
locaties <- lees_csv(pad)

ontbreekt <- is.na(locaties$lat) | is.na(locaties$lon) | locaties$lat == "" | locaties$lon == ""

for (i in which(ontbreekt)) {
  q <- utils::URLencode(paste(locaties$adres[i], locaties$plaats[i]), reserved = TRUE)
  url <- paste0("https://api.pdok.nl/bzk/locatieserver/search/v3_1/free?fq=type:adres&rows=1",
                "&fl=weergavenaam,centroide_ll&q=", q)
  antwoord <- paste(readLines(url(url), warn = FALSE), collapse = "")
  punt <- regmatches(antwoord, regexpr("POINT\\([0-9.]+ [0-9.]+\\)", antwoord))
  naam <- regmatches(antwoord, regexpr("\"weergavenaam\":\"[^\"]+\"", antwoord))

  if (length(punt) == 0) {
    message("Niet gevonden: ", locaties$adres[i], ", ", locaties$plaats[i])
    next
  }
  coords <- as.numeric(strsplit(gsub("POINT\\(|\\)", "", punt), " ")[[1]])
  locaties$lon[i] <- coords[1]
  locaties$lat[i] <- coords[2]
  message("Gevonden: ", sub("\"weergavenaam\":", "", naam))
}

utils::write.table(locaties, pad, sep = ";", row.names = FALSE, quote = FALSE,
                   fileEncoding = "UTF-8")
message("Opgeslagen: ", pad)
