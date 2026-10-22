schrijf_csv <- function(regels, encoding = "UTF-8") {
  pad <- tempfile(fileext = ".csv")
  con <- file(pad, open = "w", encoding = encoding)
  writeLines(regels, con)
  close(con)
  pad
}

test_that("CSV uit Nederlandse Excel: puntkomma, WAAR/ONWAAR, Windows-1252", {
  pad <- schrijf_csv(c(
    "[Postcode];[Gemeente];[IsBekostigd]",
    "2612PD;Súdwest-Fryslân;WAAR",
    "2711AB;Zoetermeer;ONWAAR"
  ), encoding = "windows-1252")
  df <- laad_studenten_data(pad)

  expect_equal(names(df), c("Postcode", "Gemeente", "IsBekostigd"))
  expect_equal(df$Gemeente[1], "Súdwest-Fryslân")
  expect_equal(df$IsBekostigd, c(TRUE, FALSE))
  expect_equal(nrow(filter_studenten(df, list(IsBekostigd = TRUE))), 1)
})

test_that("CSV met komma en UTF-8; postcodes blijven tekst", {
  pad <- schrijf_csv(c("Postcode,Gemeente,Aantal", "2612 PD,Delft,1", "1011AB,Amsterdam,2"))
  df <- laad_studenten_data(pad)

  expect_type(df$Postcode, "character")
  expect_equal(df$Postcode[1], "2612 PD")
  expect_equal(normaliseer_postcode(df$Postcode), c("2612PD", "1011AB"))
})

test_that("bestandstype volgt de oorspronkelijke naam (Shiny-upload)", {
  pad <- schrijf_csv(c("Gemeente", "Delft"))
  tijdelijk <- sub("\\.csv$", "", pad)
  file.rename(pad, tijdelijk)
  expect_equal(laad_studenten_data(tijdelijk, naam = "export.csv")$Gemeente, "Delft")
})

test_that("peildatum uit data en als tekst", {
  expect_equal(peildatum_uit_data(data.frame(Peildatum = as.POSIXct("2026-10-01", tz = "UTC"))),
               as.Date("2026-10-01"))
  expect_equal(peildatum_uit_data(data.frame(Peildatum = "01-10-2026")), as.Date("2026-10-01"))
  expect_null(peildatum_uit_data(data.frame(Peildatum = c("2025-10-01", "2026-10-01"))))
  expect_null(peildatum_uit_data(data.frame(Gemeente = "Delft")))
  expect_equal(formatteer_peildatum(as.Date("2026-10-01")), "1 okt 2026")
})

test_that("jaargang: config, dan peildatum, dan nieuwste", {
  config <- laad_config("default")
  beschikbaar <- beschikbare_jaargangen(config)
  expect_true(all(c(2025, 2026) %in% beschikbaar))

  expect_equal(kies_jaargang(config), max(beschikbaar))
  expect_equal(kies_jaargang(config, as.Date("2025-10-01")), 2025)
  expect_equal(kies_jaargang(config, as.Date("1999-10-01")), max(beschikbaar))
  config$jaar <- 2025
  expect_equal(kies_jaargang(config, as.Date("2026-10-01")), 2025)
})

test_that("GM-codes in de profielen bestaan in elke jaargang", {
  for (jaar in beschikbare_jaargangen(laad_config("default"))) {
    for (profiel in profiel_keuzes()) {
      config <- laad_config(profiel)
      config$jaar <- jaar
      codes <- config$gebied$filter$gm_codes
      if (is.null(codes)) next
      geo <- laad_geo_data(config, "gemeente")
      expect_true(all(codes %in% geo$GM_CODE), info = paste(profiel, jaar))
    }
  }
})

test_that("schoollocaties: aan/uit, filter op campus, alleen binnen het gebied", {
  config <- laad_config("den_haag")
  config$jaar <- 2026
  geo <- filter_geo_data(laad_geo_data(config, "wijk"), config)

  locaties <- maak_locaties_sf(config, geo)
  expect_gt(nrow(locaties), 5)
  expect_false("Leiden" %in% locaties$plaats)      # Vondellaan 35 ligt buiten Den Haag

  config$filters <- list(CampusOms = "Tinwerf")
  expect_setequal(maak_locaties_sf(config, geo)$code, c("T24", "T16", "MEP"))

  config$toon_locaties <- FALSE
  expect_null(maak_locaties_sf(config, geo))
})

test_that("CBS-downloads voor 2026 zijn te vinden", {
  testthat::skip_if_offline("www.cbs.nl")
  urls <- zoek_cbs_urls(2026)
  expect_match(urls$kaart, "WijkBuurtkaart_2026_v\\d+\\.zip$")
  expect_match(urls$pc6, "^https://download\\.cbs\\.nl/postcode/.*\\.zip$")
  expect_error(zoek_cbs_urls(2099), "nog niet gepubliceerd")
})
