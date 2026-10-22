test_that("filter_studenten vergelijkt als tekst en slaat ontbrekende kolommen over", {
  df <- data.frame(IsBekostigd = c(TRUE, FALSE, TRUE), Leerweg = c("BOL", "BBL", "BBL"))

  res <- filter_studenten(df, list(IsBekostigd = TRUE, Leerweg = "BBL", Bestaat = "x"))
  expect_equal(nrow(res), 1)
  expect_equal(attr(res, "overgeslagen"), "Bestaat")
  expect_equal(attr(res, "weggefilterd"), c(IsBekostigd = 1L, Leerweg = 1L))

  expect_equal(nrow(filter_studenten(df, list(IsBekostigd = "TRUE"))), 2)
  expect_equal(nrow(filter_studenten(df, list(Leerweg = character(0)))), 3)
})

test_that("merge_config overschrijft genest", {
  parent <- list(a = 1, vis = list(kleur = "Blues", labels = FALSE))
  child <- list(inherits = "default", vis = list(kleur = "Reds"))

  expect_equal(
    merge_config(parent, child),
    list(a = 1, vis = list(kleur = "Reds", labels = FALSE))
  )
})

test_that("alle profielen in config.yml zijn geldig", {
  for (profiel in profiel_keuzes()) {
    config <- laad_config(profiel)
    expect_true(config$niveau %in% c("gemeente", "wijk"), info = profiel)
    expect_length(ontbrekende_referentiedata(config), 0)
  }
})

test_that("filter_geo_data op codes en provincie", {
  geo <- data.frame(GM_CODE = c("GM1", "GM2", "GM3"), PROVINCIE = c("A", "A", "B"))

  expect_equal(nrow(filter_geo_data(geo, list(gebied = list(filter = list(gm_codes = "GM3"))))), 1)
  expect_equal(nrow(filter_geo_data(geo, list(gebied = list(filter = list(provincie = "A"))))), 2)
  expect_equal(nrow(filter_geo_data(geo, list(gebied = list(filter = NULL)))), 3)
})

test_that("koppelrapport noemt de filters die studenten wegfilterden", {
  rapport <- list(op_kaart = 5, na_filter = 6, voor_filter = 10, buiten_gebied = 1,
                  niet_herkend = 0, weggefilterd_per_filter = c(IsBekostigd = 3L, Leerweg = 1L))
  expect_match(beschrijf_koppelrapport(rapport), "4 weggefilterd door filters IsBekostigd \\(3\\), Leerweg \\(1\\)")

  rapport$weggefilterd_per_filter <- c(IsBekostigd = 4L, Leerweg = 0L)
  expect_match(beschrijf_koppelrapport(rapport), "4 weggefilterd door filter IsBekostigd\\)")
})
