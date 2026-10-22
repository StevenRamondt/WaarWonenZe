gemeenten <- data.frame(
  GM_CODE = c("GM0518", "GM0603", "GM0373", "GM0893"),
  GM_NAAM = c("'s-Gravenhage", "Rijswijk (ZH.)", "Bergen (NH.)", "Bergen (L.)")
)

test_that("postcodes worden genormaliseerd", {
  expect_equal(
    normaliseer_postcode(c("2612PD", "2612 pd", " 2612pd ", "2612", NA, "0612PD")),
    c("2612PD", "2612PD", "2612PD", NA, NA, NA)
  )
})

test_that("gemeentenamen koppelen via naam, alias en zonder achtervoegsel", {
  expect_equal(
    koppel_gemeente_code(
      c("'s-Gravenhage", "Den Haag", "Rijswijk (ZH.)", "Rijswijk", "rijswijk ", "Onbekend"),
      gemeenten
    ),
    c("GM0518", "GM0518", "GM0603", "GM0603", "GM0603", NA)
  )
})

test_that("dubbelzinnige namen worden niet geraden", {
  expect_equal(koppel_gemeente_code(c("Bergen", "Bergen (L.)"), gemeenten), c(NA, "GM0893"))
})

test_that("wijkkoppeling werkt met vuile postcodes", {
  pc6 <- data.frame(PC6 = c("2612PD", "2611AA"), WK_CODE = c("WK050301", "WK050302"))
  df <- data.frame(Postcode = c("2612 pd", "2611AA", "9999ZZ", NA))
  gekoppeld <- koppel_studenten(df, "wijk", pc6_mapping = pc6)

  expect_equal(gekoppeld$code, c("WK050301", "WK050302", NA, NA))
})

test_that("niet herkende postcodes krijgen een reden", {
  pc6 <- data.frame(PC6 = "2612PD", WK_CODE = "WK050301")
  df <- data.frame(Postcode = c("2612 pd", "2526", "9999ZZ", "abc", NA, " "))
  gekoppeld <- koppel_studenten(df, "wijk", pc6_mapping = pc6)

  expect_equal(
    gekoppeld$reden,
    c(NA, "alleen 4 cijfers", "postcode niet gevonden", "ongeldige postcode", "leeg", "leeg")
  )
})

test_that("koppelrapport telt kaart, buiten gebied en niet herkend", {
  gekoppeld <- data.frame(
    bron = c("a", "a", "b", "c", NA),
    code = c("GM1", "GM1", "GM2", NA, NA),
    reden = c(NA, NA, NA, "gemeente onbekend", "leeg")
  )
  rapport <- maak_koppelrapport(gekoppeld, geo_codes = "GM1", n_voor_filter = 7)

  expect_equal(rapport$op_kaart, 2)
  expect_equal(rapport$buiten_gebied, 1)
  expect_equal(rapport$niet_herkend, 2)
  expect_equal(rapport$voor_filter, 7)
  expect_setequal(rapport$niet_herkend_waarden$waarde, c("c", "(leeg)"))
})
