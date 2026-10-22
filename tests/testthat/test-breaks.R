test_that("labels sluiten aan op de klassegrenzen", {
  expect_equal(
    maak_klasse_labels(c(1, 200, 500, Inf)),
    c("1-200", "201-500", "> 500")
  )
})

test_that("waarden op een grens vallen in de klasse met dat label", {
  breaks_info <- bepaal_breaks(c(1, 200, 201, 500), preset = "eigen", eigen = c(1, 200, 500))
  data <- data.frame(aantal = c(0, 1, 200, 201, 500))
  klasse <- as.character(categoriseer_aantallen(data, breaks_info)$aantal_cat)

  expect_equal(klasse, c("0", "1-200", "1-200", "201-500", "201-500"))
})

test_that("bovenklasse alleen als er grotere aantallen zijn", {
  expect_false(any(is.infinite(bepaal_breaks(c(5, 50), "eigen", c(1, 20, 100))$breaks)))

  info <- bepaal_breaks(c(5, 150), "eigen", c(1, 20, 100))
  expect_equal(utils::tail(info$labels, 1), "> 100")
  klasse <- categoriseer_aantallen(data.frame(aantal = 150), info)$aantal_cat
  expect_equal(as.character(klasse), "> 100")
})

test_that("auto kiest een ladder die het maximum omvat", {
  expect_equal(max(bepaal_breaks(c(1, 40), "auto")$breaks), 50)
  expect_equal(max(bepaal_breaks(c(1, 900), "auto")$breaks), 1000)
})

test_that("parse_breaks valideert invoer", {
  expect_equal(parse_breaks("1, 20, 150"), c(1, 20, 150))
  expect_equal(parse_breaks("1;20 150"), c(1, 20, 150))
  expect_error(parse_breaks("abc"))
  expect_error(parse_breaks("20, 1"))
  expect_error(parse_breaks("0, 10"))
})
