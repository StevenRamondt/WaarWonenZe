# Referentiescenario's uit de oorspronkelijke analysescripts (zie issue #16),
# gedraaid op de fictieve demodata.

demo <- laad_studenten_data(here::here("data", "voorbeeld", "studenten_demo.xlsx"))

test_that("Haaglanden: bekostigd, 9 gemeenten, klassen 1-200 ... 8001-10000", {
  res <- genereer_kaart(demo, laad_config("haaglanden"))

  expect_equal(nrow(res$data), 9)
  expect_equal(res$breaks_info$labels[1], "1-200")
  expect_equal(utils::tail(res$breaks_info$labels, 1), "8001-10000")
  expect_equal(res$rapport$na_filter, sum(demo$IsBekostigd))
  expect_true(all(res$tabel_data$aantal >= 10))
  expect_s3_class(res$plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(res$plot))
})

test_that("Nederland: eigen klassen 1-20 ... 9001-10000, alle gemeenten", {
  res <- genereer_kaart(demo, laad_config("nederland"))

  expect_equal(nrow(res$data), 342)
  expect_equal(
    res$breaks_info$labels,
    c("1-20", "21-150", "151-500", "501-2000", "2001-9000", "9001-10000")
  )
  expect_equal(res$rapport$buiten_gebied, 0)
  expect_equal(res$rapport$op_kaart + res$rapport$niet_herkend, res$rapport$na_filter)
  expect_no_error(ggplot2::ggplot_build(res$plot))
})

test_that("Den Haag op wijkniveau via postcodes", {
  res <- genereer_kaart(demo, laad_config("den_haag"))

  expect_true(all(startsWith(res$data$WK_CODE, "WK0518")))
  expect_gt(res$rapport$op_kaart, 0)
  expect_no_error(ggplot2::ggplot_build(res$plot))
})

test_that("aantallen op de kaart tellen op tot het rapport", {
  res <- genereer_kaart(demo, laad_config("zuid_holland"))
  expect_equal(sum(res$data$aantal), res$rapport$op_kaart)
})
