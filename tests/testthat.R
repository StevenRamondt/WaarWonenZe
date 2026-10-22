# Run alle tests: Rscript tests/testthat.R
Sys.setenv(NOT_CRAN = "true")   # ook tests die internet gebruiken (CBS)
testthat::test_dir(here::here("tests", "testthat"), stop_on_failure = TRUE)
