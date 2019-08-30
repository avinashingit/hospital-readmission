context("Testing Check If Readmission Function")

testthat::test_that("Testing the function first time", {
  data_df <- data.frame(
    UPIN = c("A", "A", "A", "A", "A"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-11-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-11-08"),
    stringsAsFactors = FALSE
  )
  lowerbound <- 0
  upperbound <- 30
  output_df <- data.frame(
    UPIN = c("A", "A", "A", "A"),
    ClaimNumber = c("25", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2016-02-02", "2017-01-01", "2017-11-08"),
    max_ssd = c("2015-01-20", "2016-02-29", "2017-02-23", "2017-11-08"),
    readmission = c(1, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  expect_equal(output_df, check_if_readmission(data_df, lowerbound, upperbound),
               check.attributes = FALSE)
})

testthat::test_that("Testing the function second time", {
  data_df <- data.frame(
    UPIN = c("A", "A", "A", "A", "A"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2016-03-12", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2016-03-23", "2017-01-08"),
    stringsAsFactors = FALSE
  )
  lowerbound <- 0
  upperbound <- 30
  output_df <- data.frame(
    UPIN = c("A", "A", "A"),
    ClaimNumber = c("25", "11", "30"),
    min_ssd = c("2015-01-01", "2016-02-02", "2017-01-08"),
    max_ssd = c("2015-01-20", "2016-02-29", "2017-01-08"),
    readmission = c(1, 1, 0),
    stringsAsFactors = FALSE
  )
  expect_equal(output_df, check_if_readmission(data_df, lowerbound, upperbound),
               check.attributes = FALSE)
})

