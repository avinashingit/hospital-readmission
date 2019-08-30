testthat::context("Testing Get Previous Admissions function")

testthat::test_that("Testing get previous admissions", {
  data_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08"),
    length_of_stay = c(20, 11, 28, 54, 1),
    readmission = c(0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08"),
    length_of_stay = c(20, 11, 28, 54, 1),
    readmission = c(0, 0, 0, 0, 0),
    previous_admissions = c(0, 1, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  expect_equal(output_df, get_previous_admissions(data_df), check.attributes = FALSE)
})

testthat::test_that("Testing get previous admissions", {
  data_df <- data.frame(
    UPIN = c("A", "A", "A", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08"),
    length_of_stay = c(20, 11, 28, 54, 1),
    readmission = c(0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("A", "A", "A", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08"),
    length_of_stay = c(20, 11, 28, 54, 1),
    readmission = c(0, 0, 0, 0, 0),
    previous_admissions = c(0, 1, 2, 0, 0),
    stringsAsFactors = FALSE
  )
  expect_equal(output_df, get_previous_admissions(data_df), check.attributes = FALSE)
})

