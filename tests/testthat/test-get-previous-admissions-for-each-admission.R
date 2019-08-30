context("Testing Get Previous Admissions for Each Admission function")

testthat::test_that("Testing for case 1", {
  data_df <- data.frame(
    UPIN = c("A", "A", "A", "A", "A"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08"),
    length_of_stay = c(20, 11, 28, 54, 1),
    readmission = c(0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("A", "A", "A", "A", "A"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08"),
    length_of_stay = c(20, 11, 28, 54, 1),
    readmission = c(0, 0, 0, 0, 0),
    previous_admissions = c(0, 1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  expect_equal(output_df, get_previous_admissions_for_each_admission(data_df),
               check.attributes = FALSE)
})

testthat::test_that("Testing for case 2", {
  data_df <- data.frame(
    UPIN = c("A"),
    ClaimNumber = c("25"),
    min_ssd = c("2015-01-01"),
    max_ssd = c("2015-01-20"),
    length_of_stay = c(20),
    readmission = c(0),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("A"),
    ClaimNumber = c("25"),
    min_ssd = c("2015-01-01"),
    max_ssd = c("2015-01-20"),
    length_of_stay = c(20),
    readmission = c(0),
    previous_admissions = c(0),
    stringsAsFactors = FALSE
  )

  expect_equal(output_df, get_previous_admissions_for_each_admission(data_df),
               check.attributes = FALSE)
})
