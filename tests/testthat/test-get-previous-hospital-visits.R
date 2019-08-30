testthat::context("Testing Get Previous Hospital Visits function")

testthat::test_that("Testing the function for case 1", {
  admission_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = as.Date(c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08")),
    max_ssd = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    length_of_stay = c(20, 11, 28, 54, 1),
    readmission = c(0, 0, 0, 0, 1),
    readmission_date = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    previous_admissions = c(0, 1, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  initial_claims <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = as.Date(c("2014-01-01", "2014-01-31", "2015-02-02", "2016-01-01", "2017-02-08")),
    max_ssd = as.Date(c("2014-01-20", "2014-02-10", "2015-02-28", "2016-02-23", "2017-02-08")),
    stringsAsFactors = FALSE
  )

  output_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    min_ssd = as.Date(c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08")),
    max_ssd = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    length_of_stay = c(20, 11, 28, 54, 1),
    readmission = c(0, 0, 0, 0, 1),
    readmission_date = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    previous_admissions = c(0, 1, 0, 0, 0),
    previous_hospital_visits = c(2, 2, 1, 1, 0),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(output_df, get_previous_hospital_visits(admission_df, initial_claims))
})