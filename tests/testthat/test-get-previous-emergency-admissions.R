testthat::context("Testing Get Previous Emergency Admissions function")

testthat::test_that("Testing get_previous_emergency_admissions for case 1", {
  emergency_claims <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = as.Date(c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08")),
    max_ssd = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    stringsAsFactors = FALSE
  )

  inpatient_claims <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = as.Date(c("2014-01-01", "2014-01-31", "2015-02-02", "2016-01-01", "2017-02-08")),
    max_ssd = as.Date(c("2014-01-20", "2014-02-10", "2015-02-28", "2016-02-23", "2017-02-08")),
    stringsAsFactors = FALSE
  )

  output_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    min_ssd = as.Date(c("2014-01-01", "2014-01-31", "2015-02-02", "2016-01-01", "2017-02-08")),
    max_ssd = as.Date(c("2014-01-20", "2014-02-10", "2015-02-28", "2016-02-23", "2017-02-08")),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    previous_emergency_admissions = c(0, 0, 0, 0, 1),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(output_df, get_previous_emergency_admissions(inpatient_claims,
                                                                      emergency_claims))
})

