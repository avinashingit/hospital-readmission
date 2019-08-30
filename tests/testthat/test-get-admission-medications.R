testthat::context("Testing Get Admission Medications function")

testthat::test_that("Testing the function for case 1", {
  admission_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = as.Date(c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08")),
    max_ssd = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    stringsAsFactors = FALSE
  )
  pharma_df <- data.frame(
    upin = c("A", "A", "A", "B", "C", "D", "B", "B", "C"),
    gpi_1 = c(0, 1, 0, 0, 1, 1, 0, 1, 1),
    gpi_2 = c(0, 1, 0, 0, 1, 1, 0, 1, 1),
    gpi_3 = c(0, 1, 0, 0, 1, 1, 0, 1, 1),
    servicedate = as.Date(c("2015-01-03", "2015-01-03", "2015-01-03", "2016-02-14", "2017-02-21",
                    "2017-01-08", "2016-02-14", "2016-02-14", "2017-02-21")),
    servicedate2 = as.Date(c("2015-01-03", "2015-01-03", "2015-01-03", "2016-02-14", "2017-02-21",
                    "2017-01-08", "2016-02-14", "2016-02-14", "2017-02-21")),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    min_ssd = as.Date(c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08")),
    max_ssd = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    gpi_1 = c(1, 0, 1, 1, 1),
    gpi_2 = c(1, 0, 1, 1, 1),
    gpi_3 = c(1, 0, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  suppressWarnings(testthat::expect_equal(output_df, get_admission_medications(admission_df = admission_df,
                                                              pharma_df)))
})


testthat::test_that("Testing the function for case 1", {
  admission_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = as.Date(c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08")),
    max_ssd = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    stringsAsFactors = FALSE
  )
  pharma_df <- data.frame(
    upin = c("A", "A", "A", "B", "C", "D", "B", "B", "C"),
    gpi_1 = c(0, 1, 0, 0, 1, 1, 0, 1, 1),
    gpi_2 = c(0, 1, 0, 0, 1, 1, 0, 1, 1),
    gpi_3 = c(0, 1, 0, 0, 1, 1, 0, 1, 1),
    servicedate = as.Date(c("2015-01-03", "2015-01-03", "2015-01-03", "2016-02-14", "2017-3-21",
                            "2017-01-08", "2016-02-14", "2016-03-14", "2017-03-21")),
    servicedate2 = as.Date(c("2015-01-03", "2015-01-03", "2015-01-03", "2016-02-14", "2017-3-21",
                            "2017-01-08", "2016-02-14", "2016-03-14", "2017-03-21")),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    min_ssd = as.Date(c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08")),
    max_ssd = as.Date(c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08")),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    gpi_1 = c(1, 0, 0, 0, 1),
    gpi_2 = c(1, 0, 0, 0, 1),
    gpi_3 = c(1, 0, 0, 0, 1),
    stringsAsFactors = FALSE
  )
  suppressWarnings(testthat::expect_equal(output_df, get_admission_medications(admission_df = admission_df,
                                                              pharma_df)))
})