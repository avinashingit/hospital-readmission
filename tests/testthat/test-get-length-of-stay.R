testthat::context("Testing Length of Stay")

testthat::test_that("Test for get_length_of_stay", {
  data_df <- data.frame(
    UPIN = c("191087ee-bab0-4f41-8133-004c7a6fee2a",
             "191087ee-bab0-4f41-8133-004c7a6fee2a",
             "191087ee-bab0-4f41-8133-004c7a6fee2a",
             "191087ee-bab0-4f41-8133-004c7a6fee2b"),
    min_ssd = c("2017-06-15", "2017-06-21", "2014-05-01", "2015-01-01"),
    max_ssd = c("2017-06-30", "2017-06-21", "2014-05-21", "2015-02-28"),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("191087ee-bab0-4f41-8133-004c7a6fee2b",
             "191087ee-bab0-4f41-8133-004c7a6fee2a",
             "191087ee-bab0-4f41-8133-004c7a6fee2a",
             "191087ee-bab0-4f41-8133-004c7a6fee2a"),
    min_ssd = c("2015-01-01", "2014-05-01", "2017-06-15", "2017-06-21"),
    max_ssd = c("2015-02-28", "2014-05-21", "2017-06-30", "2017-06-21"),
    length_of_stay = c(difftime("2015-02-28", "2015-01-01", units = "days") + 1,
                       difftime("2014-05-21", "2014-05-01", units = "days") + 1,
                       difftime("2017-06-30", "2017-06-15", units = "days") + 1,
                       difftime("2017-06-21", "2017-06-21", units = "days") + 1),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(output_df, get_length_of_stay(data_df))
})