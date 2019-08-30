testthat::context("Testing Group Consecutive Claims Function")

testthat::test_that("Checking if correct data frame is being returned for threshold 10", {
  data_df <- data.frame(
    UPIN = c("A", "A", "A", "A"),
    min_ssd = c('2015-01-01', '2015-01-06', '2015-01-12', '2015-01-31'),
    max_ssd = c('2015-01-08', '2015-01-10', '2015-01-20', '2015-02-10'),
    ClaimNumber = c('25', '18', '19', '20'),
    stringsAsFactors = FALSE
  )
  threshold <- 10
  output_df <- data.frame(
    UPIN = c("A", "A"),
    min_ssd = c("2015-01-01", "2015-01-31"),
    max_ssd = c("2015-01-20", "2015-02-10"),
    ClaimNumber = c("25", "20"),
    stringsAsFactors = FALSE
  )
  expect_equal(output_df, group_consecutive_claims(data_df, threshold),
               check.attributes = FALSE)
})


testthat::test_that("Checking if correct data frame is being returned for threshold 5", {
  data_df <- data.frame(
    UPIN = c("A", "A", "A", "A"),
    min_ssd = c('2015-01-01', '2015-01-06', '2015-01-12', '2015-01-26'),
    max_ssd = c('2015-01-08', '2015-01-10', '2015-01-20', '2015-02-10'),
    ClaimNumber = c('25', '18', '19', '20'),
    stringsAsFactors = FALSE
  )
  threshold <- 5
  output_df <- data.frame(
    UPIN = c("A", "A"),
    min_ssd = c("2015-01-01", "2015-01-26"),
    max_ssd = c("2015-01-20", "2015-02-10"),
    ClaimNumber = c("25", "20"),
    stringsAsFactors = FALSE
  )
  expect_equal(output_df, group_consecutive_claims(data_df, threshold),
               check.attributes = FALSE)
})
