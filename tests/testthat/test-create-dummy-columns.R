testthat::context("Testing the Create Dummy Columns function")

testthat::test_that("Testing the function for Case 1 - unsorted", {
  data_df <- data.frame(
    UPIN = c("A", "B", "C", "D", "E"),
    string_col = c("1_2", "1", "2", "2_3_4", "4_2"),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("A", "B", "C", "D", "E"),
    string_col = c("1_2", "1", "2", "2_3_4", "4_2"),
    string_col_1 = c(1, 1, 0, 0, 0),
    string_col_2 = c(1, 0, 1, 1, 1),
    string_col_3 = c(0, 0, 0, 1, 0),
    string_col_4 = c(0, 0, 0, 1, 1),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(output_df, create_dummy_columns(data_df, "string_col", "_", "numeric",
                                                         0, FALSE))
})

testthat::test_that("Testing the function for Case 1 - sorted", {
  data_df <- data.frame(
    UPIN = c("A", "B", "C", "D", "E"),
    string_col = c("1_2", "1", "2", "2_3_4", "2_4"),
    stringsAsFactors = FALSE
  )
  output_df <- data.frame(
    UPIN = c("A", "B", "C", "D", "E"),
    string_col = c("1_2", "1", "2", "2_3_4", "2_4"),
    string_col_1 = c(1, 1, 0, 0, 0),
    string_col_2 = c(1, 0, 1, 1, 1),
    string_col_3 = c(0, 0, 0, 1, 0),
    string_col_4 = c(0, 0, 0, 1, 1),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(output_df, create_dummy_columns(data_df, "string_col", "_", "numeric",
                                                         0, FALSE))
})
