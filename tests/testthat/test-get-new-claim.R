testthat::context("Get new claim from two claims")

testthat::test_that("Get new claim", {
  first_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-30",
    stringsAsFactors = FALSE
  )

  second_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-07-1",
    max_ssd = "2017-07-12",
    stringsAsFactors = FALSE
  )

  output_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-30",
    stringsAsFactors = FALSE
  )

  expected_output_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-07-12",
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(get_new_claim(output_claim, first_claim, second_claim),
                         expected_output_claim)
})

testthat::test_that("Get new claim", {
  first_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-30",
    stringsAsFactors = FALSE
  )

  second_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-07-1",
    max_ssd = "2017-07-1",
    stringsAsFactors = FALSE
  )

  output_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-30",
    stringsAsFactors = FALSE
  )

  expected_output_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-07-1",
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(get_new_claim(output_claim, first_claim, second_claim),
                         expected_output_claim)
})

testthat::test_that("Get new claim", {
  first_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-15",
    stringsAsFactors = FALSE
  )

  second_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-15",
    stringsAsFactors = FALSE
  )

  output_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-30",
    stringsAsFactors = FALSE
  )

  expected_output_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-15",
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(get_new_claim(output_claim, first_claim, second_claim),
                         expected_output_claim)
})

testthat::test_that("Get new claim", {
  first_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-15",
    stringsAsFactors = FALSE
  )

  second_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-07-30",
    stringsAsFactors = FALSE
  )

  output_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-06-30",
    stringsAsFactors = FALSE
  )

  expected_output_claim <- data.frame(
    UPIN = "191087ee-bab0-4f41-8133-004c7a6fee2a",
    min_ssd = "2017-06-15",
    max_ssd = "2017-07-30",
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(get_new_claim(output_claim, first_claim, second_claim),
                         expected_output_claim)
})

