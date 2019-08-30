testthat::context("Testing Find Blood Transfusion Function")

testthat::test_that("Testing find_blood_transfusion function for case 1", {
  data_df <- data.frame(
    UPIN = c("A"),
    ClaimNumber = c("25"),
    min_ssd = c("2015-01-01"),
    max_ssd = c("2015-01-20"),
    stringsAsFactors = FALSE
  )
  blood_transfusion_claims <- data.frame(
    UPIN = c("A", "B", "A", "A", "A", "A","B", "B", "C", "D"),
    ServiceStartDate = c('2015-01-01', '2016-02-02', '2015-01-04', '2015-01-06', '2015-01-12', '2015-01-31', '2016-02-03', '2016-02-23', '2017-01-01', '2017-01-08'),
    ServiceEndDate = c('2015-01-05', '2016-02-15', '2015-01-08', '2015-01-10', '2015-01-20', '2015-02-10', '2016-02-14', '2016-02-29', '2017-02-23', '2017-01-08'),
    DiagnosisCode1 = c('R6889', '682.50', '789.03', '518.81', '784.00', '708.90', '786.50', 'G43909', '372.34', 'O3432'),
    DiagnosisCode2 = c('000.00', '041.12', '000.00', '466.11', '000.00', '000.00', '000.00', 'R6889', '000.00', 'O132'),
    DiagnosisCode3 = c('000.00', '995.29', '000.00', '000.00', '000.00', '000.00', '000.00', 'R064', '000.00', 'O99012'),
    DiagnosisCode4 = c('000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', 'O2342'),
    DiagnosisCode5 = c('000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00'),
    CPTCode = c('36430', '36430', '36430', '36430', '36430', '36430', '36430', '36430', '36430', '36430'),
    ClaimNumber = c('25', '11', '25', '18', '19', '20', '21', '22', '28','30'),
    stringsAsFactors = FALSE
  )
  output_df <- data_df <- data.frame(
    UPIN = c("A"),
    ClaimNumber = c("25"),
    min_ssd = c("2015-01-01"),
    max_ssd = c("2015-01-20"),
    blood_transfusion = 1,
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(output_df, find_blood_transfusion(data_df, blood_transfusion_claims))
})

testthat::test_that("Testing find_blood_transfusion function for case 0", {
  data_df <- data.frame(
    UPIN = c("A"),
    ClaimNumber = c("25"),
    min_ssd = c("2015-01-01"),
    max_ssd = c("2015-01-20"),
    stringsAsFactors = FALSE
  )
  blood_transfusion_claims <- data.frame(
    UPIN = c("A", "B", "A", "A", "A", "A","B", "B", "C", "D"),
    ServiceStartDate = c('2017-01-01', '2017-02-02', '2014-01-04', '2017-01-06', '2014-01-12', '2012-01-31', '2012-02-03', '2011-02-23', '2013-01-01', '2011-01-08'),
    ServiceEndDate = c('2017-01-05', '2017-02-15', '2014-01-08', '2017-01-10', '2014-01-20', '2012-02-10', '2012-02-14', '2011-02-29', '2013-02-23', '2011-01-08'),
    DiagnosisCode1 = c('R6889', '682.50', '789.03', '518.81', '784.00', '708.90', '786.50', 'G43909', '372.34', 'O3432'),
    DiagnosisCode2 = c('000.00', '041.12', '000.00', '466.11', '000.00', '000.00', '000.00', 'R6889', '000.00', 'O132'),
    DiagnosisCode3 = c('000.00', '995.29', '000.00', '000.00', '000.00', '000.00', '000.00', 'R064', '000.00', 'O99012'),
    DiagnosisCode4 = c('000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', 'O2342'),
    DiagnosisCode5 = c('000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00'),
    CPTCode = c('36430', '36430', '36430', '36430', '36430', '36430', '36430', '36430', '36430', '36430'),
    ClaimNumber = c('25', '11', '25', '18', '19', '20', '21', '22', '28','30'),
    stringsAsFactors = FALSE
  )
  output_df <- data_df <- data.frame(
    UPIN = c("A"),
    ClaimNumber = c("25"),
    min_ssd = c("2015-01-01"),
    max_ssd = c("2015-01-20"),
    blood_transfusion = 0,
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(output_df, find_blood_transfusion(data_df, blood_transfusion_claims))
})
