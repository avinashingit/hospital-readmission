context("Testing find admitting diagnosis at admission level function")

testthat::test_that("Find admitting diagnosis at admission level", {
  data_df <- data.frame(
    UPIN = c("A"),
    ClaimNumber = c("25"),
    min_ssd = c("2015-01-01"),
    max_ssd = c("2015-01-20"),
    stringsAsFactors = FALSE
  )
  claims_df <- data.frame(
    UPIN = c("A", "B", "A", "A", "A", "A","B", "B", "C", "D"),
    ServiceStartDate = c('2015-01-01', '2016-02-02', '2015-01-04', '2015-01-06', '2015-01-12', '2015-01-31', '2016-02-03', '2016-02-23', '2017-01-01', '2017-01-08'),
    ServiceEndDate = c('2015-01-05', '2016-02-15', '2015-01-08', '2015-01-10', '2015-01-20', '2015-02-10', '2016-02-14', '2016-02-29', '2017-02-23', '2017-01-08'),
    DiagnosisCode1 = c('R6889', '682.50', '789.03', '518.81', '784.00', '708.90', '786.50', 'G43909', '372.34', 'O3432'),
    DiagnosisCode2 = c('000.00', '041.12', '000.00', '466.11', '000.00', '000.00', '000.00', 'R6889', '000.00', 'O132'),
    DiagnosisCode3 = c('000.00', '995.29', '000.00', '000.00', '000.00', '000.00', '000.00', 'R064', '000.00', 'O99012'),
    DiagnosisCode4 = c('000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', 'O2342'),
    DiagnosisCode5 = c('000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00', '000.00'),
    CPTCode = c('99285', '99232', '99284', '99233', '99285', '99282', '99285', '99285', '99282', '99232'),
    ClaimNumber = c('25', '11', '25', '18', '19', '20', '21', '22', '28','30'),
    stringsAsFactors = FALSE
  )
  bodysystem_df <- get_bodysystem_df(getwd(), "chronic_body_reference_icd.rds")
  ccs_df <- get_ccs_disease_df(getwd(), "ccs_icd_9_10.rds")

  output_df <- data.frame(
    UPIN = "A",
    ClaimNumber = "25",
    min_ssd = "2015-01-01",
    max_ssd = "2015-01-20",
    admitting_diagnosis = "R6889_78903_51881_78400",
    admitting_diagnosis_bs = "8_16_19",
    admitting_diagnosis_ccs = "Abdomnl pain_Adlt resp fl_Headache/mig_Residual codes; unclassified",
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(output_df, find_admitting_diagnosis_admission_level(data_df, claims_df, bodysystem_df, ccs_df))
})