testthat::context("Testing Get ICD Comorbidites function")

testthat::test_that("Testing the function for case 1", {
  admission_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08"),
    stringsAsFactors = FALSE
  )
  complete_claims_df <- data.frame(
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
  output_df <- data.frame(
    UPIN = c("A", "A", "B", "C", "D"),
    min_ssd = c("2015-01-01", "2015-01-31", "2016-02-02", "2017-01-01", "2017-01-08"),
    max_ssd = c("2015-01-20", "2015-02-10", "2016-02-29", "2017-02-23", "2017-01-08"),
    ClaimNumber = c("25", "20", "11", "28", "30"),
    CHF = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Valvular = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    PHTN = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    PVD = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    HTN = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    HTNcx = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Paralysis = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    NeuroOther = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Pulmonary = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    DM = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    DMcx = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Hypothyroid = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Renal = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Liver = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    PUD = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    HIV = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Lymphoma = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Mets = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Tumor = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Rheumatic = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Coagulopathy = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Obesity = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    WeightLoss = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    FluidsLytes = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    BloodLoss = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    Anemia = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Alcohol = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Drugs = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Psychoses = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    Depression = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(output_df, get_icd_comorbidities(admission_df, complete_claims_df))
})