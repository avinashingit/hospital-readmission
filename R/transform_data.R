
# Transforming medical data with demographics -----------------------------


#' Transform claims and demographics
#'
#' @param data_dir the path to the directory in which the files are present
#' @param intermediate_dir the path to the dir to store intermediate files
#' @param claims_file the pattern of claims file
#' @param cpt_codes_file the name of cpt codes file
#' @param eligibility_file the name of eligibility file
#' @param census_file the name of census file
#' @param properties_file the name of properties_file
#' @param ccs_cpt_file the name of the ccs cpt file
#' @param ccs_bodysystem_file  the name of the icd to bodysystem file
#' @param ccs_icd_file the name of the ccs icd file
#' @param threshold the threshold for grouping claims
#' @param lowerbound the lower bound of readmisison interval
#' @param upperbound the upper bound of readmission interval
#'
#' @return data frame with admissions and demographics
#' @export
#'
#' @examples
#' transform_claims_and_demographics("/home/avinash", "/intermediate",
#' "claims.csv", "cpt_codes_file.csv", "elig.rds", "census.csv", "props.csv")
transform_claims_and_demographics <- function(data_dir,
                                              intermediate_dir,
                                              claims_file,
                                              cpt_codes_file,
                                              ccs_cpt_file,
                                              ccs_bodysystem_file,
                                              ccs_icd_file,
                                              eligibility_file,
                                              census_file,
                                              properties_file,
                                              threshold,
                                              lowerbound,
                                              upperbound) {
  properties_df <- get_properties_data(data_dir, properties_file)
  elig_df <- process_elig(data_dir, eligibility_file, properties_df)
  census_df <- process_census(data_dir, census_file, properties_df, elig_df)
  admission_df <- transform_claims(
    data_dir,
    intermediate_dir,
    claims_file,
    properties_df,
    cpt_codes_file,
    ccs_cpt_file,
    ccs_bodysystem_file,
    ccs_icd_file,
    threshold,
    lowerbound,
    upperbound
  )
  loginfo("The admission data frame is created and dimensions are %s", dim(admission_df))
  loginfo("The table of readmissions is %s", table(admission_df$readmission))
  colnames(admission_df) <- tolower(colnames(admission_df))
  saveRDS(admission_df, file.path(intermediate_dir, "admission_dataframe.rds"))
  admission_elig <- merge(admission_df, elig_df, by = "upin")
  loginfo("The admission data frame with elig and dimensions are %s", dim(admission_elig))
  loginfo("The table of readmissions is %s", table(admission_elig$readmission))
  admission_elig_census <-
    merge(admission_elig, census_df, by = "upin")
  loginfo("The admissions elig cenus and dimensions are %s", dim(admission_elig_census))
  loginfo("The table of readmissions is %s", table(admission_elig_census$readmission))
  return(admission_elig_census)
}

#' Transform claims and demographics and pharma data
#'
#' @param data_dir the path to the directory in which the files are present
#' @param intermediate_dir the path to the dir to store intermediate files
#' @param claims_file the pattern of claims file
#' @param cpt_codes_file the name of cpt codes file
#' @param eligibility_file the name of eligibility file
#' @param census_file the name of census file
#' @param properties_file the name of properties_file
#' @param pharma_file the name of the pharma file
#' @param ccs_cpt_file the name of the ccs cpt file
#' @param ccs_bodysystem_file  the name of the icd to bodysystem file
#' @param ccs_icd_file the name of the ccs icd file
#' @param threshold the threshold for grouping claims
#' @param lowerbound the lower bound of readmisison interval
#' @param upperbound the upper bound of readmission interval
#'
#' @return the data frame with claims, pharmacy claims and demographics
#' @export
#'
#' @examples
#' transform_claims_and_demographics_and_pharma("/home/avinash",
#' "/intermediate", "claims.csv", "cpt_codes_file.csv", "elig.rds",
#' "census.csv", "props.csv", "pharma.csv")
transform_claims_and_demographics_and_pharma <- function(data_dir,
                                                         intermediate_dir,
                                                         claims_file,
                                                         cpt_codes_file,
                                                         ccs_cpt_file,
                                                         ccs_bodysystem_file,
                                                         ccs_icd_file,
                                                         eligibility_file,
                                                         census_file,
                                                         properties_file,
                                                         pharma_file,
                                                         threshold,
                                                         lowerbound,
                                                         upperbound) {
  properties_df <- get_properties_data(data_dir, properties_file)
  elig_df <-
    process_elig(data_dir, eligibility_file, properties_df)
  census_df <-
    process_census(data_dir, census_file, properties_df, elig_df)
  admission_df <-
    transform_claims_and_pharmacy(data_dir,
                                  intermediate_dir,
                                  claims_file,
                                  properties_df,
                                  cpt_codes_file,
                                  ccs_cpt_file,
                                  ccs_bodysystem_file,
                                  ccs_icd_file,
                                  pharma_file,
                                  threshold,
                                  lowerbound,
                                  upperbound)
  loginfo("The admission data frame is created and dimensions are %s", dim(admission_df))
  loginfo("The table of readmissions is %s", table(admission_df$readmission))
  colnames(admission_df) <- tolower(colnames(admission_df))
  saveRDS(admission_df, file.path(intermediate_dir, "admission_pharma_df.rds"))
  admission_elig <- merge(admission_df, elig_df, by = "upin")
  loginfo("The admission data frame with elig and dimensions are %s", dim(admission_elig))
  loginfo("The table of readmissions is %s", table(admission_elig$readmission))
  admission_elig_census <-
    merge(admission_elig, census_df, by = "upin")
  loginfo("The admissions elig cenus and dimensions are %s", dim(admission_elig_census))
  loginfo("The table of readmissions is %s", table(admission_elig_census$readmission))

  return(admission_elig_census)

}

# Transforming medical data -----------------------------------------------


#' Transform claims and pharmacy
#'
#' @param data_dir the path to the directory in which the data is present
#' @param intermediate_dir the patht to the directory in which the
#' intermediate files need to be stored.
#' @param claims_file the name of the claims file
#' @param props_df the properties data frame
#' @param cpt_codes_file the name of the cpt codes file
#' @param pharma_file the name of the pharmacy file
#' @param ccs_cpt_file the name of the ccs cpt file
#' @param ccs_bodysystem_file  the name of the icd to bodysystem file
#' @param ccs_icd_file the name of the ccs icd file
#' @param threshold the threshold for grouping claims
#' @param lowerbound the lower bound of readmisison interval
#' @param upperbound the upper bound of readmission interval
#'
#' @return the claims and pharmacy together as a data frame
#' @export
#'
#' @examples
#' transform_claims_and_pharmacy("/home", "/home/intermediate",
#' "claims.csv", props_df, "cpt_codes.csv", "pharma.csv")
transform_claims_and_pharmacy <- function(data_dir,
                                          intermediate_dir,
                                          claims_file,
                                          props_df,
                                          cpt_codes_file,
                                          ccs_cpt_file,
                                          ccs_bodysystem_file,
                                          ccs_icd_file,
                                          pharma_file,
                                          threshold,
                                          lowerbound,
                                          upperbound) {
  admission_comorbidities_df <- preprocess_claims(data_dir,
                                                  intermediate_dir,
                                                  claims_file,
                                                  props_df,
                                                  cpt_codes_file,
                                                  ccs_cpt_file,
                                                  ccs_bodysystem_file,
                                                  ccs_icd_file,
                                                  threshold,
                                                  lowerbound,
                                                  upperbound)
  pharma_df <- process_pharma_file(data_dir, pharma_file)
  loginfo("Getting admission medications")
  admission_medications_df <-
    get_admission_medications(admission_comorbidities_df, pharma_df)
  loginfo("Found out the admission medications for the admissions")
  admission_medications_df$min_ssd <-
    admission_medications_df$max_ssd <-
    admission_medications_df$readmission_date <- NULL
  return(admission_medications_df)
}

#' Transform claims
#'
#' @param data_dir the path to the directory in which the data is present
#' @param intermediate_dir the path to the directory in which the intermediate
#' files need to be stored
#' @param claims_file the name of the claims file
#' @param props_df the properties data frame
#' @param cpt_codes_file the name of the cpt codes file
#' @param ccs_cpt_file the name of the ccs cpt file
#' @param ccs_bodysystem_file  the name of the icd to bodysystem file
#' @param ccs_icd_file the name of the ccs icd file
#' @param threshold the threshold for grouping claims
#' @param lowerbound the lower bound of readmisison interval
#' @param upperbound the upper bound of readmission interval
#'
#' @return the admissions as a data frame
#' @export
#'
#' @examples
#' transform_claims("/home/avinash", "/home/intermediate", "claims.csv",
#' props_df, "cpt_codes.csv")
transform_claims <- function(data_dir,
                             intermediate_dir,
                             claims_file,
                             props_df,
                             cpt_codes_file,
                             ccs_cpt_file,
                             ccs_bodysystem_file,
                             ccs_icd_file,
                             threshold,
                             lowerbound,
                             upperbound) {

  admission_comorbidities_df <- preprocess_claims(data_dir,
                                                  intermediate_dir,
                                                  claims_file,
                                                  props_df,
                                                  cpt_codes_file,
                                                  ccs_cpt_file,
                                                  ccs_bodysystem_file,
                                                  ccs_icd_file,
                                                  threshold,
                                                  lowerbound,
                                                  upperbound)
  admission_comorbidities_df$min_ssd <- admission_comorbidities_df$max_ssd <-
    admission_comorbidities_df$readmission_date <- NULL
  admission_comorbidities_df <- as.data.frame(admission_comorbidities_df)
  return(admission_comorbidities_df)
}

# Claims preprocessing ----------------------------------------------------


#' Pre process claims
#'
#' This function reads the claims file and finds the admissions, readmissions,
#' emergency admissions, procedures, comorbidities.
#'
#' @param data_dir The path to the directory in which the data is present
#' @param intermediate_dir the path to the directory in which the intermediate
#' files need to be stored
#' @param claims_file the name of the claims file
#' @param props_df the properties dataframe
#' @param cpt_codes_file the name of the cpt codes file
#' @param ccs_cpt_file the name of the ccs cpt file
#' @param ccs_bodysystem_file  the name of the icd to bodysystem file
#' @param ccs_icd_file the name of the ccs icd file
#' @param threshold the threshold for grouping claims
#' @param lowerbound the lower bound of readmisison interval
#' @param upperbound the upper bound of readmission interval
#'
#' @return the admission dataframe
#' @export
#'
#' @examples
#' preprocess_claims('/home', '/home/intermediate', 'claims.csv', props_df,
#' 'cpt_codes.csv')
preprocess_claims <- function(data_dir,
                              intermediate_dir,
                              claims_file,
                              props_df,
                              cpt_codes_file,
                              ccs_cpt_file,
                              ccs_bodysystem_file,
                              ccs_icd_file,
                              threshold,
                              lowerbound,
                              upperbound) {

# Reading cpt codes -----------------------------------------------------------------

  loginfo("Reading cpt codes file")
  cpt_codes_df <- process_cpt_codes(data_dir, cpt_codes_file)
  loginfo("The dimension of the cpt codes file are %s", dim(cpt_codes_df))

# Processing claims -----------------------------------------------------------------

  loginfo("Processing claims file")
  filter_criteria <-
    lazyeval::interp(~CPTCode %in% c(EviveUtil::remove_na(cpt_codes_df$inpatient),
                                     EviveUtil::remove_na(cpt_codes_df$emergency),
                                     EviveUtil::remove_na(cpt_codes_df$blood_transfusion)))
  claims_df <- process_claims(data_dir, claims_file, props_df, filter_criteria)
  loginfo("The claims file is read and its dimensions are %s", dim(claims_df))
  saveRDS(claims_df, file.path(intermediate_dir, "claims_df.rds"))
  loginfo("Saved the read claims file in %s as claims_df.rds", intermediate_dir)

  loginfo("Getting inpatient claims")
  inpatient_claims_df <- get_cptwise_claims(claims_df,
                                            EviveUtil::remove_na(cpt_codes_df$inpatient))
  saveRDS(inpatient_claims_df, file.path(intermediate_dir, "inpatient_claims_df.rds"))

  loginfo("Getting complete claims for UPINs who are admitted")
  filter_criteria <-
    lazyeval::interp(~UPIN %in% EviveUtil::remove_na(unique(inpatient_claims_df$UPIN)))
  complete_claims_df <- process_claims(data_dir, claims_file, props_df, filter_criteria)
  loginfo("The claims of person who are admitted are read and its dimentions are %s",
          dim(complete_claims_df))
  saveRDS(complete_claims_df, file.path(intermediate_dir, "complete_claims.rds"))

# Getting readmissions --------------------------------------------------------------

  loginfo("Finding readmissions for each admission")
  inpatient_claims_readmission <-
    get_readmissions(inpatient_claims_df, complete_claims_df, threshold, lowerbound,
                     upperbound)
  loginfo("The admissions are grouped and its dimensions are %s",
          dim(inpatient_claims_readmission))
  loginfo("The table of readmissions are %s",
          table(inpatient_claims_readmission$readmission))
  saveRDS(inpatient_claims_readmission,
          file.path(intermediate_dir, "inpatient_claims_readmission.rds"))

# Getting previous admissions -------------------------------------------------------

  loginfo("Getting previous admissions.")
  inpatient_claims_readmission <- get_previous_admissions(inpatient_claims_readmission)
  saveRDS(inpatient_claims_readmission,
          file.path(intermediate_dir, "inpatient_claims_read_prev_ad.rds"))
  loginfo("The previous admissions before each admission are found out.")


# Getting hospital visits -----------------------------------------------------------

  loginfo("Getting hospital visits")
  initial_observation_claims <-
    get_cpt_claims_grouped(complete_claims_df, cpt_codes_df$initial, 2)
  inpatient_claims_readmission <-
    get_previous_hospital_visits(inpatient_claims_readmission, initial_observation_claims)
  loginfo("The hospital visits before each admission are found out.")


# Getting index admission diagnosis -------------------------------------------------

  loginfo("Getting index admission diagnosis")
  inpatient_claims_with_admit_diagnosis_df <-
    get_admitting_diagnosis(inpatient_claims_readmission,
                            complete_claims_df,
                            ccs_bodysystem_file, ccs_icd_file, data_dir)
  saveRDS(inpatient_claims_with_admit_diagnosis_df,
          file.path(intermediate_dir, "inpatient_claims_with_admit_diagnosis.rds"))
  loginfo("The admitting diagnosis for each admission is found out")

# Getting procedures undertaken during each admission -------------------------------

  loginfo("Getting the procedures undertaken during the admission")
  inpatient_claims_with_admit_procedures <-
    get_admission_procedures(inpatient_claims_with_admit_diagnosis_df,
                             complete_claims_df,
                             ccs_cpt_file)
  saveRDS(inpatient_claims_with_admit_procedures,
          file.path(intermediate_dir, "inpatient_claims_with_admit_procedures.rds"))
  loginfo("The procedures undertaken during each admission are found out.")

# Getting emergency claims ----------------------------------------------------------

  loginfo("Getting emergency claims")
  emergency_claims_df <-
    get_cpt_claims_grouped(claims_df, cpt_codes_df$emergency, threshold)
  saveRDS(emergency_claims_df, file.path(intermediate_dir,
                                         "emergency_claims_df.rds"))

# Getting previous emergency admissions ----------------------------------------------

  loginfo("Getting previous emergency admissions")
  admission_df <-
    get_previous_emergency_admissions(inpatient_claims_with_admit_procedures,
                                      emergency_claims_df)
  saveRDS(admission_df, file.path(intermediate_dir, "emergency_admissions.rds"))
  loginfo("The number of previous emergency admissions bef. each admission are found.")

# Checking for bloood transfusion ---------------------------------------------------

  loginfo("Checking if blood transfusions were done")
  admission_df <-
    check_if_blood_transfusion(admission_df,
                               claims_df,
                               EviveUtil::remove_na(cpt_codes_df$blood_transfusion))
  saveRDS(admission_df, file.path(intermediate_dir, "blood_transfusion.rds"))
  loginfo("Checking for blood transfusion during the admission is done")


# Checking for comorbidities --------------------------------------------------------

  loginfo("Getting comorbidities during admission")
  admission_comorbidites_df <-
    get_icd_comorbidities(admission_df, complete_claims_df)
  saveRDS(admission_comorbidites_df,
          file.path(intermediate_dir, "admission_comorbidities.rds"))
  loginfo("Found out the comorbidites at the time of admission")

  return(admission_comorbidites_df)
}