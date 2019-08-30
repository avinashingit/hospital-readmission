#' Title
#'
#' @param data_dir
#' @param intermediate_dir
#' @param output_dir
#' @param properties_file
#' @param eligibility_file
#' @param census_file
#' @param census_pca_file
#' @param ccs_cpt_file
#' @param ccs_bodysystem_file
#' @param ccs_icd_file
#' @param claims_file
#' @param cpt_codes_file
#' @param pharma_file
#' @param threshold
#' @param lowerbound
#' @param upperbound
#' @param model_file
#' @param reference_file
#'
#' @return
#' @export
#'
#' @examples
predict_main_all_glmnet <- function(data_dir,
                                    intermediate_dir,
                                    output_dir,
                                    properties_file,
                                    eligibility_file,
                                    census_file,
                                    census_pca_file = NULL,
                                    ccs_cpt_file,
                                    ccs_bodysystem_file,
                                    ccs_icd_file,
                                    claims_file,
                                    cpt_codes_file,
                                    pharma_file,
                                    threshold = 10,
                                    lowerbound = 0,
                                    upperbound = 30,
                                    model_file,
                                    reference_file) {
  admission_pharma_df <-
    transform_claims_and_demographics_and_pharma(
      data_dir,
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
      upperbound
    )
  loginfo("The dimensions of the admission pharmacy dataframe is %s",
          dim(admission_pharma_df))

  admission_pharma_df <- as.data.frame(admission_pharma_df)
  cols <- sapply(admission_pharma_df, is.logical)
  admission_pharma_df[,cols] <- lapply(admission_pharma_df[,cols], as.numeric)
  colnames(admission_pharma_df) <- gsub(" ", "_", colnames(admission_pharma_df))
  colnames(admission_pharma_df) <- gsub(",", "", colnames(admission_pharma_df))

  admission_pharma_df$readmission <-
    factor(admission_pharma_df$readmission,
           levels = c(0, 1))
  saveRDS(admission_pharma_df,
          file.path(intermediate_dir, "data_for_modeling_claims_pharma_dem.rds"))

  loginfo("Creating dummy columns")
  admission_pharma_df <- EviveUtil::createDummy(admission_pharma_df,
                                                col = c("age", "gender",
                                                        "computed_ethnicity",
                                                        "scheme_type"),
                                                drop_base_cols = TRUE)
  admission_pharma_df <-
    create_dummy_columns(admission_pharma_df, "admitting_diagnosis_bs", "_",
                         "numeric", 0, FALSE)
  saveRDS(admission_pharma_df,
          file.path(intermediate_dir, "data_for_modeling_claims_pharma_dummy_vars.rds"))
  prediction_with_glmnet(model_file,
                         file.path(intermediate_dir, "data_for_modeling_claims_pharma_dummy_vars.rds"),
                         output_dir,
                         reference_file)
}

#' Title
#'
#' @param data_dir
#' @param intermediate_dir
#' @param output_dir
#' @param properties_file
#' @param eligibility_file
#' @param census_file
#' @param census_pca_file
#' @param ccs_cpt_file
#' @param ccs_bodysystem_file
#' @param ccs_icd_file
#' @param claims_file
#' @param cpt_codes_file
#' @param pharma_file
#' @param threshold
#' @param lowerbound
#' @param upperbound
#' @param model_file
#' @param pca_output_file
#' @param number_of_principal_components_file
#' @param reference_file
#'
#' @return
#' @export
#'
#' @examples
predict_main_pca_glmnet <- function(data_dir,
                                    intermediate_dir,
                                    output_dir,
                                    properties_file,
                                    eligibility_file,
                                    census_file,
                                    census_pca_file = NULL,
                                    ccs_cpt_file,
                                    ccs_bodysystem_file,
                                    ccs_icd_file,
                                    claims_file,
                                    cpt_codes_file,
                                    pharma_file,
                                    threshold = 10,
                                    lowerbound = 0,
                                    upperbound = 30,
                                    model_file,
                                    pca_output_file,
                                    number_of_principal_components_file,
                                    reference_file) {
  admission_pharma_df <-
    transform_claims_and_demographics_and_pharma(
      data_dir,
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
      upperbound
    )
  loginfo("The dimensions of the admission pharmacy dataframe is %s",
          dim(admission_pharma_df))

  admission_pharma_df <- as.data.frame(admission_pharma_df)
  cols <- sapply(admission_pharma_df, is.logical)
  admission_pharma_df[,cols] <- lapply(admission_pharma_df[,cols], as.numeric)
  colnames(admission_pharma_df) <- gsub(" ", "_", colnames(admission_pharma_df))
  colnames(admission_pharma_df) <- gsub(",", "", colnames(admission_pharma_df))

  admission_pharma_df$readmission <-
    factor(admission_pharma_df$readmission,
           levels = c(0, 1))
  saveRDS(admission_pharma_df,
          file.path(intermediate_dir, "data_for_modeling_claims_pharma_dem.rds"))

  loginfo("Creating dummy columns")
  admission_pharma_df <- EviveUtil::createDummy(admission_pharma_df,
                                                col = c("age", "gender",
                                                        "computed_ethnicity",
                                                        "scheme_type"),
                                                drop_base_cols = TRUE)
  admission_pharma_df <-
    create_dummy_columns(admission_pharma_df, "admitting_diagnosis_bs", "_",
                         "numeric", 0, FALSE)
  saveRDS(admission_pharma_df,
          file.path(intermediate_dir, "data_for_modeling_claims_pharma_dummy_vars.rds"))
  prediction_with_pca_glmnet(
    model_file,
    file.path(intermediate_dir, "data_for_modeling_claims_pharma_dummy_vars.rds"),
    output_dir,
    pca_output_file,
    number_of_principal_components_file,
    reference_file
  )
}