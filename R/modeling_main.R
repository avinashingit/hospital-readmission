# Building client specific models ------------------------------------

#' Build model with claims, pharmacy and demographics data
#'
#' Use this function to build model when claims, pharmacy and demographics
#' data are present
#'
#' @param data_dir The path to the directory in which the data files are present
#' . Give the complete path to the directory irrespective of the working
#' directory.
#' @param intermediate_dir The path to the directory to which the intermediate
#' data files are to be stored. Give the complete path to the directory
#' irrespective of the working directory.
#' @param output_dir The path to the directory in which the output files need
#' to be stored. Give the complete path irrespective of the working directory.
#' @param properties_file the name of the properties file
#' @param eligibility_file the name of the eligibility file
#' @param census_file the name of the census file
#' @param census_pca_file the name of the census pca file
#' @param ccs_cpt_file the name of the ccs cpt file
#' @param ccs_icd_file the name of the ccs icd file
#' @param claims_file the name of the claims file
#' @param cpt_codes_file the name of the cpt codes file
#' @param pharma_file the name of the pharmacy data file
#' @param ccs_bodysystem_file the name of ccs bodysystem file
#' @param threshold the threshold for grouping claims
#' @param lowerbound the lower bound of the readmission interval
#' @param upperbound the upper bound of the readmission interval
#'
#' @return This function does not return anything. All the models are built and
#'  and stored in separate files
#' @export
#' @import logging
#' @import pROC
#' @import ROCR
#'
#' @examples
#' build_model_with_claims_and_pharma("/home/avinash",
#' "/home/avinash/intermediate",
#' "/output/", "properties.csv", "elig.rds", "census.csv", "ccs_cpt.rds",
#' "ccs_icd.rds", "claims.csv", "cpt_codes.csv", "pharma.csv")
#'
#' build_model_with_claims("/home/avinash", "/home/avinash/intermediate",
#' "/output/", "properties.csv", "elig.rds", "census.csv", "census_pca.csv",
#' "ccs_cpt.rds", "ccs_icd.rds", "claims.csv", "cpt_codes.csv", "pharma.csv")
build_model_with_claims_and_pharma <- function(data_dir,
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
                                               upperbound = 30) {
  loginfo("Started building model with claims and demographics and pharma data")
  loginfo("Getting the data frame required for modeling")
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

  loginfo("Splitting the data into train and test")
  train_index <- EviveUtil::train_test_split(admission_pharma_df,
                                             visit = "readmission",
                                             split_per = 0.9)
  saveRDS(admission_pharma_df[train_index, ],
          file.path(intermediate_dir,
                    "data_for_modeling_claims_pharma_dem_train.rds"))
  saveRDS(admission_pharma_df[-train_index, ],
          file.path(intermediate_dir,
                    "data_for_modeling_claims_pharma_dem_test.rds"))
  saveRDS(admission_pharma_df,
          file.path(intermediate_dir, "data_for_modeling_claims_pharma_dummies.rds"))
  admission_pharma_df <- as.data.frame(admission_pharma_df)

  loginfo("Building glmnet model")
  EviveAutoEncoder::build_glmnet_model(
    admission_pharma_df[train_index, ],
    admission_pharma_df[-train_index, ],
    data_vars = setdiff(colnames(admission_pharma_df), c("upin", "readmission")),
    outcome = "readmission",
    threshold = 0.1,
    output_name = "without_feature_selection",
    data_dir = output_dir
  )
  loginfo("Modeling process ended")
}

# Building global models --------------------------------------------------

#' Build Global model using claims, pharmacy claims and demographics data
#'
#' @param train_files A vector containing the absolute paths to each of the
#' training data sets for each client
#' @param test_files A vector containing the absolute paths to each of the
#' testing datasets for each client
#' @param client_codes the client codes of each client in the same order as that
#' of the files in the above vectors
#' @param intermediate_dir the path to the directory in which the intermediate
#' files need to be stored
#' @param global_output_dir the output directory to which the model results need to be
#' stored
#'
#' @return This function does not return anything. It stores the model outputs
#' in separate files
#' @export
#'
#' @examples
#' build_global_model_using_claims_pharma_and_demographics(
#' c("thd_train.csv", "aa_train.csv", "df_train.csv", "pepsi_train.csv"),
#' c("thd_test.csv", "aa_test.csv", "df_test.csv", "pepsi_test.csv"),
#' c(1004, 1013, 1011, 1030),
#' "/home/avinash/intermediate_dir")
build_global_model_using_claims_pharma_and_demographics <-
  function(train_files, test_files, client_codes, intermediate_dir, global_output_dir) {

    admission_train_df <- data.frame()
    for(i in 1:length(train_files)) {
      temp_train_df <- readRDS(train_files[i])
      temp_train_df$client <- client_codes[i]
      if(nrow(admission_train_df) == 0) {
        admission_train_df <- temp_train_df
      } else {
        admission_train_df <- data.table::rbindlist(list(admission_train_df, temp_train_df),
                                          fill = T)
      }
    }
    admission_train_df[is.na(admission_train_df)] <- 0

    admission_test_df <- data.frame()
    for(i in 1:length(test_files)) {
      temp_test_df <- readRDS(test_files[i])
      temp_test_df$client <- client_codes[i]
      if(nrow(admission_test_df) == 0) {
        admission_test_df <- temp_test_df
      } else {
        admission_test_df <- data.table::rbindlist(list(admission_test_df, temp_test_df), fill = T)
      }
    }
    admission_test_df[is.na(admission_test_df)] <- 0

    saveRDS(admission_train_df,
            file.path(intermediate_dir,
                      "global_model_data_with_claims_pharma_dem_train.rds"))
    saveRDS(admission_test_df,
            file.path(intermediate_dir,
                      "global_model_data_with_claims_pharma_dem_test.rds"))

    admission_train_df$readmission <- factor(admission_train_df$readmission,
                                             levels = c(0, 1))
    admission_test_df$readmission <- factor(admission_test_df$readmission,
                                            levels = c(0, 1))

    colnames(admission_train_df) <- tolower(colnames(admission_train_df))
    colnames(admission_test_df) <- tolower(colnames(admission_test_df))

    loginfo("Building glmnet model")
    EviveAutoEncoder::build_glmnet_model(
      admission_train_df,
      admission_test_df,
      data_vars = setdiff(
        colnames(admission_train_df),
        c("upin", "admitting_diagnosis",
          "admitting_diagnosis_bs",
          "admitting_diagnosis_ccs")
      ),
      outcome = "readmission",
      threshold = 0.1,
      output_name = "claims_pharma_and_demographics_global_90",
      data_dir = global_output_dir
    )
    loginfo("Modeling ended")
  }

#' Build global model with Feature Selection
#'
#' @param train_files the vector containing paths to each of the training files
#' @param test_files the vector containing paths to each of the test files
#' @param client_codes the client codes vector
#' @param intermediate_dir the path to the directory in which the intermediate files and
#' reference files are stored
#' @param global_output_dir the path to the directory in which the output files are stored
#' @param loglikelihood_ref_file_name the name of the log likelihood reference file which
#' should be present in the intermediate directory
#'
#' @return This function does not return anything. It builds the model and stores the
#' model output to the output directory
#' @export
#'
#' @examples
#' build_global_model_with_feature_selection(train_files, test_files, client_codes,
#' intermediate_dir, global_output_dir, "ref_file.csv")
build_global_model_with_feature_selection <-
  function(train_files,
           test_files,
           client_codes,
           intermediate_dir,
           global_output_dir,
           loglikelihood_ref_file_name) {

    admission_train_df <- data.frame()
    for(i in 1:length(train_files)) {
      temp_train_df <- readRDS(train_files[i])
      temp_train_df$client <- client_codes[i]
      if(nrow(admission_train_df) == 0) {
        admission_train_df <- temp_train_df
      } else {
        admission_train_df <- data.table::rbindlist(list(admission_train_df, temp_train_df),
                                                    fill = T)
      }
    }
    admission_train_df[is.na(admission_train_df)] <- 0

    admission_test_df <- data.frame()
    for(i in 1:length(test_files)) {
      temp_test_df <- readRDS(test_files[i])
      temp_test_df$client <- client_codes[i]
      if(nrow(admission_test_df) == 0) {
        admission_test_df <- temp_test_df
      } else {
        admission_test_df <- data.table::rbindlist(list(admission_test_df, temp_test_df), fill = T)
      }
    }
    admission_test_df[is.na(admission_test_df)] <- 0

    saveRDS(admission_train_df,
            file.path(intermediate_dir,
                      "global_model_data_with_claims_pharma_dem_train.rds"))
    saveRDS(admission_test_df,
            file.path(intermediate_dir,
                      "global_model_data_with_claims_pharma_dem_test.rds"))

    admission_train_df$readmission <- factor(admission_train_df$readmission,
                                             levels = c(0, 1))
    admission_test_df$readmission <- factor(admission_test_df$readmission,
                                            levels = c(0, 1))

    colnames(admission_train_df) <- tolower(colnames(admission_train_df))
    colnames(admission_test_df) <- tolower(colnames(admission_test_df))

    output <-
      do_feature_selection_by_loglikelihood_new(intermediate_dir,
                                                loglikelihood_ref_file_name,
                                                "global_model_data_with_claims_pharma_dem_train.rds",
                                                "LL_OUTPUT_GLOBAL")
    ll_variables <- output$variables[output$threshold_v == 1]
    data_vars <- c(ll_variables, "upin", "readmission")

    admission_train_df <- as.data.frame(admission_train_df)
    admission_train_df_fs <- admission_train_df[, data_vars]
    saveRDS(admission_train_df_fs, file.path(output_dir, "global_train_data_fs.rds"))

    admission_test_df <- as.data.frame(admission_test_df)
    admission_test_df_fs <- admission_test_df[, data_vars]
    saveRDS(admission_test_df_fs, file.path(output_dir, "global_test_data_fs.rds"))

    EviveAutoEncoder::build_glmnet_model(
      train_df = admission_train_df_fs,
      test_df = admission_train_df_fs,
      data_vars = setdiff(colnames(admission_train_df_fs), c("upin", "admitting_diagnosis",
                                                                 "admitting_diagnosis_bs",
                                                                 "admitting_diagnosis_ccs")),
      output_name = "global_model_v2_with_ll_fs_variables",
      outcome = "readmission",
      threshold = 0.1,
      data_dir = global_output_dir
    )

  }


# Building Global Models with PCA ---------------------------------------------------

#' Build global model with PCA
#'
#' Build global model using PCA and GLMNET
#'
#' @param train_files the vector containing the paths to the training files
#' @param test_files the vector containing the paths to the test files
#' @param client_codes the client codes vector
#' @param intermediate_dir the path to the directory to which the intermediate files and
#' reference files are stored
#' @param global_output_dir the path to the directory to which the output is to be stored
#' @param pca_ref_file the name of the PCA reference file which is stored in the
#' intermediate directory
#'
#' @return This function does not return anything. It stores the model output in the
#' output directory
#' @export
#'
#' @examples
#' build_global_model_with_pca(train_files, test_files, client_codes, intermediate_dir,
#' global_output_dir, pca_ref_file)
build_global_model_with_pca <-
  function(train_files,
           test_files,
           client_codes,
           intermediate_dir,
           global_output_dir,
           pca_ref_file) {
    admission_train_df <- data.frame()
    for(i in 1:length(train_files)) {
      temp_train_df <- readRDS(train_files[i])
      temp_train_df$client <- client_codes[i]
      if(nrow(admission_train_df) == 0) {
        admission_train_df <- temp_train_df
      } else {
        admission_train_df <- data.table::rbindlist(list(admission_train_df, temp_train_df),
                                                    fill = T)
      }
    }
    admission_train_df[is.na(admission_train_df)] <- 0

    admission_test_df <- data.frame()
    for(i in 1:length(test_files)) {
      temp_test_df <- readRDS(test_files[i])
      temp_test_df$client <- client_codes[i]
      if(nrow(admission_test_df) == 0) {
        admission_test_df <- temp_test_df
      } else {
        admission_test_df <- data.table::rbindlist(list(admission_test_df,
                                                        temp_test_df), fill = T)
      }
    }
    admission_test_df[is.na(admission_test_df)] <- 0

    saveRDS(admission_train_df,
            file.path(intermediate_dir,
                      "global_model_data_with_claims_pharma_dem_train.rds"))
    saveRDS(admission_test_df,
            file.path(intermediate_dir,
                      "global_model_data_with_claims_pharma_dem_test.rds"))

    admission_train_df$readmission <- factor(admission_train_df$readmission,
                                             levels = c(0, 1))
    admission_test_df$readmission <- factor(admission_test_df$readmission,
                                            levels = c(0, 1))

    pca_v2_ref_file <- readr::read_csv(file.path(intermediate_dir, pca_ref_file))
    cols_to_not_consider <- EviveUtil::remove_na(pca_v2_ref_file$remove)
    cols_to_consider <- setdiff(colnames(global_train_data), cols_to_not_consider)
    pca_output <- prcomp(data_df[, cols_to_consider])
    saveRDS(pca_output, file.path(output_dir, "pca_output.rds"))
    select_pcs <- get_pcs(pca_output, output_dir)
    pca_selected <- pca_output$x[1:select_pcs]
    admission_train_df_pca <- cbind(admission_train_df[, cols_to_not_consider],
                                    pca_selected)

    admission_test_df_pca <- scale(admission_test_df[, cols_to_consider],
                                 pca_output$center,
                                 pca_output$scale) %*% pca_output$rotation
    admission_test_df_pca <- as.data.frame(admission_test_df_pca)
    admission_test_df_pca <- cbind(admission_test_df_pca[, cols_to_not_consider],
                                   admission_test_df_pca[, 1:select_pcs])


    EviveAutoEncoder::build_glmnet_model(
      train_df = admission_train_df_pca,
      test_df = admission_test_df_pca,
      data_vars = setdiff(colnames(admission_train_df_pca), c("upin",
                                                              "admitting_diagnosis",
                                                              "admitting_diagnosis_bs",
                                                              "admitting_diagnosis_ccs")),
      output_name = "global_model_with_pca_all_variables",
      outcome = "readmission",
      threshold = 0.1,
      data_dir = output_dir
    )

  }


#' Build model with PCA and GLMNET after feature selection
#'
#' @param train_files the vector containing the path to the train files
#' @param test_files the vector containing the path to the test files
#' @param client_codes the vector of client codes in the same order as train files
#' @param intermediate_dir the directory in which the intermediate files are to be stored
#' and the reference files are present
#' @param global_output_dir the directory to which the model and pca output are to be
#' stored
#' @param pca_ref_file the name of the pca reference file located in the intermediate
#' directory
#'
#' @return This function does not return anything. It builds the model and stores the
#' model output in output_dir
#' @export
#'
#' @examples
#' build_global_model_with_pca_feature_selection(train_files, test_files, client_codes,
#' intermediate_dir, global_output_dir, pca_ref_file)
build_global_model_with_pca_feature_selection <-
  function(train_files,
           test_files,
           client_codes,
           intermediate_dir,
           global_output_dir,
           pca_ref_file) {
    admission_train_df <- data.frame()
    for(i in 1:length(train_files)) {
      temp_train_df <- readRDS(train_files[i])
      temp_train_df$client <- client_codes[i]
      if(nrow(admission_train_df) == 0) {
        admission_train_df <- temp_train_df
      } else {
        admission_train_df <- data.table::rbindlist(list(admission_train_df, temp_train_df),
                                                    fill = T)
      }
    }
    admission_train_df[is.na(admission_train_df)] <- 0

    admission_test_df <- data.frame()
    for(i in 1:length(test_files)) {
      temp_test_df <- readRDS(test_files[i])
      temp_test_df$client <- client_codes[i]
      if(nrow(admission_test_df) == 0) {
        admission_test_df <- temp_test_df
      } else {
        admission_test_df <- data.table::rbindlist(list(admission_test_df,
                                                        temp_test_df), fill = T)
      }
    }
    admission_test_df[is.na(admission_test_df)] <- 0

    saveRDS(admission_train_df,
            file.path(intermediate_dir,
                      "global_model_data_with_claims_pharma_dem_train.rds"))
    saveRDS(admission_test_df,
            file.path(intermediate_dir,
                      "global_model_data_with_claims_pharma_dem_test.rds"))

    admission_train_df$readmission <- factor(admission_train_df$readmission,
                                             levels = c(0, 1))
    admission_test_df$readmission <- factor(admission_test_df$readmission,
                                            levels = c(0, 1))



    output <-
      do_feature_selection_by_loglikelihood_new(intermediate_dir,
                                                pca_ref_file,
                                                "global_model_data_with_claims_pharma_dem_train.rds",
                                                "LL_OUTPUT_GLOBAL_PCA")
    ll_variables <- output$variables[output$threshold_v == 1]
    pca_v2_ref_file <- readr::read_csv(file.path(intermediate_dir, pca_ref_file))
    cols_to_not_consider <- EviveUtil::remove_na(pca_v2_ref_file$remove)

    cols_to_consider <- ll_variables
    pca_output <- prcomp(global_train_data[, cols_to_consider])

    saveRDS(pca_output, file.path(output_dir, "pca_output.rds"))
    select_pcs <- get_pcs(pca_output, output_dir)
    pca_selected <- pca_output$x[1:select_pcs]
    admission_train_df_pca <- cbind(admission_train_df[, cols_to_not_consider],
                                    pca_selected)

    admission_test_df_pca <- scale(admission_test_df[, cols_to_consider],
                                   pca_output$center,
                                   pca_output$scale) %*% pca_output$rotation
    admission_test_df_pca <- as.data.frame(admission_test_df_pca)
    admission_test_df_pca <- cbind(admission_test_df_pca[, cols_to_not_consider],
                                   admission_test_df_pca[, 1:select_pcs])


    EviveAutoEncoder::build_glmnet_model(
      train_df = admission_train_df_pca,
      test_df = admission_test_df_pca,
      data_vars = setdiff(colnames(admission_train_df_pca), c("upin", "admitting_diagnosis",
                                                              "admitting_diagnosis_bs",
                                                              "admitting_diagnosis_ccs")),
      output_name = "global_model_with_pca_all_variables",
      outcome = "readmission",
      threshold = 0.1,
      data_dir = output_dir
    )

  }