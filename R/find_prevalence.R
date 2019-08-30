find_prevlance_acc_to_length_of_stay <- function(data_dir,
                                                 intermediate_dir,
                                                 claims_file,
                                                 properties_file,
                                                 cpt_codes_file) {
  props_df <- get_properties_data(data_dir, properties_file)
  cpt_codes_df <- process_cpt_codes(data_dir, cpt_codes_file)
  claims_df <- process_claims(data_dir, claims_file, props_df, cpt_codes_df)
  claims_df <- readRDS("/home/avinash/R_Code_Exec/Hospital_Readmission/DeanFoods/intermediate_file_dir/claims_df.rds")
  inpatient_claims_df <- get_cptwise_claims(claims_df,
                                            EviveUtil::remove_na(
                                              cpt_codes_df$inpatient))
  complete_claims_df <-
    process_claims(data_dir, claims_file, props_df, filter_criteria)
  saveRDS(complete_claims_df,
          file.path(intermediate_dir, "complete_claims.rds"))
  lowerbounds <- c(0, 30, 60)
  upperbounds <- c(30, 60, 100)
  for(threshold in c(1, 2, 5, 8, 10)) {
    print(threshold)
    for(i in seq_len(length(lowerbounds))) {
      inpatient_claims_readmission <-
        get_readmissions(inpatient_claims_df, complete_claims_df,
                         threshold, lowerbounds[i], upperbounds[i])
      print(lowerbounds[i])
      print(upperbounds[i])
      print(dim(inpatient_claims_readmission))
      print(table(inpatient_claims_readmission$readmission))
      saveRDS(inpatient_claims_readmission, file.path(intermediate_dir,
                                                      paste0(threshold,"_", lowerbounds[i], "_", upperbounds[i],
                                                             "_inpatient_claims_readmission.rds")))
    }

  }

}