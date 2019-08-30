
# DeanFoods parameters ----------------------------------------------------

# Directories
data_dir <-
  "/home/avinash/R_Code_Exec/Hospital_Readmission/DeanFoods/data_dir"

# Files
properties_file <- "readmission_properties.csv"
eligibility_file <- "1011_elig_2017_06_19.rds"
census_file <- "may_census.csv"
census_pca_file <- "census_pca.csv"
ccs_cpt_file <- "ccs_cpt.rds"
ccs_bodysystem_file <- "chronic_body_reference_icd.rds"
ccs_icd_file <- "ccs_icd_9_10.rds"
claims_file <- "deanfoods_claims_may_05.csv"
cpt_codes_file <- "cpt_codes.csv"
pharma_file <- "df_gpi_2.rds"
threshold <- 10
lowerbound <- 0
upperbound <- 30

output_dir <-
  paste0("/home/avinash/R_Code_Exec/Hospital_Readmission/DeanFoods/output_dir/", threshold,
         "_",lowerbound,".", upperbound)
intermediate_dir <-
  paste0("/home/avinash/R_Code_Exec/Hospital_Readmission/DeanFoods/intermediate_file_dir/", threshold,
"_",lowerbound,".", upperbound)


#
# # THD Parameters ----------------------------------------------------------
#
# Directories
data_dir <-
  "/home/avinash/R_Code_Exec/Hospital_Readmission/THD/data_dir"
# Files
properties_file <- "readmission_properties.csv"
eligibility_file <- "1004_elig_2017_06_19.rds"
census_file <- "may_census.csv"
census_pca_file <- "census_pca.csv"
ccs_cpt_file <- "ccs_cpt.rds"
ccs_bodysystem_file <- "chronic_body_reference_icd.rds"
ccs_icd_file <- "ccs_icd_9_10.rds"
claims_file <- "thd_med_may.csv"
cpt_codes_file <- "cpt_codes.csv"
pharma_file <- "gpi_level_2_thd.rds"
threshold <- 10
lowerbound <- 0
upperbound <- 30

output_dir <-
  paste0("/home/avinash/R_Code_Exec/Hospital_Readmission/THD/output_dir/", threshold,
         "_",lowerbound,".", upperbound)
intermediate_dir <-
  paste0("/home/avinash/R_Code_Exec/Hospital_Readmission/THD/intermediate_file_dir/", threshold,
         "_",lowerbound,".", upperbound)

#
#
# # Pepsico Parameters ------------------------------------------------------
#
# # Directories
data_dir <-
  "/home/avinash/R_Code_Exec/Hospital_Readmission/Pepsico/data_dir"
output_dir <-
  "/home/avinash/R_Code_Exec/Hospital_Readmission/Pepsico/output_dir/10_0.30"
intermediate_dir <-
  "/home/avinash/R_Code_Exec/Hospital_Readmission/Pepsico/intermediate_file_dir/10_0.30"

# Files
properties_file <- "readmission_properties.csv"
eligibility_file <- "pepsico_elig.rds"
census_file <- "may_census.csv"
census_pca_file <- "census_pca.csv"
ccs_cpt <- "ccs_cpt.rds"
ccs_icd <- "ccs_icd_9_10.rds"
claims_file <- "pepsi_claims_all.csv"
cpt_codes_file <- "cpt_codes.csv"
threshold <- 10
lowerbound <- 0
upperbound <- 30
#
#
# # American Airlines Parameters --------------------------------------------
#
data_dir <-
  "/home/avinash/R_Code_Exec/Hospital_Readmission/AA/data_dir"
# Files
properties_file <- "readmission_properties.csv"
eligibility_file <- "1013_elig_2017_06_19.rds"
census_file <- "may_census.csv"
census_pca_file <- "census_pca.csv"
ccs_cpt_file <- "ccs_cpt.rds"
ccs_bodysystem_file <- "chronic_body_reference_icd.rds"
ccs_icd_file <- "ccs_icd_9_10.rds"
claims_file <- "american_air_claims_may_05.csv"
cpt_codes_file <- "cpt_codes.csv"
pharma_file <- "aa_gpi2.rds"
threshold <- 10
lowerbound <- 0
upperbound <- 30

output_dir <-
  paste0("/home/avinash/R_Code_Exec/Hospital_Readmission/AA/output_dir/", threshold,
         "_",lowerbound,".", upperbound, "_v2")
intermediate_dir <-
  paste0("/home/avinash/R_Code_Exec/Hospital_Readmission/AA/intermediate_file_dir/", threshold,
         "_",lowerbound,".", upperbound, "_v2")

#

global_directory <- "/home/avinash/R_Code_Exec/Hospital_Readmission/Global/intermediate_file_dir"
global_output_dir <- "/home/avinash/R_Code_Exec/Hospital_Readmission/Global/output_dir"

#
#
# get_missing_cols <- function(df) {
#   gpi_cols <- c('gpi_2gpi_00','gpi_2gpi_01','gpi_2gpi_02','gpi_2gpi_03','gpi_2gpi_04','gpi_2gpi_05','gpi_2gpi_06','gpi_2gpi_07','gpi_2gpi_08','gpi_2gpi_09','gpi_2gpi_10','gpi_2gpi_11','gpi_2gpi_12','gpi_2gpi_13','gpi_2gpi_14','gpi_2gpi_15','gpi_2gpi_16','gpi_2gpi_17','gpi_2gpi_18','gpi_2gpi_19','gpi_2gpi_20','gpi_2gpi_21','gpi_2gpi_22','gpi_2gpi_23','gpi_2gpi_24','gpi_2gpi_25','gpi_2gpi_26','gpi_2gpi_27','gpi_2gpi_28','gpi_2gpi_29','gpi_2gpi_30','gpi_2gpi_31','gpi_2gpi_32','gpi_2gpi_33','gpi_2gpi_34','gpi_2gpi_35','gpi_2gpi_36','gpi_2gpi_37','gpi_2gpi_38','gpi_2gpi_39','gpi_2gpi_40','gpi_2gpi_41','gpi_2gpi_42','gpi_2gpi_43','gpi_2gpi_44','gpi_2gpi_45','gpi_2gpi_46','gpi_2gpi_47','gpi_2gpi_48','gpi_2gpi_49','gpi_2gpi_50','gpi_2gpi_51','gpi_2gpi_52','gpi_2gpi_53','gpi_2gpi_54','gpi_2gpi_55','gpi_2gpi_56','gpi_2gpi_57','gpi_2gpi_58','gpi_2gpi_59','gpi_2gpi_60','gpi_2gpi_61','gpi_2gpi_62','gpi_2gpi_63','gpi_2gpi_64','gpi_2gpi_65','gpi_2gpi_66','gpi_2gpi_67','gpi_2gpi_68','gpi_2gpi_69','gpi_2gpi_70','gpi_2gpi_71','gpi_2gpi_72','gpi_2gpi_73','gpi_2gpi_74','gpi_2gpi_75','gpi_2gpi_76','gpi_2gpi_77','gpi_2gpi_78','gpi_2gpi_79','gpi_2gpi_80','gpi_2gpi_81','gpi_2gpi_82','gpi_2gpi_83','gpi_2gpi_84','gpi_2gpi_85','gpi_2gpi_86','gpi_2gpi_87','gpi_2gpi_88','gpi_2gpi_89','gpi_2gpi_90','gpi_2gpi_91','gpi_2gpi_92','gpi_2gpi_93','gpi_2gpi_94','gpi_2gpi_95','gpi_2gpi_96','gpi_2gpi_97','gpi_2gpi_98','gpi_2gpi_99')
#   missing_cols <- setdiff(gpi_cols, colnames(df))
#   for(col in missing_cols) {
#     df[, col] <- 0
#   }
#   return(df)
# }