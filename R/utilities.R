# Admission related utilities ---------------------------------------------

#' Group consecutive claims
#'
#' The function groups claims based on their service start dates. Two claims are
#' grouped together if their service start date difference <= 10 days
#'
#' @param data_df the dataframe which is to be grouped
#' @param threshold the number of days before which a claims can be
#' considered as a part of the same admission
#' @param recent if TRUE, then as soon as a claim exceeds the threshold the
#' grouping stops
#'
#' @return - the grouped claims data
#' @export
#' @importFrom EviveUtil find_diff_dates
#' @importFrom data.table between
#'
#' @examples
#' group_consecutive_claims(claims_df, 15)
#'
#' group_consecutive_claims(claims_df, 20)
#'
#' group_consecutive_claims(claims_df, 2, TRUE)
group_consecutive_claims <- function(data_df, threshold, recent = FALSE) {
  i <- 1
  while (i < nrow(data_df)) {
    first_claim <- data_df[i,]
    second_claim <- data_df[i + 1,]
    new_claim <- first_claim
    min_diff <-
      EviveUtil::find_diff_dates(first_claim$max_ssd, second_claim$min_ssd)
    in_between_dates <- between(second_claim$min_ssd, first_claim$min_ssd,
                                first_claim$max_ssd) |
                        between(second_claim$max_ssd, first_claim$min_ssd,
                                first_claim$max_ssd)
    validity <- min_diff <= threshold | in_between_dates
    if(!recent) {
      if (validity) {
        new_claim <- get_new_claim(new_claim, first_claim, second_claim)
        data_df <- data_df[-(i + 1),]
        data_df[i,] <- new_claim
      } else {
        i <- i + 1
      }
    } else {
      if (validity) {
        new_claim <- get_new_claim(new_claim, first_claim, second_claim)
        data_df <- data_df[-(i + 1),]
        data_df[i,] <- new_claim
      } else {
        return(as.data.frame(data_df, row.names = NULL))
      }
    }
  }
  return(as.data.frame(data_df, row.names = NULL))
}

#' Get New Claim
#'
#' Given two claims this function will merge the two claims and get the min and max
#' service start date and return the new claim.
#'
#' @param new_claim The claim whose service start date and service end date need to be
#' changed
#' @param first_claim The first claim
#' @param second_claim The second claim
#'
#' @return New claim with changed service start and end dates
#' @export
#'
#' @examples
#' get_new_claim(new_claim, first_claim, second_claim)
get_new_claim <- function(new_claim, first_claim, second_claim) {
  new_claim$min_ssd = min(first_claim$min_ssd, second_claim$min_ssd)
  new_claim$max_ssd = max(first_claim$max_ssd, second_claim$max_ssd)
  return(new_claim)
}

#' Group claims
#'
#' This function is a wrapper to group consecutive claims at UPIN and admission
#' level.
#'
#' @param data_df  the data frame which is to be grouped
#'
#' @return the claims grouped at admission level.
#' @import dplyr
#' @export
#'
#' @examples
#' group_claims(claims_df)
group_claims <- function(data_df, threshold, recent = FALSE) {
  patient_admission_df <- data_df %>%
    group_by(UPIN, ClaimNumber) %>%
    summarise(min_ssd = min(ServiceStartDate),
              max_ssd = max(ServiceEndDate)) %>%
    arrange(min_ssd)
  grouped_claims <- patient_admission_df %>%
    group_by(UPIN) %>%
    do(group_consecutive_claims(., threshold, recent))
  return(as.data.frame(grouped_claims, row.names = NULL))
}


# Readmission related utilities -------------------------------------------


#' Check if readmission
#'
#' The function checks if there exists a readmission for any admission. If a
#' readmission exists that readmission is removed.
#'
#' @param data_df The data frame which consists of admissions at UPIN level
#' @param lowerbound
#' @param upperbound
#'
#' @return The data frame with a new column stating readmission or not.
#' @export
#'
#' @examples
#' check_if_readmssion(admission_df, 0, 30)
check_if_readmission <- function(data_df, lowerbound, upperbound) {
  data_df$readmission <- 0
  if (nrow(data_df) >= 2) {
    row_pairs <- t(combn(nrow(data_df), 2))
    row_pairs <- as.data.frame(row_pairs)
    i <- 1
    while (i <= nrow(row_pairs)) {
      row1 <- row_pairs[i, 1]
      row2 <- row_pairs[i, 2]
      date_diff <-
        difftime(data_df$min_ssd[row2], data_df$max_ssd[row1], units = "days") + 1
      if(date_diff < 0) {
        logerror("Date difference is less than 0. Something got screwed.")
        stop()
      }
      if (date_diff <= upperbound & date_diff > lowerbound) {
        data_df$readmission[row1] <- 1
        data_df <- data_df[-row2,]
        if (nrow(data_df) >= 2) {
          row_pairs <- t(combn(nrow(data_df), 2))
          row_pairs <- as.data.frame(row_pairs)
        } else {
          row_pairs <- data.frame()
        }
      } else {
        i <- i + 1
      }
    }
  }
  return(data_df)
}


#' get_length_of_stay
#'
#' Given an admission dataframe this function finds the length of stay at
#' hospital
#'
#' @param data_df - the data frame consisting of admissions
#'
#' @return - the dataframe with a new column length_of_stay
#' @export
#'
#' @examples
#' get_length_of_stay(admissions_df)
get_length_of_stay <- function(data_df) {
  data_df$length_of_stay <-
    difftime(as.Date(data_df$max_ssd),
             as.Date(data_df$min_ssd), units = "days") + 1
  data_df <- data_df %>%
    arrange(desc(length_of_stay))
  return(data_df)
}


#' Get readmissions
#'
#' This function is used to find if readmissions are there for admissions.
#'
#' @param data_df the dataframe which consists of admissions.
#'
#' @return data frame with new column as readmission (TRUE/FALSE)
#' @export
#'
#' @examples
#' get_readmissions(admission_df)
get_readmissions <- function(data_df, complete_claims_df,
                             threshold = 10, lowerbound = 0, upperbound = 30) {
  data_df <- group_claims(data_df, threshold)
  saveRDS(data_df, file.path(intermediate_dir, "group_claims.rds"))
  # data_with_add_claims_df <-
  #   consider_non_admission_claims(data_df, complete_claims_df, threshold)
  # saveRDS(data_with_add_claims_df, file.path(intermediate_dir, "data_with_add_claims_df.rds"))
  length_of_stay_df <- get_length_of_stay(data_df)
  claims_readmission <- length_of_stay_df %>%
    group_by(UPIN) %>%
    arrange(min_ssd) %>%
    do(check_if_readmission(., lowerbound, upperbound))
  claims_readmission <- as.data.frame(claims_readmission)
  return(claims_readmission)
}

#' Consider Non Admissions related claims
#'
#' This function groups the claims after the admission end date which do not have CPT
#' codes related to admission.
#'
#' @param data_df the inpatient claims data frame
#' @param complete_claims_df the complete claims data frame
#' @param threshold the threshold for grouping
#'
#' @return Admission claims including Non Admission related claims as a dataframe
#' @export
#'
#' @examples
#' consider_non_admission_claims(data_df, complete_claims_df, 10)
consider_non_admission_claims <- function(data_df, complete_claims_df,
                                          threshold) {
  data_df$maximum_date <- Sys.Date()
  setDT(data_df)
  setDT(complete_claims_df)
  setkey(data_df, UPIN, max_ssd, maximum_date)
  setkey(complete_claims_df, UPIN, ServiceStartDate, ServiceEndDate)
  admission_complete_claims_df <-
    foverlaps(data_df, complete_claims_df)
  admission_complete_claims_df$DiagnosisCode1 <-
    admission_complete_claims_df$DiagnosisCode2 <-
    admission_complete_claims_df$DiagnosisCode3 <-
    admission_complete_claims_df$DiagnosisCode4 <-
    admission_complete_claims_df$DiagnosisCode5 <-
    admission_complete_claims_df$i.ClaimNumber <-
    admission_complete_claims_df$min_ssd <-
    admission_complete_claims_df$max_ssd <-
    admission_complete_claims_df$maximum_date <-
    admission_complete_claims_df$CPTCode <- NULL
  admission_complete_claims_df <- unique(admission_complete_claims_df)
  data_df$maximum_date <- NULL
  admission_complete_claims_df$admission_related <- 0
  colnames(data_df) <- gsub("min_ssd", "ServiceStartDate", colnames(data_df))
  colnames(data_df) <- gsub("max_ssd", "ServiceEndDate", colnames(data_df))
  additional_claims_grouped_df <- data_df %>%
    group_by(UPIN, ClaimNumber, ServiceStartDate, ServiceEndDate) %>%
    do(group_additional_claims(., admission_complete_claims_df, threshold))
  return(additional_claims_grouped_df)
}

#' Group Additional Claims
#'
#' Group additional claims for each admission based on the claims dataframe that is passed
#'
#' @param data_df the already present admission dataframe
#' @param to_group_df the claims dataframe which should be grouped
#' @param threshold the threshold for grouping
#'
#' @return the dataframe after grouping additional claims with the admission dataframe
#' @export
#'
#' @examples
#' group_additional_claims(data_df, to_group_df, 10)
group_additional_claims <- function(data_df, to_group_df, threshold) {
  filtered_claims_df <- to_group_df %>%
    filter(UPIN == data_df$UPIN[1],
           ServiceStartDate >= max(data_df$ServiceEndDate))
  filtered_claims_grouped_df <- group_claims(filtered_claims_df, threshold)
  colnames(filtered_claims_grouped_df) <-
    gsub("min_ssd", "ServiceStartDate", colnames(filtered_claims_grouped_df))
  colnames(filtered_claims_grouped_df) <-
    gsub("max_ssd", "ServiceEndDate", colnames(filtered_claims_grouped_df))
  final_claims <- data.table::rbindlist(list(data_df, filtered_claims_grouped_df[1,]))
  final_claims_grouped <- group_claims(final_claims, threshold, TRUE)
  return(final_claims_grouped[1])
}

#' Get previous admissions for each admission
#'
#' This function gets the number of admissions before each admissions at UPIN
#' level.
#'
#' @param data_df the data frame which consists of admissions
#'
#' @return data frame with new column as the number of previous readmissions
#' @export
#'
#' @examples
#' get_previous_admissions_for_each_admission(admission_df)
get_previous_admissions_for_each_admission <- function(data_df) {
  data_df$previous_admissions <- 0:(nrow(data_df)-1)
  return(data_df)
}

#' Get previous admissions
#'
#' This function finds the number of previous admissions before each admission.
#'
#' @param data_df the data frame consisting of admissions
#'
#' @return the data frame with a column containing #previous admissions
#' @export
#'
#' @examples
#' get_previous_admission(admission_df)
get_previous_admissions <- function(data_df) {
  previous_admissions_df <- data_df %>%
    group_by(UPIN) %>%
    arrange(min_ssd) %>%
    do(get_previous_admissions_for_each_admission(.))
  return(previous_admissions_df)
}


# Emergency admission related utlities ------------------------------------


#' Get emergency claims
#'
#' A function to filter emergency claims to find emergency admissions.
#'
#' @param claims_df the claims data frame
#' @param cpt_codes_df the cpt codes data frame
#'
#' @return - the data frame containing emergency admissions
#' @export
#' @import dplyr
#'
#' @examples
#' get_cpt_claims_grouped(claims_df, cpt_codes_df)
get_cpt_claims_grouped <- function(claims_df, cpt_v, threshold = 10) {
  cpt_claims_df <- EviveUtil::get_cptwise_claims(claims_df,
                                            EviveUtil::remove_na(
                                              cpt_v))
  cpt_claims_df <- group_claims(cpt_claims_df, threshold)
  cpt_claims_df$ClaimNumber <- NULL
  cpt_claims_df <- as.data.frame(cpt_claims_df)
  return(cpt_claims_df)
}


#' Get previous emergency admissions
#'
#' A function that finds the number of emergency admissions before each
#' admission.
#'
#' @param inpatient_claims_readmission the dataframe containing the admissions
#' @param emergency_claims_df the claims related to emergency admissions
#'
#' @return a dataframe with the number of previous emergency admissions
#' @importFrom data.table setDT setkey foverlaps
#' @export
#'
#' @examples
#' get_previous_emergency_admissions(claims_df, emergency_df)
get_previous_emergency_admissions <- function(inpatient_claims_readmission,
                                              emergency_claims_df) {
  original_inpatient_claims <- inpatient_claims_readmission
  inpatient_claims_readmission$start_date <- as.Date("1970-01-01",
                                                     format = "%Y-%m-%d")
  setDT(inpatient_claims_readmission)
  setDT(emergency_claims_df)
  setkey(emergency_claims_df, UPIN, min_ssd, max_ssd)
  setkey(inpatient_claims_readmission, UPIN, start_date, min_ssd)
  emergency_inpatient_merged_df <-
    foverlaps(inpatient_claims_readmission, emergency_claims_df)
  rows_not_to_consider <- emergency_inpatient_merged_df$min_ssd ==
    emergency_inpatient_merged_df$i.min_ssd |
    emergency_inpatient_merged_df$max_ssd ==
    emergency_inpatient_merged_df$i.max_ssd
  emergency_inpatient_merged_df$min_ssd[rows_not_to_consider] <- NA
  emergency_inpatient_merged_df$max_ssd[rows_not_to_consider] <- NA
  emergency_admissions_count_df <- emergency_inpatient_merged_df %>%
    mutate(emergency_admission =
             ifelse(!is.na(min_ssd) & !is.na(max_ssd), 1, 0)) %>%
    group_by(UPIN, i.min_ssd, i.max_ssd) %>%
    summarise(previous_emergency_admissions = sum(emergency_admission))
  colnames(emergency_admissions_count_df) <- gsub(
    "^^i.", "", colnames(emergency_admissions_count_df))
  emergency_admissions_count_df <- merge(original_inpatient_claims,
                                         emergency_admissions_count_df,
                                         by = c("UPIN", "min_ssd", "max_ssd"))
  return(emergency_admissions_count_df)
}

# Comorbidities related utilities -----------------------------------------


#' Get comorbidities
#'
#' This function is used to find the comorbidities present during the time of
#' admissiong
#'
#' @param data_df the data frame containing te admissions and diagnosis codes
#' in each admission
#'
#' @return - the comorbidites during each admission
#' @import icd
#' @export
#'
#' @examples
#' get_comorbidities(admission_df)
get_comorbidities <- function(data_df, complete_claims_df) {
  claims_for_upin <- complete_claims_df[complete_claims_df$UPIN == data_df$UPIN, ]
  admission_claims <- claims_for_upin %>%
    filter(data_df$min_ssd <= ServiceStartDate &
             data_df$max_ssd >= ServiceEndDate)
  admission_claims_df <- merge(data_df, admission_claims, by = "UPIN")
  select_cols <- c("UPIN", "DiagnosisCode1", "DiagnosisCode2", "DiagnosisCode3",
                   "DiagnosisCode4", "DiagnosisCode5", "min_ssd", "max_ssd")
  admission_claims_df <- admission_claims_df[, select_cols]
  admission_claims_df <- tidyr::gather_(admission_claims_df,
                                        key_col = "DiagnosisCodeType",
                                        value = "DiagnosisCodes",
                                        gather_cols = c("DiagnosisCode1",
                                                        "DiagnosisCode2",
                                                        "DiagnosisCode3",
                                                        "DiagnosisCode4",
                                                        "DiagnosisCode5"))
  admission_claims_df$DiagnosisCodeType <- NULL
  admission_claims_df <- unique(admission_claims_df)
  admission_claims_df <- admission_claims_df %>%
    filter(DiagnosisCodes!="000.00" & !is.na(DiagnosisCodes) &
             !is.null(DiagnosisCodes))
  admission_claims_df$DiagnosisCodes <-
    icd::icd_decimal_to_short(admission_claims_df$DiagnosisCodes)
  admission_claims_df$sequence <- 1
  comorbidities_icd9 <- icd::icd9_comorbid(
    admission_claims_df[, c("sequence", "DiagnosisCodes")], icd::icd9_map_ahrq, short_code = T)
  comorbidities_icd10 <- icd::icd9_comorbid(
    admission_claims_df[, c("sequence", "DiagnosisCodes")], icd::icd10_map_ahrq, short_code = T)
  comorbidities <- comorbidities_icd9 | comorbidities_icd10
  admission_claims_df$sequence <- NULL
  admission_claims_df$DiagnosisCodes <- NULL
  admission_claims_df <- unique(admission_claims_df)
  admission_claims_df[, colnames(comorbidities)] <- comorbidities
  return(admission_claims_df)
}


#' Get ICD comorbidities
#'
#' Function to find the comorbidities present at the time of admission.
#'
#' @param admission_df the admission data frame
#' @param inpatient_claims_df the claims related to admissions
#'
#' @return data frame consisting of admissions and comorbidities
#' @importFrom data.table foverlaps setDT setkey
#' @import tidyr
#' @export
#'
#' @examples
#' get_icd_comorbidities(admission_df, claims_df)
get_icd_comorbidities <- function(admission_df, complete_claims_df) {

  original_admission_df <- admission_df
  admission_comorbidities_df <- admission_df %>%
    group_by(UPIN, min_ssd, max_ssd) %>%
    do(get_comorbidities(., complete_claims_df))
  admission_comorbidities_df <- merge(original_admission_df, admission_comorbidities_df,
                                      by = c("UPIN", "min_ssd", "max_ssd"))
  return(admission_comorbidities_df)
}


# Blood transfusion related utilities -------------------------------------


#' Check if blood transfusion is done
#'
#' A function to check if blood transfusion is done during the admission.
#'
#' @param admission_df the admission df
#' @param inpatient_claims_df the claims related to admission
#' @param blood_transfusion_codes the cpt codes of blood transfusion
#'
#' @return This function returns the data frame with a blood transfusion boolean
#' column
#' @import dplyr
#' @export
#'
#' @examples
#' check_if_blood_transfusion(admission_df, claims_df, "99258")
check_if_blood_transfusion <- function(admission_df, inpatient_claims_df,
                                       blood_transfusion_codes) {
  original_admission_df <- admission_df
  admission_df <- admission_df[, c("UPIN", "min_ssd", "max_ssd")]
  blood_transfusion_claims <- inpatient_claims_df %>%
    filter(inpatient_claims_df$CPTCode %in% blood_transfusion_codes)
  admission_df <- admission_df %>%
    group_by(UPIN, min_ssd, max_ssd) %>%
    do(find_blood_transfusion(., blood_transfusion_claims))
  admission_df <- merge(original_admission_df, admission_df,
                        by = c("UPIN", "min_ssd", "max_ssd"))
  return(admission_df)
}

#' Find blood transfusion
#'
#' @param data_df - the data frame with consists of admissions
#' @param blood_transfusion_claims - the data frame which consists of blood
#' transfusion claims
#'
#' @return This function returns the data frame with a blood transfusion boolean
#' column
#' @import dplyr
#' @export
#'
#' @examples
#' find_blood_transfusion(admissions_df, blood_transfusion_claims_df)
find_blood_transfusion <- function(data_df, blood_transfusion_claims) {
  claims_within_range <- blood_transfusion_claims %>%
    filter(blood_transfusion_claims$ServiceStartDate >= data_df$min_ssd &
             blood_transfusion_claims$ServiceStartDate <= data_df$max_ssd &
             UPIN == data_df$UPIN)
  if(nrow(claims_within_range) >= 1) {
    data_df$blood_transfusion <- 1
  } else {
    data_df$blood_transfusion <- 0
  }
  return(data_df)
}

# Pharmacy related utilities ----------------------------------------------

#' Get admission medications
#'
#' A function to find the medications a person is under during the admission
#'
#' @param admission_df the admission dataframe
#' @param pharma_df the pharmacy claims dataframe at gpi level
#'
#' @return the data frame containg admissions and medications
#' @importFrom data.table setDT setkey foverlaps
#' @export
#'
#' @examples
#' get_admission_medications(admission_df, pharma_df)
get_admission_medications <- function(admission_df, pharma_df) {
  setDT(admission_df)
  setDT(pharma_df)
  setkey(admission_df, UPIN, min_ssd, max_ssd)
  setkey(pharma_df, upin, servicedate, servicedate2)
  admission_pharma <- foverlaps(admission_df, pharma_df)
  admission_pharma$servicedate <- admission_pharma$servicedate2 <-
    admission_pharma$readmission_date <- NULL
  admission_pharma[is.na(admission_pharma)] <- 0
  admission_pharma <- data.frame(admission_pharma)
  gpi_cols <-
    colnames(admission_pharma)[grepl("^gpi", colnames(admission_pharma))]
  original_admission_pharma <-
    admission_pharma[, setdiff(colnames(admission_pharma), gpi_cols)]
  original_admission_pharma <- unique(original_admission_pharma)
  admission_pharma <-
    admission_pharma[, c("UPIN", "min_ssd", "max_ssd", gpi_cols)]
  admission_pharma[gpi_cols] <- sapply(admission_pharma[gpi_cols], as.character)
  admission_pharma[gpi_cols] <- sapply(admission_pharma[gpi_cols], as.numeric)
  admission_pharma <- admission_pharma %>%
    group_by(UPIN, min_ssd, max_ssd) %>%
    summarise_each(funs(max))
  admission_pharma[, gpi_cols][admission_pharma[, gpi_cols] >= 1] <- 1
  admission_pharma[, gpi_cols][admission_pharma[, gpi_cols] < 1] <- 0
  original_admission_pharma <-
    merge(original_admission_pharma, admission_pharma,
          by = c("UPIN", "min_ssd", "max_ssd"))
  return(original_admission_pharma)
}

#' Title
#'
#' @param data_dir
#' @param ccs_bodysystem_file
#'
#' @return
#' @export
#'
#' @examples
get_bodysystem_df <- function(data_dir, ccs_bodysystem_file) {
  bodysystem_df <- EviveUtil::read_data(data_dir, ccs_bodysystem_file, getwd())
  bodysystem_df[] <- lapply(bodysystem_df, as.character)
  bodysystem_df$BODYSYSTEM <- as.numeric(bodysystem_df$BODYSYSTEM)
  bodysystem_df <- rbind(bodysystem_df, list("", "","", 19))
  return(bodysystem_df)
}

#' Title
#'
#' @param data_dir
#' @param ccs_icd_file
#'
#' @return
#' @export
#'
#' @examples
get_ccs_disease_df <- function(data_dir, ccs_icd_file) {
  ccs_df <- EviveUtil::read_data(data_dir, ccs_icd_file, getwd())
  ccs_df[] <- lapply(ccs_df, as.character)
  ccs_df <- rbind(ccs_df, list("","Others"))
  return(ccs_df)
}

#' Get Admitting Diagnosis
#'
#' This function will get all the unique primary diagnoses at bodysystem level during the
#' admission
#'
#' @param inpatient_claims_readmission the admission dataframe
#' @param complete_claims_df the complete claims dataframe
#' @param ccs_icd_file the ccs to icd mapping file
#' @param ccs_bodysystem_file the ccs to bodysystem mapping file
#'
#' @return the admission data frame that consists primary diagnoses at bodysystem level
#' @export
#'
#' @examples
#' get_admitting_diagnosis(inpatient_claims_readmission, complete_claims_df, "ccs_bs.csv",
#' "ccs_icd.csv")
get_admitting_diagnosis <- function(inpatient_claims_readmission, complete_claims_df,
                                    ccs_bodysystem_file, ccs_icd_file, data_dir) {
  readmission_df <- inpatient_claims_readmission[, c("UPIN", "min_ssd", "max_ssd")]

  bodysystem_df <- get_bodysystem_df(data_dir, ccs_bodysystem_file)
  ccs_df <- get_ccs_disease_df(data_dir, ccs_icd_file)

  admitting_diagnosis_df <- readmission_df %>%
    group_by(UPIN, min_ssd, max_ssd) %>%
    do(find_admitting_diagnosis_admission_level(., complete_claims_df,
                                                bodysystem_df, ccs_df))

  admission_with_admit_diagnosis_df <- merge(inpatient_claims_readmission,
                                            admitting_diagnosis_df,
                                            by = c("UPIN", "min_ssd", "max_ssd"))
  return(admission_with_admit_diagnosis_df)
}

#' Find admitting diagnosis at admission level
#'
#' @param data_df the data frame which contains the admission
#' @param complete_claims_df the complete claims data frame
#' @param bodysystem_df the bodysystem data frame
#' @param ccs_df the CCS ICD dataframe
#'
#' @return the data frame with new columns for primary diagnoses at both bodysystem level
#' and CCS level
#' @export
#'
#' @examples
#' find_admittin_diagnosis_admission_level(data_df, complete_claims_df, bodysystem_df,
#' ccs_df)
find_admitting_diagnosis_admission_level <-
  function(data_df, complete_claims_df, bodysystem_df, ccs_df) {
    admission_claims <- complete_claims_df[complete_claims_df$UPIN == data_df$UPIN, ]
    admission_claims <- admission_claims %>%
      filter(ServiceStartDate >= min(data_df$min_ssd) &
               ServiceStartDate <= max(data_df$max_ssd)) %>%
      select(DiagnosisCode1)
  admission_claims$DiagnosisCode1 <- as.character(admission_claims$DiagnosisCode1)
  primary_diagnosis <- unique(icd::icd_decimal_to_short(admission_claims$DiagnosisCode1))
  primary_diagnosis_bs <- match(primary_diagnosis, bodysystem_df$DiagnosisCodes,
                                nomatch = nrow(bodysystem_df))
  primary_diagnosis_ccs <- match(primary_diagnosis, ccs_df$DiagnosisCodes,
                                 nomatch = nrow(ccs_df))
  for(i in seq_along(primary_diagnosis_ccs)) {
    if(primary_diagnosis_ccs[i] == nrow(ccs_df)) {
      diagnosis <- gsub("0$", " ", primary_diagnosis[i])
      ccs_diagnosis <- match(diagnosis, ccs_df$DiagnosisCodes,
                             nomatch = nrow(ccs_df))
      primary_diagnosis_ccs[i] <- ccs_diagnosis
    }
  }
  data_df$admitting_diagnosis <- paste(primary_diagnosis, collapse = "_")
  data_df$admitting_diagnosis_bs <-
    paste(sort(unique(bodysystem_df$BODYSYSTEM[primary_diagnosis_bs])), collapse = "_")
  data_df$admitting_diagnosis_ccs <-
    paste(sort(unique(ccs_df$DiseaseName[primary_diagnosis_ccs])), collapse = "_")
  return(data_df)
  }


#' Get Admission Procedures
#'
#' This function is used to find the procedures underwent by a patient during an admission
#'
#' @param admission_with_admit_diagnosis_df The dataframe with admissions
#' @param complete_claims_df the complete claims data frame
#' @param ccs_cpt_file the CCS CPT File
#'
#' @return the data frame with admission procedures at CCS CPT level
#' @export
#'
#' @examples
#' get_admission_procedures(admission_df, complete_claims_df, "ccs_cpt.csv")
get_admission_procedures <-
  function(admission_with_admit_diagnosis_df, complete_claims_df,
           ccs_cpt_file) {
    readmission_df <- admission_with_admit_diagnosis_df[, c("UPIN", "min_ssd", "max_ssd")]
    cpt_ccs_df <- EviveUtil::read_data(data_dir, ccs_cpt_file, getwd())
    ccs_cpt_for_admission <- cpt_ccs_df["CCS.Label"]
    ccs_cpt_for_admission <- unique(ccs_cpt_for_admission)
    ccs_cpt_for_admission$present <- 0
    ccs_cpt_flat <- tidyr::spread(ccs_cpt_for_admission, CCS.Label, present)
    readmission_df <- merge(readmission_df, ccs_cpt_flat)
    admission_procedures_df <- data.frame()
    admission_procedures_df <- readmission_df %>%
      group_by(UPIN, min_ssd, max_ssd) %>%
      do(get_procedures_at_admission_level(., complete_claims_df, cpt_ccs_df))
    admission_procedures_df <-
      merge(admission_with_admit_diagnosis_df, admission_procedures_df,
          by = c("UPIN", "min_ssd", "max_ssd"))
    admission_procedures_df$`Other diagnostic procedures (interview, evaluation, consultation)` <- NULL
    return(admission_procedures_df)
  }

#' Get Procedures at Admission level
#'
#' This function takes each admission and finds the procedures during the admission.
#'
#' @param data_df the data frame of the admission
#' @param complete_claims_df the complete claims dataframe
#' @param cpt_ccs_df the CPT CCS dataframe
#'
#' @return The admission dataframe with procedures during the admision.
#' @export
#'
#' @examples
#' get_procedures_at_admission_level(data_df, complete_claims_df, cpt_ccs_df)
get_procedures_at_admission_level <- function(data_df, complete_claims_df,
                                              cpt_ccs_df) {
  upin_claims <- complete_claims_df[complete_claims_df$UPIN == data_df$UPIN,]
  admission_claims <- upin_claims %>%
    filter(ServiceStartDate >= min(data_df$min_ssd) &
             ServiceStartDate <= max(data_df$max_ssd))%>%
    select(unique(CPTCode))
  admission_cpts <- merge(admission_claims, cpt_ccs_df, by = "CPTCode")
  admission_cpts$CCS <- admission_cpts$CPTCode <- NULL
  for(i in admission_cpts$CCS.Label) {
    data_df[, i] <- 1
  }
  return(data_df)
}



#' Get Previous hospital visits
#'
#' This function gets the number of previous hospital visits by a person before each
#' admission
#'
#' @param inpatient_claims_with_admit_procedures the inpatient claims df which consists of
#' admissions
#' @param initial_observation_claims the claims related to hospital visits.
#'
#' @return the admission dataframe with a new column as the number of previous hospital
#' visits
#' @export
#'
#' @examples
#' get_previous_hospital_visits(inpatient_claims_df, visit_claims_df)
get_previous_hospital_visits <- function(inpatient_claims_readmission,
                                         initial_observation_claims) {
  inpatient_claims_readmission$start_date <- as.Date("1970-01-01", format = "%Y-%m-%d")
  setDT(inpatient_claims_readmission)
  setDT(initial_observation_claims)
  setkey(initial_observation_claims, UPIN, min_ssd, max_ssd)
  setkey(inpatient_claims_readmission, UPIN, start_date, min_ssd)
  emergency_inpatient_merged_df <-
    foverlaps(inpatient_claims_readmission, initial_observation_claims)
  rows_not_to_consider <- emergency_inpatient_merged_df$min_ssd ==
    emergency_inpatient_merged_df$i.min_ssd |
    emergency_inpatient_merged_df$max_ssd ==
    emergency_inpatient_merged_df$i.max_ssd
  emergency_inpatient_merged_df$min_ssd[rows_not_to_consider] <- NA
  emergency_inpatient_merged_df$max_ssd[rows_not_to_consider] <- NA
  emergency_admissions_count_df <- emergency_inpatient_merged_df %>%
    mutate(emergency_admission = ifelse(!is.na(min_ssd) & !is.na(max_ssd), 1, 0)) %>%
    group_by(UPIN, i.min_ssd, i.max_ssd, length_of_stay,
             readmission, readmission_date, previous_admissions) %>%
    summarise(previous_hospital_visits = sum(emergency_admission))
  colnames(emergency_admissions_count_df) <-
    gsub("^^i.", "", colnames(emergency_admissions_count_df))
  return(as.data.frame(emergency_admissions_count_df))
}



#' Title
#'
#' @param directory
#' @param ref_file
#' @param file_name
#' @param output_name
#'
#' @return
#' @export
#'
#' @examples
do_feature_selection_by_loglikelihood <- function(directory, ref_file, file_name,
                                                  output_name) {
  # Do feature selection to find out important variables
  variable_selection_by_loglikelihood(
    directory,
    ref_file,
    paste0(directory, "/"),
    file_name,
    output_name
  )
  loglikelihood_output <- EviveUtil::read_data(intermediate_dir,
                                               output_name,
                                               getwd())
  important_variables <-
    loglikelihood_output$variables[loglikelihood_output$threshold_v == 1]
  return(important_variables)
}


#' Title
#'
#' @param data_df
#' @param data_vars
#' @param outcome
#' @param data_dir
#' @param output_pattern
#'
#' @return
#' @export
#'
#' @examples
do_feature_selection_by_random_forest <- function(data_df, data_vars, outcome, data_dir,
                                                  output_pattern) {
  feature_selection_rf(data_df, data_vars, outcome, data_dir, output_pattern)
  important_variables_acc <- read.csv(file.path(data_dir,
                                            paste0(output_pattern, "_rf_var_selection_acc.csv")),
                                  stringsAsFactors = F)
  important_variables_gini <- read.csv(file.path(data_dir,
                                            paste0(output_pattern, "_rf_var_selection_gini.csv")),
                                  stringsAsFactors = F)
  return(c(important_variables_acc$accuracy_cols, important_variables_gini$gini_cols))
}


#' Title
#'
#' @param data_df
#' @param output_name
#' @param output_fs_name
#' @param loglikelihood_name
#' @param data_file_name
#' @param train_index
#' @param intermediate_dir
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples
build_glmnet_model <- function(data_df, output_name, output_fs_name, loglikelihood_name,
                               data_file_name, train_index, intermediate_dir, output_dir) {
  loginfo("Building glmnet model")
  EviveAutoEncoder::build_glmnet_model(
    data_df[train_index, ],
    data_df[-train_index, ],
    data_vars = setdiff(colnames(data_df), c("upin", "readmission", "admitting_diagnosis",
                                             "admitting_diagnosis_bs", "admitting_diagnosis_ccs")),
    outcome = "readmission",
    threshold = 0.1,
    output_name = output_name,
    data_dir = output_dir
  )
  loginfo("Modeling process ended")

  ref_df <- EviveUtil::read_data(intermediate_dir, "ref_file.csv", getwd())

  important_variables <-
    do_feature_selection_by_loglikelihood(intermediate_dir,
                                          "ref_file.csv",
                                          data_file_name,
                                          loglikelihood_name)

  data_vars <- setdiff(colnames(data_df), ref_df$remove)
  rf_imp_variables <- do_feature_selection_by_random_forest(data_df, data_vars,
                                                            "readmission",
                                                            intermediate_dir,
                                                            "rf_feature_selection")

  important_variables <- unique(c(important_variables, rf_imp_variables))
  loginfo("The important variables are %s", rf_imp_variables)
  saveRDS(important_variables, file.path(intermediate_dir, "important_variables.rds"))

  x <- setdiff(colnames(data_df), important_variables)
  y <- setdiff(x, ref_df$remove)
  data_vars <- setdiff(colnames(data_df), c(y, c("upin", "readmission","admitting_diagnosis",
                                                 "admitting_diagnosis_bs", "admitting_diagnosis_ccs")))

  # data_vars <- setdiff(c(ref_df$remove, important_variables), "upin")

  loginfo("Building glmnet model with feature selection")
  EviveAutoEncoder::build_glmnet_model(
    data_df[train_index, ],
    data_df[-train_index, ],
    data_vars = data_vars,
    outcome = "readmission",
    threshold = 0.1,
    output_name = output_fs_name,
    data_dir = output_dir
  )
  loginfo("Modeling process with feature selection ended")
}


#' Create dummy columns from a column which consists of strings separated by a character
#'
#' @param data_df the dataframe whose columns are to be spread
#' @param column_name tbe column name whose column is to be spread
#' @param separator the separator by which the column is concatenated
#' @param type the type of the variables which are concatenated to string
#' @param fill value to be filled incase the value is missing in the column
#' @param remove_original remove the original column or not
#'
#' @return the dataframe with a set of new columns based on the spread
#' @export
#'
#' @examples
#' create_dummy_columns(admission_df, "admitting_diagnosis_bs", "_", "numeric", 0, TRUE)
create_dummy_columns <- function(data_df, column_name, separator, type, fill,
                                 remove_original = TRUE) {
  data_dummy_df <-
    splitstackshape::cSplit_e(data_df, column_name, separator, type = type, fill = fill)
  if(remove_original)
    return(data_dummy_df[, -c(column_name)])
  else
    return(data_dummy_df)
}



#' Title
#'
#' @param directory
#' @param ref_file
#' @param file_name
#' @param output_name
#'
#' @return
#' @export
#'
#' @examples
do_feature_selection_by_loglikelihood_new <- function(directory, ref_file, file_name,
                                                  output_name) {
  # Do feature selection to find out important variables
  variable_selection_by_loglikelihood(
    directory,
    ref_file,
    paste0(directory, "/"),
    file_name,
    output_name
  )
  loglikelihood_output <- EviveUtil::read_data(directory,
                                               output_name,
                                               getwd())
  return(loglikelihood_output)
}


#' Get Number of Principal Components for 90% variance
#'
#' @param pca_output The output of PCA
#' @param output_dir the output dir to which the scree plots need to be stored
#'
#' @return the number of principal components to be considered
#' @export
#'
#' @examples
#' get_pcs(pca_output, "/home/avinash/output_dir", 0.85)
#'
#' get_pcs(pca_output, "/home/avinash/output_dir")
get_pcs <- function(pca_output, output_dir, variance = 0.90) {
  std_dev <- pca_output$sdev
  pr_var <- std_dev^2
  prop_varex <- pr_var/sum(pr_var)
  pdf(file = file.path(output_dir, "PCA_SCREE_PLOTS.pdf"))
  plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
  plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  dev.off()
  select_pcs <- match(x = variance, round(cumsum(prop_varex),2))
  return(select_pcs)
}