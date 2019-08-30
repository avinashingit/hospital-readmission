#' Title
#'
#' @param data_dir
#' @param intermediate_dir
#' @param output_dir
#' @param properties_file
#' @param claims_file
#' @param cpt_codes_file
#' @param threshold
#' @param lowerbound
#' @param upperbound
#'
#' @return
#' @export
#'
#' @examples
calculate_cost_in_each_admission <- function(data_dir,
                                             intermediate_dir,
                                             output_dir,
                                             properties_file,
                                             claims_file,
                                             cpt_codes_file,
                                             threshold,
                                             lowerbound,
                                             upperbound) {
  loginfo("Reading Properties file")
  props_df <- get_properties_data(data_dir, properties_file)
  loginfo("Properties file is read and its dimensions are %s", dim(props_df))

  loginfo("Reading cpt codes file")
  cpt_codes_df <- process_cpt_codes(data_dir, cpt_codes_file)
  loginfo("The dimension of the cpt codes file are %s", dim(cpt_codes_df))

  loginfo("Processing claims file")
  filter_criteria <-
    lazyeval::interp(~CPTCode %in% c(EviveUtil::remove_na(cpt_codes_df$inpatient),
                                     EviveUtil::remove_na(cpt_codes_df$emergency),
                                     EviveUtil::remove_na(cpt_codes_df$blood_transfusion)))
  claims_df <- process_claims(data_dir, claims_file, props_df, filter_criteria)
  loginfo("The claims file is read and its dimensions are %s", dim(claims_df))

  loginfo("Getting inpatient claims")
  inpatient_claims_df <- get_cptwise_claims(claims_df,
                                            EviveUtil::remove_na(cpt_codes_df$inpatient))

  loginfo("Getting complete claims for UPINs who are admitted")
  filter_criteria <-
    lazyeval::interp(~UPIN %in% EviveUtil::remove_na(unique(inpatient_claims_df$UPIN)))
  complete_claims_df <- process_claims(data_dir, claims_file, props_df, filter_criteria)
  saveRDS(complete_claims_df, file.path(intermediate_dir, "complete_claims_with_costs.rds"))
  loginfo("The claims of person who are admitted are read and its dimentions are %s",
          dim(complete_claims_df))

  data_df <- group_claims(inpatient_claims_df, threshold)
  length_of_stay_df <- get_length_of_stay(data_df)
  readmission_data <- length_of_stay_df %>%
    group_by(UPIN) %>%
    arrange(min_ssd) %>%
    do(get_readmission_info(., lowerbound, upperbound))
  costs_df <- calculate_admission_and_readmission_cost(readmission_data,
                                                       complete_claims_df)
  saveRDS(costs_df, file.path(intermediate_dir, "admission_and_readmission_costs.rds"))
}

#' Title
#'
#' @param data_df
#' @param amounts_df
#'
#' @return
#' @export
#'
#' @examples
calculate_admission_and_readmission_cost <- function(data_df, amounts_df) {
  data_df$admission_amount <- data_df$readmission_amount <- 0
  amounts_df$DeductibleAmount[amounts_df$DeductibleAmount == "NULL"] <- 0
  amounts_df$CoinsuranceAmount[amounts_df$CoinsuranceAmount == "NULL"] <- 0
  amounts_df$CopaymentAmount[amounts_df$CopaymentAmount == "NULL"] <- 0
  amounts_df$NetPayment[amounts_df$NetPayment == "NULL"] <- 0
  amounts_df$total_amount <- as.numeric(amounts_df$DeductibleAmount) +
    as.numeric(amounts_df$CoinsuranceAmount) +
                                 as.numeric(amounts_df$CopaymentAmount) +
    as.numeric(amounts_df$NetPayment)
  amounts_df <- amounts_df %>% filter(total_amount < 50000 & total_amount > -49999)
  for(i in 1:nrow(data_df)) {
    temp_claims_df <- amounts_df  %>%
      filter(UPIN == data_df$UPIN[i]) %>%
      filter(ServiceStartDate >= data_df$min_ssd[i] &
               ServiceEndDate <= data_df$max_ssd[i])
    data_df$admission_amount[i] <- sum(temp_claims_df$total_amount, na.rm = TRUE)
    if(data_df$readmission[i] != 0) {
      readmission_claims_df <- amounts_df %>%
        filter(UPIN == data_df$UPIN[i]) %>%
        filter(ServiceStartDate >= data_df$readmission_start_date[i] &
                 ServiceEndDate <= data_df$readmission_end_date[i])
      data_df$readmission_amount[i] <- sum(readmission_claims_df$total_amount,
                                           na.rm = TRUE)
    }
  }
  return(data_df)
}

#' Title
#'
#' @param data_df
#' @param lowerbound
#' @param upperbound
#'
#' @return
#' @export
#'
#' @examples
get_readmission_info <- function(data_df, lowerbound, upperbound) {
  data_df$readmission <- 0
  data_df$readmission_start_date <- as.Date("01-01-1970")
  data_df$readmission_end_date <- as.Date("01-01-1970")
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
        data_df$readmission_start_date[row1] <- as.Date(data_df$min_ssd[row2])
        data_df$readmission_end_date[row1] <- as.Date(data_df$max_ssd[row2])
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
