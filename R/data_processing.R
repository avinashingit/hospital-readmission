
# Processing external files ---------------------------------------------


#' get_properties_data
#'
#' This function reads the properties file and returns the data frame
#'
#' @param data_dir the path to the directory in which the file exists
#' @param properties_file the name of the properties file
#'
#' @return the properties file as a data frame
#' @export
#'
#' @examples
#' get_properties_data("/home/avinash", "properties.csv")
#'
#' get_properties_data("/home/avinash", "properties.rds")
#'
#' get_properties_data("/home/avinash", "properties.tsv")
get_properties_data <- function(data_dir, properties_file) {
  properties_df <- EviveUtil::read_data(data_dir, properties_file, getwd())
  return(properties_df)
}

#' process_cpt_codes
#'
#' This function reads the cpt codes from the file and returns it as a dataframe
#'
#' @param data_dir the path of the directory in which the file exists
#' @param cpt_codes_file the name of the cpt codes file
#'
#' @return - the cpt codes file as a data frame
#' @export
#'
#' @examples
#' process_cpt_codes("/home/avinash", "cpt_codes.csv")
process_cpt_codes <- function(data_dir, cpt_codes_file) {
  cpt_codes_df <- EviveUtil::read_data(data_dir, cpt_codes_file, getwd())
  return(cpt_codes_df)
}


# Processing demographics -------------------------------------------------


#' process_elig
#'
#' This function reads the eligibility file and returns the dataframe
#'
#' @param data_dir the path to the directory in which the file exists
#' @param elig_file the name of the eligibility file
#' @param props_df the properties data frame
#'
#' @return the eligibility file as a data frame
#' @export
#'
#' @examples
#' process_elig("/home/avinash", "eligibility.csv", properties_df)
process_elig <- function(data_dir, elig_file, props_df) {
  elig_df <- EviveUtil:::read_data(data_dir, elig_file, getwd())
  colnames(elig_df)[1] <- "UPIN"
  colnames(elig_df) <- tolower(colnames(elig_df))
  cols_v <- EviveUtil::remove_na(props_df$elig_cols)
  cols_v <- tolower(cols_v)
  elig_df$age <- EviveUtil::get_age_groups(elig_df$age)
  return(elig_df[, c(cols_v)])
}



#' process_census
#'
#' This function reads the census file and returns the dataframe
#'
#' @param data_dir the path to the directory in which the file exists
#' @param elig_file the name of the census file
#' @param props_df the properties data frame
#'
#' @return the census file as a data frame
#' @export
#'
#' @examples
#' process_census("/home/avinash", "census.csv", properties_df)
process_census <- function(data_dir, census_file, props_df, elig_df) {
  census_df <- EviveUtil:::read_data(data_dir, census_file, getwd())
  colnames(census_df)[1] <- "UPIN"
  colnames(census_df) <- tolower(colnames(census_df))
  census_df <- merge(census_df, elig_df, by = "upin")
  cols_v <- EviveUtil::remove_na(props_df$census_cols)
  cols_v <- tolower(cols_v)
  return(census_df[, cols_v])
}


#' process_census_pca
#'
#' The function reads the census pca file and returs it as a data frame
#'
#' @param data_dir the path to the directory in which the file is present
#' @param census_pca_file the name of the census pca file
#'
#' @return the census pca file as a dataframe
#' @export
#'
#' @examples
#' process_census_pca("/home/avinash", "census_pca.csv")
process_census_pca <- function(data_dir, census_pca_file) {
  census_pca_df <- EviveUtil::read_data(data_dir, census_pca_file, getwd())
  return(census_pca_df)
}



# Processing Medical Data -------------------------------------------------


#' process_claims
#'
#' The function reads claims data using read_claims_data in EviveUtil and
#' returns them as a dataframe
#'
#' @param data_dir the path to the directory in which the claims are present
#' @param claims_file the pattern of the claims file
#' @param props_df the properties df
#' @param filter_criteria the filter criteria based on which the claims will be
#' filtered
#'
#' @return the claims file filtered by cpt codes as a dataframe
#' @import lazyeval
#' @import EviveUtil
#' @export
#'
#' @examples
#' process_claims("/home/avinash", "_claims.csv", props_df, cpt_codes_df)
process_claims <- function(data_dir, claims_file, props_df,
                           filter_criteria = NULL) {
  claims_cols_v <- EviveUtil::remove_na(props_df$claims_cols)
  claims_df <- EviveUtil::read_claims_data(data_dir = data_dir,
                                           claims_file_pattern = claims_file,
                                           output_dir = getwd(),
                                           filter_criteria = filter_criteria,
                                           select_cols = claims_cols_v,
                                           rows_to_read = 2000000)
  claims_df$ServiceStartDate <- as.Date(claims_df$ServiceStartDate)
  claims_df$ServiceEndDate <- as.Date(claims_df$ServiceEndDate)
  return(unique(claims_df))
}

#' process_pharma_file
#'
#' The function reads the pharmacy data file and returns it as a dataframe
#'
#' @param data_dir the path to the directory in which the file is present
#' @param pharma_file the name of the pharmacy data file
#'
#' @return the pharmacy file as a dataframe
#' @export
#'
#' @examples
#' process_pharma_file("/home/avinash", "pharma.csv")
process_pharma_file <- function(data_dir, pharma_file) {
  pharma_df <- EviveUtil::read_data(data_dir = data_dir, pharma_file, getwd())
  colnames(pharma_df) <- tolower(colnames(pharma_df))
  pharma_df$servicedate <- as.Date(pharma_df$servicedate)
  pharma_df$servicedate2 <- pharma_df$servicedate
  pharma_df <- na.omit(pharma_df)
  return(pharma_df)
}



