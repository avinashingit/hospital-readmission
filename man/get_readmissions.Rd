% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{get_readmissions}
\alias{get_readmissions}
\title{Get readmissions}
\usage{
get_readmissions(data_df, complete_claims_df, threshold = 10,
  lowerbound = 0, upperbound = 30)
}
\arguments{
\item{data_df}{the dataframe which consists of admissions.}
}
\value{
data frame with new column as readmission (TRUE/FALSE)
}
\description{
This function is used to find if readmissions are there for admissions.
}
\examples{
get_readmissions(admission_df)
}
