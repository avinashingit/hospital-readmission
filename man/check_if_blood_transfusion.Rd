% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{check_if_blood_transfusion}
\alias{check_if_blood_transfusion}
\title{Check if blood transfusion is done}
\usage{
check_if_blood_transfusion(admission_df, inpatient_claims_df,
  blood_transfusion_codes)
}
\arguments{
\item{admission_df}{the admission df}

\item{inpatient_claims_df}{the claims related to admission}

\item{blood_transfusion_codes}{the cpt codes of blood transfusion}
}
\value{
This function returns the data frame with a blood transfusion boolean
column
}
\description{
A function to check if blood transfusion is done during the admission.
}
\examples{
check_if_blood_transfusion(admission_df, claims_df, "99258")
}
