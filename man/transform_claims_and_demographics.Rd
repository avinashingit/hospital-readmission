% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_data.R
\name{transform_claims_and_demographics}
\alias{transform_claims_and_demographics}
\title{Transform claims and demographics}
\usage{
transform_claims_and_demographics(data_dir, intermediate_dir, claims_file,
  cpt_codes_file, ccs_cpt_file, ccs_bodysystem_file, ccs_icd_file,
  eligibility_file, census_file, properties_file, threshold, lowerbound,
  upperbound)
}
\arguments{
\item{data_dir}{the path to the directory in which the files are present}

\item{intermediate_dir}{the path to the dir to store intermediate files}

\item{claims_file}{the pattern of claims file}

\item{cpt_codes_file}{the name of cpt codes file}

\item{ccs_cpt_file}{the name of the ccs cpt file}

\item{ccs_bodysystem_file}{the name of the icd to bodysystem file}

\item{ccs_icd_file}{the name of the ccs icd file}

\item{eligibility_file}{the name of eligibility file}

\item{census_file}{the name of census file}

\item{properties_file}{the name of properties_file}

\item{threshold}{the threshold for grouping claims}

\item{lowerbound}{the lower bound of readmisison interval}

\item{upperbound}{the upper bound of readmission interval}
}
\value{
data frame with admissions and demographics
}
\description{
Transform claims and demographics
}
\examples{
transform_claims_and_demographics("/home/avinash", "/intermediate",
"claims.csv", "cpt_codes_file.csv", "elig.rds", "census.csv", "props.csv")
}
