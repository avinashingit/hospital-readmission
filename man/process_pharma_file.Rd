% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{process_pharma_file}
\alias{process_pharma_file}
\title{process_pharma_file}
\usage{
process_pharma_file(data_dir, pharma_file)
}
\arguments{
\item{data_dir}{the path to the directory in which the file is present}

\item{pharma_file}{the name of the pharmacy data file}
}
\value{
the pharmacy file as a dataframe
}
\description{
The function reads the pharmacy data file and returns it as a dataframe
}
\examples{
process_pharma_file("/home/avinash", "pharma.csv")
}
