% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_fit_funs.R
\name{get_clean_exposures}
\alias{get_clean_exposures}
\title{Create cleaned, binary response frame for contx_type}
\usage{
get_clean_exposures(dat, taxa_names)
}
\arguments{
\item{dat}{raw data frame, prefereably passed through select_country_dat}

\item{taxa_names}{character vector of taxa names}
}
\value{
wide data frame with binary exposures of interest
}
\description{
Create cleaned, binary response frame for contx_type
}
\author{
matteo-V
}
