% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_manip_funs.R
\name{annotate_factor_with_n}
\alias{annotate_factor_with_n}
\title{Annotate factor labels with number of observations}
\usage{
annotate_factor_with_n(contaxa_dat, var_name, demo_dat)
}
\arguments{
\item{var_name}{variable for which to append (N=XX)}

\item{human_dat}{from ed2_human used to calculate N}

\item{contaxa_data}{dataframe from reshape_contaxa_data joined with demographic variables}
}
\value{
dataframe with (N=XX) appended to var_name
}
\description{
Annotate factor labels with number of observations
}
\author{
matteo-V
}
