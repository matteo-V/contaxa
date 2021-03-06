% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_fit_funs.R
\name{remove_colinear_columns}
\alias{remove_colinear_columns}
\title{Use matrix rank algorithm to remove linearly dependent columns from data}
\usage{
remove_colinear_columns(dat)
}
\arguments{
\item{dat}{data frame generated by get_XXX_analysis_frame}
}
\value{
named list of removed variable names (removed_vars) and matrix for lasso modeling (data_matrix)
}
\description{
Use matrix rank algorithm to remove linearly dependent columns from data
}
