% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_ecdf_dist.R
\name{max_ecdf_dist}
\alias{max_ecdf_dist}
\title{Generate the Cumulative Distribution for a given set of values}
\usage{
max_ecdf_dist(df, group_var, feature_var, feature_string)
}
\arguments{
\item{df}{A data frame}

\item{group_var}{The variable that contains your two groups that you want to
compare--think 1 vs. 0}

\item{feature_var}{The feature or variable whose values you are interested
in exploring}

\item{feature_string}{The quoted name of the feature_var}
}
\value{
A tibble with the full cumulative distribution for the different
values of the group_var.
}
\description{
Generate the Cumulative Distribution for a given set of values
}
\examples{
\dontrun{mac_ecdf_dist(df, outcome, key, "key")}
}
