% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{impute}
\alias{impute}
\title{Replaces NA values with imputed values}
\usage{
impute(.data, ...)
}
\description{
Given a data.frame and expressions, replaces
NaN values. The function is made in the style
as dplyr and as such, can use dplyr style
operations.
}
\examples{

df <- data.frame(
  a = c(1, 2, NA, 20, 30),
  b = seq(1, 5)
)

df \%>\% impute(a = mean(a, na.rm=TRUE))
df \%>\% impute(a = mean(b))
df \%>\% impute(a = sum(b[a > 3], na.rm=T))
}
