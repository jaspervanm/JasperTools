% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_ttr.R
\name{calc_ttr}
\alias{calc_ttr}
\title{Time within therapeutic range}
\usage{
calc_ttr(INR_meas, range.lower, range.upper, to = max(INR_meas$INR_date),
  from = min(INR_meas$INR_date))
}
\arguments{
\item{INR_meas}{Data.frame with (at least) the following columns:
INR_date (a date object with the date of the INR measurement),
INR (the international normalised ratio).}

\item{range.lower, range.upper}{The INR target range.}

\item{to, from}{Period over which the TTR is calculated.
Interpolation from earlier INRs will take place, but not from INR's after to (since we cannot see in the future).}
}
\description{
Calculate the TTR according to Rosendaal.
}
