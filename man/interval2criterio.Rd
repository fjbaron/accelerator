% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval2criterio.R
\name{interval2criterio}
\alias{interval2criterio}
\title{Convert a dataframe of intervals, given a vector of timestamps representing epochs to
a boolean vector representing if that epoch belong to one of the intervals}
\usage{
interval2criterio(ts, intervalos)
}
\arguments{
\item{ts}{}

\item{intervalos}{}
}
\value{
a logical vector of length(ts) indicating if that time velong to a interval
}
\description{
Convert a dataframe of intervals, given a vector of timestamps representing epochs to
a boolean vector representing if that epoch belong to one of the intervals
}
