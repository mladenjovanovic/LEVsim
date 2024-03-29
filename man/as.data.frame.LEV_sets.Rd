% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sets-S3.R
\name{as.data.frame.LEV_sets}
\alias{as.data.frame.LEV_sets}
\title{S3 method for extracting LEV Profiles Sets}
\usage{
\method{as.data.frame}{LEV_sets}(x, row.names = NULL, optional = FALSE, ...)
}
\arguments{
\item{x}{Object of class \code{LEV_sets}}

\item{row.names}{Not used}

\item{optional}{Not used}

\item{...}{Extra arguments}
}
\value{
Data Frame
}
\description{
S3 method for extracting LEV Profiles Sets
}
\examples{
# Create random athletes/profiles
sets <- create_athletes(1) \%>\%
  create_visits(1) \%>\%
  create_sets(load = c(70, 80, 90))

sets_params <- as.data.frame(sets)
}
