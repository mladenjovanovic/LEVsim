% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary-S3.R
\name{print.LEV_summary}
\alias{print.LEV_summary}
\title{S3 method for printing LEV Sets Summaries}
\usage{
\method{print}{LEV_summary}(x, ...)
}
\arguments{
\item{x}{Object of class \code{LEV_summary}}

\item{...}{Extra arguments}
}
\description{
S3 method for printing LEV Sets Summaries
}
\examples{
# Create random athletes/profiles
sets <- create_athletes(1) \%>\%
  create_visits(1) \%>\%
  create_sets(load = c(70, 80, 90)) \%>\%
  create_summary()

print(sets)
}
