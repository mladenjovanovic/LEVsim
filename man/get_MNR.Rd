% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-MNR.R
\name{get_MNR}
\alias{get_MNR}
\title{Get Maximum Number of Reps}
\usage{
get_MNR(
  load,
  V0 = 1.8,
  L0 = 180,
  v1RM = 0.2,
  L0_rep_drop = 0.045,
  V0_rep_drop = 0,
  relative_load = FALSE,
  fractional = FALSE,
  max_reps = 100,
  na.rm = FALSE
)
}
\arguments{
\item{load}{Numeric vector indicating absolute load used. If relative load is used,
then use \code{relative_load = TRUE}}

\item{V0}{Profile parameter}

\item{L0}{Profile parameter}

\item{v1RM}{Profile parameter}

\item{L0_rep_drop}{Profile parameter}

\item{V0_rep_drop}{Profile parameter}

\item{relative_load}{If \code{load} is relative, then absolute load is calculated by
multiplying with profile 1RM}

\item{fractional}{Should fractional MNR be returned? Default is \code{FALSE}}

\item{max_reps}{How many maximum reps to generate to search for failure? Default is 100}

\item{na.rm}{Should missing values be removed? Defaults is \code{FALSE}}
}
\value{
Numeric vector
}
\description{
Find the maximum number of repetitions (\code{MNR}, or \code{nRM}) given the
\code{load}, \code{L0}, \code{V0}, \code{v1RM}, \code{L0_rep_drop}, and \code{V0_rep_drop}
parameters. This functions uses true repetition velocity to discern MNR
}
\examples{
get_MNR(c(140, 145, 150), fractional = FALSE)
get_MNR(c(140, 145, 150), fractional = TRUE)
get_MNR(150, L0 = c(170, 180, 190), fractional = TRUE)
}
