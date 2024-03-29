% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate-RTF-grid.R
\name{estimate_RTF_grid}
\alias{estimate_RTF_grid}
\title{Estimate Reps to Failure Profile using Grid Search}
\usage{
estimate_RTF_grid(
  load,
  MNR,
  V0 = 1.8,
  L0 = 180,
  v1RM = 0.2,
  L0_rep_drop = seq(0, 0.15, by = 0.005),
  V0_rep_drop = seq(0, 0.15, by = 0.005),
  fractional = FALSE,
  relative_load = FALSE,
  return_multiple = FALSE,
  max_reps = 100,
  na.rm = FALSE
)
}
\arguments{
\item{load}{Numeric vector indicating absolute load used. If relative load is used,
then use \code{relative_load = TRUE}}

\item{MNR}{Numeric vector indicating maximum number of repetitions performed}

\item{V0}{Profile parameter}

\item{L0}{Profile parameter}

\item{v1RM}{Profile parameter}

\item{L0_rep_drop}{Search vector parameters}

\item{V0_rep_drop}{Search vector parameters}

\item{fractional}{Should fractional MNR be returned? Default is \code{FALSE}}

\item{relative_load}{If \code{load} is relative, then absolute load is calculated by
multiplying with profile 1RM}

\item{return_multiple}{Should multiple solutions be reported? Defaults is \code{FALSE}}

\item{max_reps}{How many maximum reps to generate to search for failure? Default is 100}

\item{na.rm}{Should missing values be removed? Defaults is \code{FALSE}}
}
\value{
Object \code{LEV_estimate}
}
\description{
Finds the \code{L0_reps_drop} and \code{V0_reps_drop} parameters using the grid
search that minimizes the RMSE given the performed \code{load} and \code{MNR} and \code{V0},
\code{L0}, and \code{v1RM} parameters.
}
\examples{
RTF_df <- create_profiles() \%>\%
  create_visits() \%>\%
  create_tests() \%>\%
  create_summary() \%>\%
  as.data.frame() \%>\%
  dplyr::filter(set == "RTF")

m1 <- estimate_RTF_grid(
  RTF_df$load,
  RTF_df$nRM
)

m1
}
