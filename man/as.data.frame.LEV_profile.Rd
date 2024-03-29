% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile-S3.R
\name{as.data.frame.LEV_profile}
\alias{as.data.frame.LEV_profile}
\title{S3 method for extracting LEV Profiles and Visits parameters}
\usage{
\method{as.data.frame}{LEV_profile}(x, row.names = NULL, optional = FALSE, ...)
}
\arguments{
\item{x}{Object of class \code{LEV_profile}}

\item{row.names}{Not used}

\item{optional}{Not used}

\item{...}{Extra arguments}
}
\value{
Data Frame
}
\description{
S3 method for extracting LEV Profiles and Visits parameters
}
\examples{
profiles <- create_athletes(10)
profile_params <- coef(profiles)

visits <- profiles \%>\% create_visits(1:3)
visit_params <- as.data.frame(visits)
}
