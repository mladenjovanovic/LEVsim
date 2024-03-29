% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile-S3.R
\name{plot.LEV_profile}
\alias{plot.LEV_profile}
\title{S3 method for plotting LEV Profiles}
\usage{
\method{plot}{LEV_profile}(x, type = "athletes", ...)
}
\arguments{
\item{x}{Object of class \code{LEV_profile}}

\item{type}{Type of plot. Default is "athletes" which creates facets
for each athlete. Another option is "pooled", which doesn't create
facets}

\item{...}{Extra arguments}
}
\value{
\code{\link[ggplot2]{ggplot}} object
}
\description{
S3 method for plotting LEV Profiles
}
\examples{
profiles <- create_athletes(10)
plot(profiles)

visits <- profiles \%>\% create_visits(1:3)
plot(visits)
}
