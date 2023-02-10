% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-program-sets.R
\name{create_program_sets}
\alias{create_program_sets}
\title{Create Program Sets
Function creates sets, using provided \code{LEV_profile}, \code{loads}, and \code{program_df}}
\usage{
create_program_sets(
  LEV_profile = create_profiles(),
  program_df,
  visit,
  load,
  reps,
  load_type = "absolute",
  max_reps = 100,
  failed_reps = FALSE,
  use_true_velocity = FALSE,
  inter_set_fatigue = TRUE,
  keep_program_df = FALSE
)
}
\arguments{
\item{LEV_profile}{\code{LEV_profile} object, returned by \code{\link{create_visits}} function}

\item{program_df}{Data frame with strength training program}

\item{visit}{String indicating the name of the column in \code{program_df} where visit
is located}

\item{load}{String indicating the name of the column in \code{program_df} where load or perc 1RM
is located}

\item{reps}{String indicating the name of the column in \code{program_df} where number of target
repetitions is located}

\item{load_type}{Type of load calculation. Can be either 'absolute' (default), or 'visit 1RM', or
'prescription 1RM'}

\item{max_reps}{How many maximum reps to generate to search for failure? Default is 100}

\item{failed_reps}{Should failed-reps be included in the output? Default is \code{FALSE}}

\item{use_true_velocity}{When estimating failure, should true or biological (default) velocity be used?}

\item{inter_set_fatigue}{Should profile inter-set fatigue parameters be utilized? Default is \code{TRUE}}

\item{keep_program_df}{Should \code{LEV_sets} object be returned or \code{data.frame}}
}
\value{
Object \code{LEV_sets}, or \code{data.frame} depending if \code{keep_program_df} equal to TRUE
}
\description{
Create Program Sets
Function creates sets, using provided \code{LEV_profile}, \code{loads}, and \code{program_df}
}
\examples{
data("strength_training_program")

set.seed(10)
program_sets <- create_profiles(athletes = 1, L0_visit_change = 1, L0_fatigue = -2.5) \%>\%
  create_visits(1:12) \%>\%
  create_prescription_1RM(buffer = 0.9) \%>\%
  create_program_sets(
    program_df = strength_training_program,
    visit = "visit",
    load = "perc_1RM",
    reps = "target_reps",
    load_type = "prescription 1RM") \%>\%
  create_summary()

plot(program_sets, type = "pooled")
}