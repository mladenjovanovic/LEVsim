% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-profiles.R
\name{create_profiles}
\alias{create_profiles}
\title{Create Load-Exertion-Velocity Profiles}
\usage{
create_profiles(
  athlete = NA,
  V0 = 1.8,
  V0_rep_drop = 0,
  V0_fatigue_additive = 0,
  V0_fatigue_multiplicative = 0,
  V0_visit_change_additive = 0,
  V0_visit_change_multiplicative = 0,
  V0_visit_random_additive = 0,
  V0_visit_random_multiplicative = 0,
  L0 = 180,
  L0_rep_drop = 0.045,
  L0_fatigue_additive = 0,
  L0_fatigue_multiplicative = 0,
  L0_visit_change_additive = 0,
  L0_visit_change_multiplicative = 0,
  L0_visit_random_additive = 0,
  L0_visit_random_multiplicative = 0,
  v1RM = 0.2,
  v1RM_random_additive = 0,
  v1RM_random_multiplicative = 0,
  est_RIR_systematic_additive = 0,
  est_RIR_systematic_multiplicative = 0,
  est_RIR_random_additive = 0,
  est_RIR_random_multiplicative = 0,
  est_0RIR_error = FALSE,
  biological_variation_additive = 0,
  biological_variation_multiplicative = 0,
  instrumentation_noise_additive = 0,
  instrumentation_noise_multiplicative = 0,
  load_increment = 2.5
)
}
\arguments{
\item{athlete}{Numeric or character vector indicating different athletes. Can also be names}

\item{V0}{Highest velocity when load equals zero}

\item{V0_rep_drop}{Number indicating fixed amount of \code{V0} dropping
per repetition within set. This parameter models the
exertion-velocity characteristic}

\item{V0_fatigue_additive}{Number indicating additive systematic change in \code{V0} across multiple
sets, simulating effect of fatigue}

\item{V0_fatigue_multiplicative}{Number indicating proportional systematic change in \code{V0} across multiple
sets, simulating effect of fatigue}

\item{V0_visit_change_additive}{Number indicating additive systematic change in \code{V0} per
each visit}

\item{V0_visit_change_multiplicative}{Number indicating proportional systematic change in \code{V0} per
each visit}

\item{V0_visit_random_additive}{Number indicating SD of additive random change in \code{V0} for
each visit}

\item{V0_visit_random_multiplicative}{Number indicating SD of proportional random change in \code{V0} for
each visit}

\item{L0}{Highest load when velocity equals zero}

\item{L0_rep_drop}{Number indicating fixed amount of \code{L0} dropping
per repetition within set. This parameter models the
exertion-velocity characteristic}

\item{L0_fatigue_additive}{Number indicating additive systematic change in \code{L0} across multiple
sets, simulating effect of fatigue}

\item{L0_fatigue_multiplicative}{Number indicating proportional systematic change in \code{L0} across multiple
sets, simulating effect of fatigue}

\item{L0_visit_change_additive}{Number indicating additive systematic change in \code{L0} per
each visit}

\item{L0_visit_change_multiplicative}{Number indicating proportional systematic change in \code{L0} per
each visit}

\item{L0_visit_random_additive}{Number indicating SD of additive random change in \code{L0} for
each visit}

\item{L0_visit_random_multiplicative}{Number indicating SD of proportional random change in \code{L0} for
each visit}

\item{v1RM}{Velocity at 1RM, or Minimum Velocity Threshold}

\item{v1RM_random_additive}{Number indicating SD of additive random change in \code{v1RM} for
each visit}

\item{v1RM_random_multiplicative}{Number indicating SD of proportional random change in \code{v1RM} for
each visit}

\item{est_RIR_systematic_additive}{Number indicating systematic additive effect of the random
biological variation in estimating \code{RIR}}

\item{est_RIR_systematic_multiplicative}{Number indicating systematic proportional effect of the random
biological variation in estimating \code{RIR}}

\item{est_RIR_random_additive}{Number indicating SD of the random additive biological variation in estimating
\code{RIR}}

\item{est_RIR_random_multiplicative}{Number indicating SD of the random proportional biological variation
in estimating \code{RIR}}

\item{est_0RIR_error}{Logical (default \code{FALSE}) indicating whether to apply estimated RIR noise when
the true RIR is equal to zero}

\item{biological_variation_additive}{Number indicating SD of the random additive biological variation
in velocity when performing repetitions}

\item{biological_variation_multiplicative}{Number indicating SD of the random proportional biological variation
in velocity when performing repetitions}

\item{instrumentation_noise_additive}{Number indicating SD of the random additive instrumentation noise when
measuring repetition velocity}

\item{instrumentation_noise_multiplicative}{Number indicating SD of the random proportional instrumentation noise
when measuring repetition velocity}

\item{load_increment}{What should be the minimal jump in load? Default is 2.5, common load jump
in kg in resistance training}
}
\value{
Function returns \code{LV_profile} object
}
\description{
Function creates Load-Exertion-Velocity profiles, which is needed for other functions in the package.
Multiple profiles can be created
}
\examples{
# Create random athletes/profiles
set.seed(1667)

sets <- create_athletes(5) \%>\%
  create_visits(1:3) \%>\%
  create_sets(
    load = c(90, 110, 130),
    load_type = "absolute"
  )

print(sets)

# Extract data frame
LEV_data <- as.data.frame(sets)
# Or
# LEV_data <- coef(sets)

plot(sets)
plot(sets, athletes = "Athlete 1")
plot(sets, athletes = "Athlete 1", reps = 1)
plot(sets, athletes = "Athlete 1", x_var = "RIR")

# Another way to create LEV profiles
sets <- create_profiles(athlete = c("Mladen", "Ivan"), L0 = c(200, 180)) \%>\%
  create_visits(1) \%>\%
  create_sets(load = c(100, 120, 140), load_type = "absolute")

plot(sets)
plot(sets, facet = NULL, x_var = "load")
plot(sets, visits = 1, x_var = "RIR")
}
\seealso{
\code{\link[=create_athletes]{create_athletes()}} is another option to create LEV profiles
}
