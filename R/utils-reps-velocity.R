# Get Reps Velocity
#
# Function provide Exertion-Velocity profile utilizing \code{V0_rep_drop} and
#     \code{L0_rep_drop} parameters
#
# @param V0 Numeric
# @param V0_rep_drop Numeric
# @param L0 Numeric
# @param L0_rep_drop Numeric
# @param biological_variation Numeric
# @param biological_variation_multiplicative Logical
# @param instrumentation_noise Numeric
# @param instrumentation_noise_multiplicative Logical
# @param rep Numeric
# @param load Numeric
#
# @return Data frame
get_reps_velocity <- function(V0,
                              V0_rep_drop,
                              L0,
                              L0_rep_drop,
                              biological_variation,
                              biological_variation_multiplicative,
                              instrumentation_noise,
                              instrumentation_noise_multiplicative,
                              rep,
                              load) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  rep_initial_velocity <- NULL
  rep_V0 <- NULL
  rep_L0 <- NULL
  true_rep_velocity <- NULL
  biological_rep_velocity <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  reps <- dplyr::tibble(
    V0 = V0,
    V0_rep_drop = V0_rep_drop,
    L0 = L0,
    L0_rep_drop = L0_rep_drop,
    rep = rep,
    load = load
  ) %>%
    dplyr::mutate(
      # Effects of fatigue within-set (i.e., between reps)
      rep_initial_velocity = get_velocity_at_load(V0, L0, load),
      rep_V0 = V0 - V0_rep_drop * (V0 - rep_initial_velocity) * (rep - 1),
      rep_L0 = L0 - L0_rep_drop * load * (rep - 1),

      # True profile velocity
      true_rep_velocity = get_velocity_at_load(rep_V0, rep_L0, load),

      # Add biological variation
      biological_rep_velocity = random_effect(true_rep_velocity, 1, biological_variation, biological_variation_multiplicative),

      # Add instrumentation noise
      measured_rep_velocity = random_effect(biological_rep_velocity, 1, instrumentation_noise, instrumentation_noise_multiplicative)
    )

  return(reps)
}
