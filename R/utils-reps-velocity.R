
# Functions to model within-set fatigue
rep_L0_func <- function(L0, load, rep, k) {
  L0 - k * load * (rep - 1)
}

rep_V0_func <- function(V0, initial_velocity, rep, k) {
  V0 - k * (V0 - initial_velocity) * (rep - 1)
}

# Get Reps Velocity
#
# Function provide Exertion-Velocity profile utilizing \code{V0_rep_drop} and
#     \code{L0_rep_drop} parameters
#
get_reps_velocity <- function(V0,
                              V0_rep_drop,
                              L0,
                              L0_rep_drop,
                              biological_variation_additive,
                              biological_variation_multiplicative,
                              instrumentation_noise_additive,
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
  measured_rep_velocity <- NULL
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
      rep_V0 = rep_V0_func(V0, rep_initial_velocity, rep, V0_rep_drop),
      rep_L0 = rep_L0_func(L0, load, rep, L0_rep_drop),

      # True profile velocity
      true_rep_velocity = get_velocity_at_load(rep_V0, rep_L0, load),

      # Add biological variation
      biological_rep_velocity = random_effect(true_rep_velocity, 1, biological_variation_multiplicative, TRUE),
      biological_rep_velocity = random_effect(biological_rep_velocity, 1, biological_variation_additive, FALSE),

      # Add instrumentation noise
      measured_rep_velocity = random_effect(biological_rep_velocity, 1, instrumentation_noise_multiplicative, TRUE),
      measured_rep_velocity = random_effect(measured_rep_velocity, 1, instrumentation_noise_additive, FALSE)
    )

  return(reps)
}
