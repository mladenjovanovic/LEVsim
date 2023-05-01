# Get Reps Velocity
#
# Function provide Exertion-Velocity profile utilizing \code{V0_rep_drop} and
#     \code{L0_rep_drop} parameters
#
get_reps_velocity <- function(V0,
                              V0_rep_drop_additive,
                              V0_rep_drop_multiplicative,
                              L0,
                              L0_rep_drop_additive,
                              L0_rep_drop_multiplicative,
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
    V0_rep_drop_additive = V0_rep_drop_additive,
    V0_rep_drop_multiplicative = V0_rep_drop_multiplicative,
    L0 = L0,
    L0_rep_drop_additive = L0_rep_drop_additive,
    L0_rep_drop_multiplicative = L0_rep_drop_multiplicative,
    rep = rep,
    load = load
  ) %>%
    dplyr::mutate(
      # Effects of fatigue within-set (i.e., between reps)
      rep_initial_velocity = get_velocity_at_load(V0, L0, load),
      rep_V0 = V0 - V0_rep_drop_multiplicative * (V0 - rep_initial_velocity) * (rep - 1) - V0_rep_drop_additive * (rep - 1),
      rep_L0 = L0 - L0_rep_drop_multiplicative * load * (rep - 1) - L0_rep_drop_additive * (rep - 1),

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
