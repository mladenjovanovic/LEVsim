# New Profile Constructor
#
# This function creates \code{LEV_profile} object
new_LEV_profile <- function(athlete,
                            V0,
                            V0_rep_drop,
                            V0_fatigue_additive,
                            V0_fatigue_multiplicative,
                            V0_visit_change_additive,
                            V0_visit_change_multiplicative,
                            V0_visit_random_additive,
                            V0_visit_random_multiplicative,
                            L0,
                            L0_rep_drop,
                            L0_fatigue_additive,
                            L0_fatigue_multiplicative,
                            L0_visit_change_additive,
                            L0_visit_change_multiplicative,
                            L0_visit_random_additive,
                            L0_visit_random_multiplicative,
                            v1RM,
                            v1RM_random_additive,
                            v1RM_random_multiplicative,
                            est_RIR_systematic_additive,
                            est_RIR_systematic_multiplicative,
                            est_RIR_random_additive,
                            est_RIR_random_multiplicative,
                            est_0RIR_error,
                            biological_variation_additive,
                            biological_variation_multiplicative,
                            instrumentation_noise_additive,
                            instrumentation_noise_multiplicative,
                            load_increment) {


  # First create data frame to make sure parameters align up neatly
  LEV_df <- dplyr::tibble(
    athlete = athlete,
    V0 = V0,
    V0_rep_drop = V0_rep_drop,
    V0_fatigue_additive = V0_fatigue_additive,
    V0_fatigue_multiplicative = V0_fatigue_multiplicative,
    V0_visit_change_additive = V0_visit_change_additive,
    V0_visit_change_multiplicative = V0_visit_change_multiplicative,
    V0_visit_random_additive = V0_visit_random_additive,
    V0_visit_random_multiplicative = V0_visit_random_multiplicative,
    L0 = L0,
    L0_rep_drop = L0_rep_drop,
    L0_fatigue_additive = L0_fatigue_additive,
    L0_fatigue_multiplicative = L0_fatigue_multiplicative,
    L0_visit_change_additive = L0_visit_change_additive,
    L0_visit_change_multiplicative = L0_visit_change_multiplicative,
    L0_visit_random_additive = L0_visit_random_additive,
    L0_visit_random_multiplicative = L0_visit_random_multiplicative,
    v1RM = v1RM,
    v1RM_random_additive = v1RM_random_additive,
    v1RM_random_multiplicative = v1RM_random_multiplicative,
    est_RIR_systematic_additive = est_RIR_systematic_additive,
    est_RIR_systematic_multiplicative = est_RIR_systematic_multiplicative,
    est_RIR_random_additive = est_RIR_random_additive,
    est_RIR_random_multiplicative = est_RIR_random_multiplicative,
    est_0RIR_error = est_0RIR_error,
    biological_variation_additive = biological_variation_additive,
    biological_variation_multiplicative = biological_variation_multiplicative,
    instrumentation_noise_additive = instrumentation_noise_additive,
    instrumentation_noise_multiplicative = instrumentation_noise_multiplicative,
    load_increment = load_increment,

    # Additional parameters
    intercept = V0,
    slope = -V0 / L0,
    `1RM` = get_load_at_velocity(V0, L0, v1RM)
  )

  ## Check the athlete names
  # If any entry missing, just use numbers
  if (any(is.na(LEV_df$athlete))) {
    LEV_df$athlete <- seq_along(LEV_df$athlete)
  }

  # If names not unique, use numbers
  if (length(unique(LEV_df$athlete)) != length(LEV_df$athlete)) {
    warning("Athletes are not unique. Switching to index numbers", call. = FALSE, immediate. = TRUE)
    LEV_df$athlete <- seq_along(LEV_df$athlete)
  }

  # Now, for each athlete (i.e., each row), create a list element
  LEV_profiles <- purrr::pmap(LEV_df, function(...) {
    params <- list(...)
    athlete_profile <- list(
      athlete = params$athlete,
      profile = params,
      visit = NULL
    )
  })

  class(LEV_profiles) <- "LEV_profile"

  # Name the profiles
  names(LEV_profiles) <- LEV_df$athlete

  return(LEV_profiles)
}
