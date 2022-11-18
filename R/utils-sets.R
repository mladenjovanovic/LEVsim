# Function used to round loads to lower load_increment
get_load_rounded <- function(load, load_increment) {
  floor(load / load_increment) * load_increment
}

# Function to return whether the rep is last
get_last_rep <- function(rep, failed) {
  if (any(failed)) {
    last_rep <- min(rep[failed])
  } else {
    last_rep <- NA
  }

  last_rep
}

# Generate Single Visit Sets
#
# Internal function for creating sets during a single visit
# @param visit_LEV_profile \code{FEV_visit} object
# @param load Numeric vector
# @param max_reps How many maximum reps to generate? Default is 100
#
# @return Data frame
get_sets <- function(visit_LEV_profile, load, max_reps = 100) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  V0 <- NULL
  V0_rep_drop <- NULL
  L0 <- NULL
  L0_rep_drop <- NULL
  biological_variation <- NULL
  biological_variation_multiplicative <- NULL
  instrumentation_noise <- NULL
  instrumentation_noise_multiplicative <- NULL
  trial <- NULL
  trial <- NULL
  rep_V0 <- NULL
  rep_L0 <- NULL
  biological_rep_velocity <- NULL
  v1RM <- NULL
  failed_rep <- NULL
  last_rep <- NULL
  nRM <- NULL
  measured_rep_velocity <- NULL
  fastest_rep_velocity <- NULL
  VL <- NULL
  load_index <- NULL
  trial_index <- NULL
  last_rep_velocity <- NULL
  nRM_velocity <- NULL
  last_row <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  class(visit_LEV_profile) <- "list"
  profile_info <- dplyr::bind_rows(visit_LEV_profile)

  # Generate all reps for trials and reps
  sets <- tidyr::expand_grid(
    profile_info,
    load_index = seq_along(load),
    rep = seq(1, max_reps)
  ) %>%
    dplyr::mutate(
      load = load[load_index]
    ) %>%
    dplyr::mutate(
      get_reps_velocity(
        V0,
        V0_rep_drop,
        L0,
        L0_rep_drop,
        biological_variation,
        biological_variation_multiplicative,
        instrumentation_noise,
        instrumentation_noise_multiplicative,
        rep,
        load
      )
    )

  # Now we need to clean them up and provide summaries
  # --------------------------------

  # Cleaned reps
  cleaned_sets <- sets %>%
    dplyr::group_by(load_index) %>%
    dplyr::mutate(
      failed_rep = biological_rep_velocity <= v1RM
    ) %>%
    # Find first occurrence where velocity drops below v1RM
    # And filter everything after that including that
    dplyr::mutate(
      last_rep = get_last_rep(rep, failed_rep),
      last_row = ifelse(is.na(last_rep), dplyr::n(), last_rep)) %>%
    dplyr::filter(dplyr::row_number() <= last_row) %>%
    # Rep exertion summaries
    dplyr::mutate(
      rep_1RM = get_load_at_velocity(rep_V0, rep_L0, v1RM),
      RIR = last_rep - rep - 1,
      nRM = last_rep - 1,
      RTF = ifelse(is.na(nRM), FALSE, TRUE),
      `%MNR` = (rep / nRM) * 100,
      best_measured_rep_velocity = cummax(measured_rep_velocity),
      VL = best_measured_rep_velocity - measured_rep_velocity,
      `%VL` = VL / best_measured_rep_velocity * 100,
      VR = 100 * (measured_rep_velocity - v1RM) / (best_measured_rep_velocity - v1RM)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(load_index, rep) %>%
    dplyr::mutate(set = load_index) %>%
    dplyr::select(-last_rep, -last_row)

  # Return cleaned sets
  return(cleaned_sets)
}
