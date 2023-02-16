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

# Function to find last RIR
get_last_RIR <- function(RIR, failed) {
  # If there are no failed reps
  if (!any(failed)) {
    last_RIR <- min(RIR)
  } else {
    # if there are failed reps
    last_RIR <- min(RIR) + 1
  }

  last_RIR
}

# Function to find reps done
get_reps_done <- function(rep, failed) {
  # If there are no failed reps
  if (!any(failed)) {
    last_rep <- max(rep)
  } else {
    # if there are failed reps
    last_rep <- max(rep) - 1
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
get_sets <- function(visit_LEV_profile,
                     load,
                     reps = rep(NA, length(load)),
                     max_reps = 100,
                     use_true_velocity = FALSE,
                     inter_set_fatigue = TRUE) {

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
  rep_V0 <- NULL
  rep_L0 <- NULL
  biological_rep_velocity <- NULL
  v1RM <- NULL
  failed_rep <- NULL
  last_rep <- NULL
  nRM <- NULL
  measured_rep_velocity <- NULL
  best_measured_rep_velocity <- NULL
  VL <- NULL
  load_index <- NULL
  last_row <- NULL
  set <- NULL
  V0_fatigue <- NULL
  V0_fatigue_multiplicative <- NULL
  L0_fatigue <- NULL
  L0_fatigue_multiplicative <- NULL
  set_V0 <- NULL
  set_L0 <- NULL
  set_1RM <- NULL
  true_rep_velocity <- NULL
  orig_L0 <- NULL
  orig_V0 <- NULL
  target_reps <- NULL
  RIR <- NULL
  est_RIR <- NULL
  est_RIR_random <- NULL
  est_RIR_random_multiplicative <- NULL
  est_RIR_systematic <- NULL
  est_RIR_systematic_multiplicative <- NULL
  est_nRM <- NULL
  last_RIR <- NULL
  last_eRIR <- NULL
  last_eRIR_rnd <- NULL
  last_eRIR_sys <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  class(visit_LEV_profile) <- "list"
  profile_info <- dplyr::bind_rows(visit_LEV_profile)

  # Small trick to recycle the vector
  reps <- reps + (0 * load)

  # Generate all reps for trials and reps
  sets <- tidyr::expand_grid(
    profile_info,
    load_index = seq_along(load),
    rep = seq(1, max_reps)
  ) %>%
    dplyr::mutate(
      target_reps = reps[load_index],
      set = load_index,
      load = load[load_index]
    )

  if (inter_set_fatigue == TRUE) {
    sets <- sets %>%
      dplyr::mutate(
        set_L0 = systematic_effect(L0, load_index - 1, L0_fatigue, L0_fatigue_multiplicative),
        set_V0 = systematic_effect(V0, load_index - 1, V0_fatigue, V0_fatigue_multiplicative),
        set_1RM = get_load_at_velocity(set_V0, set_L0, v1RM)
      )
  } else {
    sets <- sets %>%
      dplyr::mutate(
        set_L0 = L0,
        set_V0 = V0,
        set_1RM = get_load_at_velocity(set_V0, set_L0, v1RM)
      )
  }

  sets <- sets %>%
    dplyr::mutate(
      # These are temp
      orig_L0 = L0,
      orig_V0 = V0,
    ) %>%
    dplyr::mutate(
      get_reps_velocity(
        set_V0,
        V0_rep_drop,
        set_L0,
        L0_rep_drop,
        biological_variation,
        biological_variation_multiplicative,
        instrumentation_noise,
        instrumentation_noise_multiplicative,
        rep,
        load
      )
    ) %>%
    dplyr::mutate(
      V0 = orig_V0,
      L0 = orig_L0
    ) %>%
    dplyr::select(-orig_V0, -orig_L0)

  # Now we need to clean them up and provide summaries
  # --------------------------------

  # Cleaned reps
  cleaned_sets <- sets %>%
    dplyr::group_by(load_index)

  # Here use either true_rep_velocity or biological_rep_velocity
  if (use_true_velocity == TRUE) {
    cleaned_sets <- cleaned_sets %>%
      dplyr::mutate(
        failed_rep = true_rep_velocity <= v1RM
      )
  } else {
    cleaned_sets <- cleaned_sets %>%
      dplyr::mutate(
        failed_rep = biological_rep_velocity <= v1RM
      )
  }

  cleaned_sets <- cleaned_sets %>%
    # Find first occurrence where velocity drops below v1RM
    # And filter everything after that including that
    dplyr::mutate(
      last_rep = get_last_rep(rep, failed_rep),
      last_row = ifelse(is.na(last_rep), dplyr::n(), last_rep)
    ) %>%
    dplyr::filter(dplyr::row_number() <= last_row) %>%
    # Rep exertion summaries
    dplyr::mutate(
      rep_1RM = get_load_at_velocity(rep_V0, rep_L0, v1RM),
      RIR = last_rep - rep - 1,
      nRM = last_rep - 1,
      `%MNR` = (rep / nRM) * 100,
      best_measured_rep_velocity = cummax(measured_rep_velocity),
      VL = best_measured_rep_velocity - measured_rep_velocity,
      `%VL` = VL / best_measured_rep_velocity * 100,
      VR = 100 * (measured_rep_velocity - v1RM) / (best_measured_rep_velocity - v1RM)
    ) %>%
    dplyr::mutate(set = load_index) %>%
    dplyr::filter(rep <= ifelse(is.na(target_reps), Inf, target_reps)) %>%
    dplyr::mutate(
      # Calculate estimated RIR and %MNR
      reps_done = get_reps_done(rep, failed_rep),
      last_RIR = get_last_RIR(RIR, failed_rep),
      last_eRIR_sys = systematic_effect(last_RIR[1], visit = 1, effect = est_RIR_systematic[1], multiplicative = est_RIR_systematic_multiplicative[1]) - last_RIR[1],
      last_eRIR_rnd = random_effect(last_RIR[1], effect = est_RIR_random[1], multiplicative = est_RIR_random_multiplicative[1]) - last_RIR[1],
      last_eRIR = last_RIR[1] + last_eRIR_sys + last_eRIR_rnd,
      last_eRIR = ifelse(last_eRIR < 0, 0, last_eRIR),
      last_eRIR = round(last_eRIR),
      est_RIR = last_eRIR + (RIR - last_RIR),
      est_nRM = nRM + est_RIR - RIR,
      `est_%MNR` = (rep / est_nRM) * 100,
      set_to_failure = ifelse(is.na(RIR), FALSE, ifelse(any(RIR <= 0), TRUE, FALSE))
    ) %>%
    dplyr::select(-last_rep, -last_row, -last_RIR, -last_eRIR, -last_eRIR_sys, -last_eRIR_rnd) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(load_index, rep)

  # Return cleaned sets
  return(cleaned_sets)
}
