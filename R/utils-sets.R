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

# Filter missing reps
#
# Internal function for filtering missing reps, but keeping load_index
#
# @return Data frame
filter_missing_reps <- function(sets, failed_sets) {
  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  failed_rep <- NULL
  athlete <- NULL
  visit <- NULL
  load_index <- NULL
  . <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  filter_set <- function(set) {
    if (nrow(set) == 1 & failed_sets == TRUE) {
      # Only one rep is done, thus it must be kept
      set_filtered <- set
    } else {
      set_filtered <- set %>%
        dplyr::filter(failed_rep == FALSE)
    }
    set_filtered
  }

  sets %>%
    dplyr::group_by(athlete, visit, load_index) %>%
    dplyr::do(filter_set(.)) %>%
    dplyr::ungroup()
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
                     target_est_RIR = rep(NA, length(load)),
                     target_velocity = rep(NA, length(load)),
                     target_VL = rep(NA, length(load)),
                     target_est_MNR = rep(NA, length(load)),
                     max_reps = 100,
                     use_true_velocity = FALSE,
                     inter_set_fatigue = TRUE,
                     subjective_ratings = TRUE,
                     incremental_est_RIR = TRUE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  V0 <- NULL
  V0_rep_drop <- NULL
  L0 <- NULL
  L0_rep_drop <- NULL
  biological_variation_additive <- NULL
  biological_variation_multiplicative <- NULL
  instrumentation_noise_additive <- NULL
  instrumentation_noise_multiplicative <- NULL
  rep_V0 <- NULL
  rep_L0 <- NULL
  manifested_rep_velocity <- NULL
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
  V0_fatigue_additive <- NULL
  V0_fatigue_multiplicative <- NULL
  L0_fatigue_additive <- NULL
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
  est_RIR_random_additive <- NULL
  est_RIR_random_multiplicative <- NULL
  est_RIR_systematic_additive <- NULL
  est_RIR_systematic_multiplicative <- NULL
  est_0RIR_error <- NULL
  est_nRM <- NULL
  last_RIR <- NULL
  last_eRIR <- NULL
  last_eRIR_rnd <- NULL
  last_eRIR_sys <- NULL
  set_to_failure <- NULL
  `%VL` <- NULL
  `est_%MNR` <- NULL
  last_est_RIR <- NULL
  reps_done <- NULL
  stop_VL <- NULL
  stop_est_MNR <- NULL
  stop_est_RIR <- NULL
  stop_indicator <- NULL
  stop_indicator_last_row <- NULL
  stop_indicator_rep <- NULL
  stop_rep <- NULL
  stop_velocity <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  class(visit_LEV_profile) <- "list"
  profile_info <- dplyr::bind_rows(visit_LEV_profile)

  # Small trick to recycle the vector
  reps <- reps + (0 * load)
  target_est_RIR <- target_est_RIR + (0 * load)
  target_velocity <- target_velocity + (0 * load)
  target_est_MNR <- target_est_MNR + (0 * load)
  target_VL <- target_VL + (0 * load)

  # Generate all reps for trials and reps
  sets <- tidyr::expand_grid(
    profile_info,
    load_index = seq_along(load),
    rep = seq(1, max_reps)
  ) %>%
    dplyr::mutate(
      target_reps = reps[load_index],
      target_est_RIR = target_est_RIR[load_index],
      target_est_MNR = target_est_MNR[load_index],
      target_velocity = target_velocity[load_index],
      target_VL = target_VL[load_index],
      set = load_index,
      load = load[load_index]
    )

  if (inter_set_fatigue == TRUE) {
    sets <- sets %>%
      dplyr::mutate(
        set_L0 = systematic_effect(L0, load_index - 1, -L0_fatigue_multiplicative, TRUE),
        set_L0 = systematic_effect(set_L0, load_index - 1, -L0_fatigue_additive, FALSE),

        set_V0 = systematic_effect(V0, load_index - 1, -V0_fatigue_multiplicative, TRUE),
        set_V0 = systematic_effect(set_V0, load_index - 1, -V0_fatigue_additive, FALSE),

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
        V0 = set_V0,
        V0_rep_drop = V0_rep_drop,
        L0 = set_L0,
        L0_rep_drop = L0_rep_drop,
        biological_variation_additive = biological_variation_additive,
        biological_variation_multiplicative = biological_variation_multiplicative,
        instrumentation_noise_additive = instrumentation_noise_additive,
        instrumentation_noise_multiplicative = instrumentation_noise_multiplicative,
        rep = rep,
        load = load
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

  # Here use either true_rep_velocity or manifested_rep_velocity
  if (use_true_velocity == TRUE) {
    cleaned_sets <- cleaned_sets %>%
      dplyr::mutate(
        failed_rep = true_rep_velocity <= v1RM
      )
  } else {
    cleaned_sets <- cleaned_sets %>%
      dplyr::mutate(
        failed_rep = manifested_rep_velocity <= v1RM
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
    dplyr::mutate(set = load_index)


  # Add subjective ratings/metrics
  if (subjective_ratings == TRUE) {
    cleaned_sets <- cleaned_sets %>%
      dplyr::mutate(
        # Systematic effects
        est_RIR = systematic_effect(RIR, visit = 1, effect = est_RIR_systematic_multiplicative, TRUE),
        est_RIR = systematic_effect(est_RIR, visit = 1, effect = est_RIR_systematic_additive, FALSE),

        # Random effects
        est_RIR = random_effect(est_RIR, visit = 1, effect = est_RIR_random_multiplicative, TRUE),
        est_RIR = random_effect(est_RIR, visit = 1, effect = est_RIR_random_additive, FALSE),

        est_RIR = ifelse(est_RIR < 0, 0, est_RIR),
        est_RIR = round(est_RIR),

        # Get other estimated metrics
        est_nRM = rep + est_RIR,
        `est_%MNR` = (rep / est_nRM) * 100
      )
  } else {
    cleaned_sets <- cleaned_sets %>%
      dplyr::mutate(
        est_RIR = NA,
        est_nRM = NA,
        `est_%MNR` = NA
      )
  }

  # Quality control
  cleaned_sets <- cleaned_sets %>%
    dplyr::mutate(
      stop_rep = rep >= ifelse(is.na(target_reps), Inf, target_reps),
      stop_est_RIR = est_RIR <= ifelse(is.na(target_est_RIR), -Inf, target_est_RIR),
      stop_est_MNR = `est_%MNR` >= ifelse(is.na(target_est_MNR), Inf, target_est_MNR),
      stop_velocity = measured_rep_velocity <= ifelse(is.na(target_velocity), -Inf, target_velocity),
      stop_VL = `%VL` > ifelse(is.na(target_VL), Inf, target_VL),
      stop_indicator = stop_rep | stop_est_RIR | stop_est_MNR | stop_velocity | stop_VL,
      stop_indicator = ifelse(is.na(stop_indicator), FALSE, stop_indicator),
      stop_indicator_rep = get_last_rep(rep, stop_indicator),
      stop_indicator_last_row = ifelse(is.na(stop_indicator_rep), dplyr::n(), stop_indicator_rep)
    ) %>%
    # Now filter out remove everything after quality indicator
    dplyr::filter(dplyr::row_number() <= stop_indicator_last_row) %>%

    # Add extra metrics
    dplyr::mutate(
      reps_done = get_reps_done(rep, failed_rep),
      set_to_failure = ifelse(is.na(RIR), FALSE, ifelse(any(RIR <= 0), TRUE, FALSE)),
      # If set is taken to failure, then est_RIR MUST be the same as RIR
      # Unless est_0RIR_error set to TRUE
      est_RIR = ifelse(RIR == 0 & set_to_failure == TRUE & est_0RIR_error == FALSE, 0, est_RIR)
    )

  if (incremental_est_RIR == TRUE) {
    # Fix the est RIR and other metrics
    cleaned_sets <- cleaned_sets %>%
      dplyr::mutate(
        last_est_RIR = est_RIR[ifelse(reps_done > 0, reps_done, NA)],
        est_RIR = last_est_RIR + (reps_done - rep),
        est_nRM = rep + est_RIR,
        `est_%MNR` = (rep / est_nRM) * 100
      )
  }

  # Remove -1 RIR
  cleaned_sets <- cleaned_sets %>%
    dplyr::mutate(
      RIR = ifelse(RIR < 0, NA, RIR),
      est_RIR = ifelse(is.na(RIR), NA, est_RIR)
    )

  cleaned_sets <- cleaned_sets %>%
    #dplyr::select(-last_rep, -last_row, -last_RIR, -last_eRIR, -last_eRIR_sys, -last_eRIR_rnd) %>%
    dplyr::ungroup()

  # Return cleaned sets
  return(cleaned_sets)
}
