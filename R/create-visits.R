#' Create Visits
#'
#' Before sets can be generated, it is necessary to create visits to the laboratory/gym
#'
#' @param LEV_profile Object returned from \code{\link{create_profiles}} or \code{\link{create_athletes}}
#' @param visit Numeric vector. Default is 1
#'
#' @return \code{LEV_profile} object
#' @export
#'
#' @examples
#' # Create random athletes/profiles
#' set.seed(1667)
#'
#' sets <- create_athletes(5) %>%
#'   create_visits(1:3) %>%
#'   create_sets(
#'     load = c(90, 110, 130),
#'     load_type = "absolute"
#'   )
#'
#' print(sets)
#'
#' # Extract data frame
#' LEV_data <- as.data.frame(sets)
#' # Or
#' # LEV_data <- coef(sets)
#'
#' plot(sets)
#' plot(sets, athletes = "Athlete 1")
#' plot(sets, athletes = "Athlete 1", reps = 1)
#' plot(sets, athletes = "Athlete 1", x_var = "RIR")
#'
#' # Another way to create LEV profiles
#' sets <- create_profiles(athlete = c("Mladen", "Ivan"), L0 = c(200, 180)) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(100, 120, 140), load_type = "absolute")
#'
#' plot(sets)
#' plot(sets, facet = NULL, x_var = "load")
#' plot(sets, visits = 1, x_var = "RIR")
create_visits <- function(LEV_profile = create_profiles(),
                          visit = 1) {


  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  V0 <- NULL
  V0_visit_change_additive <- NULL
  V0_visit_change_multiplicative <- NULL
  L0 <- NULL
  L0_visit_change_additive <- NULL
  L0_visit_change_multiplicative <- NULL
  new_V0 <- NULL
  V0_visit_random_additive <- NULL
  V0_visit_random_multiplicative <- NULL
  new_L0 <- NULL
  L0_visit_random_additive <- NULL
  L0_visit_random_multiplicative <- NULL
  v1RM <- NULL
  new_v1RM <- NULL
  v1RM_random_additive <- NULL
  v1RM_random_multiplicative <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(LEV_profile, stop_running = TRUE)

  # Make sure visits are numeric
  if (!is.numeric(visit)) {
    stop("Visits must be numeric", call. = FALSE)
  }

  # Cycle through each profile in the object, and add visits
  LEV_profile <- purrr::map(LEV_profile, function(profile) {
    # Create new L0 and V0 for each visit using
    # systematic and random theoretical effects
    true_profile <- profile$profile
    visits_df <- dplyr::tibble(
      athlete = profile$athlete,
      visit = visit,

      # V0 visit effects
      V0 = true_profile$V0,
      V0_rep_drop = true_profile$V0_rep_drop,
      V0_fatigue_additive = true_profile$V0_fatigue_additive,
      V0_fatigue_multiplicative = true_profile$V0_fatigue_multiplicative,
      V0_visit_change_additive = true_profile$V0_visit_change_additive,
      V0_visit_change_multiplicative = true_profile$V0_visit_change_multiplicative,
      V0_visit_random_additive = true_profile$V0_visit_random_additive,
      V0_visit_random_multiplicative = true_profile$V0_visit_random_multiplicative,

      # L0 visit effects
      L0 = true_profile$L0,
      L0_rep_drop = true_profile$L0_rep_drop,
      L0_fatigue_additive = true_profile$L0_fatigue_additive,
      L0_fatigue_multiplicative = true_profile$L0_fatigue_multiplicative,
      L0_visit_change_additive = true_profile$L0_visit_change_additive,
      L0_visit_change_multiplicative = true_profile$L0_visit_change_multiplicative,
      L0_visit_random_additive = true_profile$L0_visit_random_additive,
      L0_visit_random_multiplicative = true_profile$L0_visit_random_multiplicative,

      # v1RM visit effects
      v1RM = true_profile$v1RM,
      v1RM_random_additive = true_profile$v1RM_random_additive,
      v1RM_random_multiplicative = true_profile$v1RM_random_multiplicative,

      est_RIR_systematic_additive = true_profile$est_RIR_systematic_additive,
      est_RIR_systematic_multiplicative = true_profile$est_RIR_systematic_multiplicative,
      est_RIR_random_additive = true_profile$est_RIR_random_additive,
      est_RIR_random_multiplicative = true_profile$est_RIR_random_multiplicative,

      biological_variation_additive = true_profile$biological_variation_additive,
      biological_variation_multiplicative = true_profile$biological_variation_multiplicative,

      instrumentation_noise_additive = true_profile$instrumentation_noise_additive,
      instrumentation_noise_multiplicative = true_profile$instrumentation_noise_multiplicative,

      load_increment = true_profile$load_increment
    )

    # New V0, L0, and v1RM
    visits_df <- visits_df %>%
      dplyr::mutate(
        # Systematic effects
        # Deduct 1 from visit since first visit is initial without systematic effects
        new_V0 = systematic_effect(V0, visit - 1, V0_visit_change_multiplicative, TRUE),
        new_V0 = systematic_effect(new_V0, visit - 1, V0_visit_change_additive, FALSE),

        new_L0 = systematic_effect(L0, visit - 1, L0_visit_change_multiplicative, TRUE),
        new_L0 = systematic_effect(new_L0, visit - 1, L0_visit_change_additive, FALSE),

        # Random effects
        new_V0 = random_effect(new_V0, visit - 1, V0_visit_random_multiplicative, TRUE),
        new_V0 = random_effect(new_V0, visit - 1, V0_visit_random_additive, FALSE),

        new_L0 = random_effect(new_L0, visit - 1, L0_visit_random_multiplicative, TRUE),
        new_L0 = random_effect(new_L0, visit - 1, L0_visit_random_additive, FALSE),

        new_v1RM = random_effect(v1RM, visit - 1, v1RM_random_multiplicative, TRUE),
        new_v1RM = random_effect(new_v1RM, visit - 1, v1RM_random_additive, FALSE),

        # Check if v1RM is below zero
        new_v1RM = dplyr::if_else(new_v1RM < 0, 0, new_v1RM)
      )

    # Now, create new profiles for each visit
    visits_list <- purrr::pmap(visits_df, function(...) {
      visit <- list(...)
      new_visit(
        athlete = visit$athlete,
        visit = visit$visit,
        V0 = visit$new_V0,
        V0_rep_drop = visit$V0_rep_drop,
        V0_fatigue_additive = visit$V0_fatigue_additive,
        V0_fatigue_multiplicative = visit$V0_fatigue_multiplicative,
        V0_visit_change_additive = visit$V0_visit_change_additive,
        V0_visit_change_multiplicative = visit$V0_visit_change_multiplicative,
        V0_visit_random_additive = visit$V0_visit_random_additive,
        V0_visit_random_multiplicative = visit$V0_visit_random_multiplicative,
        L0 = visit$new_L0,
        L0_rep_drop = visit$L0_rep_drop,
        L0_fatigue_additive = visit$L0_fatigue_additive,
        L0_fatigue_multiplicative = visit$L0_fatigue_multiplicative,
        L0_visit_change_additive = visit$L0_visit_change_additive,
        L0_visit_change_multiplicative = visit$L0_visit_change_multiplicative,
        L0_visit_random_additive = visit$L0_visit_random_additive,
        L0_visit_random_multiplicative = visit$L0_visit_random_multiplicative,
        v1RM = visit$new_v1RM,
        v1RM_random_additive = visit$v1RM_random_additive,
        v1RM_random_multiplicative = visit$v1RM_random_multiplicative,
        est_RIR_systematic_additive = visit$est_RIR_systematic_additive,
        est_RIR_systematic_multiplicative = visit$est_RIR_systematic_multiplicative,
        est_RIR_random_additive = visit$est_RIR_random_additive,
        est_RIR_random_multiplicative = visit$est_RIR_random_multiplicative,
        biological_variation_additive = visit$biological_variation_additive,
        biological_variation_multiplicative = visit$biological_variation_multiplicative,
        instrumentation_noise_additive = visit$instrumentation_noise_additive,
        instrumentation_noise_multiplicative = visit$instrumentation_noise_multiplicative,
        load_increment = visit$load_increment
      )
    })

    names(visits_list) <- visit

    # Append visits to the profile visits
    profile$visit <- c(profile$visit, visits_list)

    # Return profile
    return(profile)
  })

  # Now return added visits to LEV_profile
  class(LEV_profile) <- "LEV_profile"
  return(LEV_profile)
}
