#' Create tests
#'
#' @description Estimate individual 1RM, create Load-Velocity and Reps-To-Failure profiles.
#'     The set fatigue effects, set in the individual profiles, are disregarded when
#'     creating LV and RTF profiles
#' @param LEV_profile \code{LEV_profile} object, returned by \code{\link{create_visits}} function
#' @param load_1RM Percentages used to estimate visit 1RM. Percentage are used to calculate loads using theoretical visit profile 1RM
#' @param load_LV Percentages used to estimate LV profile
#' @param load_RTF Percentages used to estimate RTF profile
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param failed_reps Should failed-reps be included in the output? Default is \code{FALSE}
#' @param use_true_velocity When estimating failure, should true or biological (default) velocity be used?
#'
#' @return Object \code{LEV_sets}
#' @export
#' @examples
#' test_sets <- create_profiles(athletes = 1:2, L0_visit_random = 10, L0_fatigue = -10) %>%
#'   create_visits(1) %>%
#'   create_tests() %>%
#'   create_summary()
#'
#' plot(test_sets, set = "LV")
#' plot(test_sets, set = "RTF")
create_tests <- function(LEV_profile = create_profiles(),
                         load_1RM = seq(0.6, 1.2, by = 0.025),
                         load_LV = c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 0.975, 1),
                         load_RTF = c(0.9, 0.8, 0.7),
                         max_reps = 100,
                         failed_reps = FALSE,
                         use_true_velocity = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  athlete <- NULL
  set <- NULL
  visit <- NULL
  load_index <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(LEV_profile, stop_running = TRUE)

  # Estimate visit 1RMs
  visit_1RM <- LEV_profile %>%
    create_visit_1RM(load_perc = load_1RM, use_true_velocity = use_true_velocity)

  # Create Load-Velocity data
  LV_profile <- visit_1RM %>%
    create_sets(
      load = load_1RM,
      max_reps = 1,
      load_type = "visit 1RM",
      use_true_velocity = use_true_velocity,
      failed_reps = failed_reps,
      inter_set_fatigue = FALSE)

  LV_profile$set <- "LV"
  LV_profile <- as.data.frame(LV_profile)

  # Create RTF data

  RTF_profile <- visit_1RM %>%
    create_sets(
      load = load_RTF,
      max_reps = max_reps,
      load_type = "visit 1RM",
      use_true_velocity = use_true_velocity,
      failed_reps = failed_reps,
      inter_set_fatigue = FALSE)

  RTF_profile$set <- "RTF"
  RTF_profile <- as.data.frame(RTF_profile)

  # Bind together
  sets_df <- rbind(LV_profile, RTF_profile) %>%
    dplyr::arrange(athlete, visit, set, load_index)

  # Save as LV_sets object
  sets <- new_sets(
    athlete = sets_df$athlete,
    visit = sets_df$visit,
    V0 = sets_df$V0,
    L0 = sets_df$L0,
    v1RM = sets_df$v1RM,
    `1RM` = sets_df$`1RM`,
    visit_1RM = sets_df$visit_1RM,
    prescription_1RM = sets_df$prescription_1RM,
    load_increment = sets_df$load_increment,
    set = sets_df$set,
    load_index = sets_df$load_index,
    load = sets_df$load,
    set_V0 = sets_df$set_V0,
    set_L0 = sets_df$set_L0,
    set_1RM = sets_df$set_1RM,
    RTF = sets_df$RTF,
    nRM = sets_df$nRM,
    rep = sets_df$rep,
    rep_V0 = sets_df$rep_V0,
    rep_L0 = sets_df$rep_L0,
    rep_1RM = sets_df$rep_1RM,
    failed_rep = sets_df$failed_rep,
    true_rep_velocity = sets_df$true_rep_velocity,
    biological_rep_velocity = sets_df$biological_rep_velocity,
    measured_rep_velocity = sets_df$measured_rep_velocity,
    RIR = sets_df$RIR,
    `%MNR` = sets_df$`%MNR`,
    best_measured_rep_velocity = sets_df$best_measured_rep_velocity,
    VL = sets_df$VL,
    `%VL` = sets_df$`%VL`,
    VR = sets_df$VR
  )

  return(sets)
}
