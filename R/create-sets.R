#' Create Sets
#'
#' Function creates sets, using provided \code{LEV_profile}, \code{loads}, and \code{trials}
#'
#' @param LEV_profile \code{LEV_profile} object, returned by \code{\link{create_visits}} function
#' @param load Numeric vector. Loads are either absolute weight or percentages. See also \code{load_type}
#' @param reps Target number of reps. Default is equal to \code{NA}, implying max reps
#' @param target_est_RIR Target \code{est_RIR}. Set is stopped when reaching this quality threshold. Default is \code{NA}
#' @param target_velocity Target measured velocity. Set is stopped when reaching this quality threshold. Default is \code{NA}
#' @param target_VL Target velocity loss. Set is stopped when reaching this quality threshold. Default is \code{NA}
#' @param target_est_MNR Target \code{est_MNR}. Set is stopped when reaching this quality threshold. Default is \code{NA}
#' @param load_type Type of load calculation. Can be either 'absolute' (default), 'profile 1RM', 'L0',
#'     'visit 1RM', or 'prescription 1RM'
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param failed_reps Should failed-reps be included in the output? Default is \code{FALSE}
#' @param failed_sets Should failed-sets be included in the output? Default is \code{FALSE}
#' @param use_true_velocity When estimating failure, should true or biological (default) velocity be used?
#' @param inter_set_fatigue Should profile inter-set fatigue parameters be utilized? Default is \code{TRUE}
#' @param incremental_est_RIR When calculating \code{est_RIR} for the preceding reps last rep, should
#'      increments be used or instantaneous subjective ratings for each rep? Default is \code{TRUE}
#'
#' @return Object \code{LEV_sets}
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
create_sets <- function(LEV_profile,
                        load,
                        reps = rep(NA, length(load)),
                        target_est_RIR = rep(NA, length(load)),
                        target_velocity = rep(NA, length(load)),
                        target_VL = rep(NA, length(load)),
                        target_est_MNR = rep(NA, length(load)),
                        load_type = "absolute",
                        max_reps = 100,
                        failed_reps = FALSE,
                        failed_sets = FALSE,
                        use_true_velocity = FALSE,
                        inter_set_fatigue = TRUE,
                        incremental_est_RIR = TRUE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  load_increment <- NULL
  load_rounded <- NULL
  visit_1RM <- NULL
  `%1RM` <- NULL
  `nRM` <- NULL
  `failed_rep` <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Check the loading type
  if ((length(load_type) != 1) | !(load_type %in% c("absolute", "profile 1RM", "L0", "visit 1RM", "prescription 1RM"))) {
    stop("Wrong loading type. Please use 'absolute', 'profile 1RM', 'L0', 'visit 1RM', or 'prescription 1RM'", call. = FALSE)
  }

  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(LEV_profile, stop_running = TRUE)

  # Cycle through each athlete in the LEV_profile
  sets_df <- purrr::map_df(LEV_profile, function(profile) {

    # Cycle through visits
    individual_sets <- purrr::map_df(profile$visit, function(visit) {
      visit_1RM <- visit$visit_1RM
      prescription_1RM <- visit$prescription_1RM
      load_increment <- visit$load_increment

      # Create visit loads
      if (load_type == "absolute") {
        visit_load <- load
        load_perc <- NA
        load_perc_adj <- NA
      } else if (load_type == "profile 1RM") {
        visit_load <- get_load_rounded(load * visit$`1RM`, load_increment)
        load_perc <- load
        load_perc_adj <- visit_load / visit$`1RM`
      } else if (load_type == "L0") {
        visit_load <- get_load_rounded(load * visit$L0, load_increment)
        load_perc <- load
        load_perc_adj <- visit_load / visit$L0
      } else if (load_type == "visit 1RM") {
        # visit 1RM
        visit_load <- get_load_rounded(load * visit_1RM, load_increment)
        load_perc <- load
        load_perc_adj <- visit_load / visit_1RM

        # Check if there are NAs due to missing visit_1RM metric
        if (any(is.na(visit_load))) {
          stop("There are missing values in the load due to missing visit 1RM. Please use 'create_visit_1RM()' before 'create_sets()'",
            call. = FALSE
          )
        }
      } else if (load_type == "prescription 1RM") {
        # prescription 1RM
        visit_load <- get_load_rounded(load * prescription_1RM, load_increment)
        load_perc <- load
        load_perc_adj <- visit_load / prescription_1RM

        # Check if there are NAs due to missing prescription_1RM metric
        if (any(is.na(visit_load))) {
          stop("There are missing values in the load due to missing prescription 1RM. Please use 'create_prescription_1RM()' before 'create_sets()'",
            call. = FALSE
          )
        }
      }

      # Generate sets
      visit_sets <- get_sets(
        visit,
        load = visit_load,
        reps = reps,
        target_est_RIR = target_est_RIR,
        target_velocity = target_velocity,
        target_VL = target_VL,
        target_est_MNR = target_est_MNR,
        max_reps = max_reps,
        use_true_velocity = use_true_velocity,
        inter_set_fatigue = inter_set_fatigue,
        incremental_est_RIR = incremental_est_RIR
      )

      visit_sets$load_type <- load_type
      visit_sets$load_perc <- load_perc[visit_sets$load_index]
      visit_sets$load_perc_adj <- load_perc_adj[visit_sets$load_index]

      # Filter out failed reps
      if (failed_reps == FALSE) {
        visit_sets <- filter_missing_reps(visit_sets, failed_sets = failed_sets)
      }

      return(visit_sets)
    })
    return(individual_sets)
  })

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
    load_type = sets_df$load_type,
    load_index = sets_df$load_index,
    load_perc = sets_df$load_perc,
    load_perc_adj = sets_df$load_perc_adj,
    load = sets_df$load,
    set_V0 = sets_df$set_V0,
    set_L0 = sets_df$set_L0,
    set_1RM = sets_df$set_1RM,
    nRM = sets_df$nRM,
    nRM_frac = sets_df$nRM_frac,
    target_reps = sets_df$target_reps,
    reps_done = sets_df$reps_done,
    set_to_failure = sets_df$set_to_failure,
    rep = sets_df$rep,
    rep_V0 = sets_df$rep_V0,
    rep_L0 = sets_df$rep_L0,
    rep_1RM = sets_df$rep_1RM,
    failed_rep = sets_df$failed_rep,
    true_rep_velocity = sets_df$true_rep_velocity,
    manifested_rep_velocity = sets_df$manifested_rep_velocity,
    measured_rep_velocity = sets_df$measured_rep_velocity,
    RIR = sets_df$RIR,
    `%MNR` = sets_df$`%MNR`,
    best_measured_rep_velocity = sets_df$best_measured_rep_velocity,
    worst_measured_rep_velocity = sets_df$worst_measured_rep_velocity,
    VL = sets_df$VL,
    `%VL` = sets_df$`%VL`,
    VR = sets_df$VR,
    est_RIR = sets_df$est_RIR,
    `est_%MNR` = sets_df$`est_%MNR`,
    est_nRM = sets_df$est_nRM
  )

  return(sets)
}
