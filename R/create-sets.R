#' Create Sets
#'
#' Function creates sets, using provided \code{LEV_profile} \code{loads} and \code{trials}
#'
#' @param LEV_profile \code{LEV_profile} object, returned by \code{\link{create_visits}} function
#' @param load Numeric vector. Loads are either absolute weight or percentages. See also \code{load_type}
#' @param reps Target number of reps. Default is equal to \code{max_reps}
#' @param load_type Type of load calculation. Can be either 'absolute' (default), or 'visit 1RM', or
#'      'prescription 1RM'
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param failed_reps Should failed-reps be included in the output? Default is \code{FALSE}
#' @param use_true_velocity When estimating failure, should true or biological (default) velocity be used?
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
#' sets <- create_profiles(athletes = c("Mladen", "Ivan"), L0 = c(200, 180)) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(100, 120, 140), load_type = "absolute")
#'
#' plot(sets)
#' plot(sets, facet = NULL, x_var = "load")
#' plot(sets, visits = 1, x_var = "RIR")
create_sets <- function(LEV_profile,
                        load,
                        reps = rep(max_reps, length(load)),
                        load_type = "absolute",
                        max_reps = 100,
                        failed_reps = FALSE,
                        use_true_velocity = FALSE) {

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
  if ((length(load_type) != 1) | !(load_type %in% c("absolute", "visit 1RM", "prescription 1RM"))) {
    stop("Wrong loading type. Please use 'absolute', 'visit 1RM', or 'prescription 1RM'", call. = FALSE)
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
      } else if (load_type == "visit 1RM") {
        # visit 1RM
        visit_load <- get_load_rounded(load * visit_1RM, load_increment)

        # Check if there are NAs due to missing visit_1RM metric
        if (any(is.na(visit_load))) {
          stop("There are missing values in the load due to missing visit 1RM. Please use 'create_visit_1RM()' before 'create_sets()'",
            call. = FALSE
          )
        }
      } else {
        # prescription 1RM
        visit_load <- get_load_rounded(load * prescription_1RM, load_increment)

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
        max_reps = max_reps,
        use_true_velocity = use_true_velocity
      )

      # Filter out failed reps
      if (failed_reps == FALSE) {
        visit_sets <- visit_sets %>%
          dplyr::filter(
            failed_rep == FALSE
          )
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
