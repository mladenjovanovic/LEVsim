#' Create Program Sets
#' Function creates sets, using provided \code{LEV_profile}, \code{loads}, and \code{program_df}
#' @param LEV_profile \code{LEV_profile} object, returned by \code{\link{create_visits}} function
#' @param program_df Data frame with strength training program
#' @param visit String indicating the name of the column in \code{program_df} where visit
#'      is located
#' @param load String indicating the name of the column in \code{program_df} where load or perc 1RM
#'      is located
#' @param reps String indicating the name of the column in \code{program_df} where number of target
#'     repetitions is located
#' @param load_type Type of load calculation. Can be either 'absolute' (default), or 'visit 1RM', or
#'      'prescription 1RM'
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param failed_reps Should failed-reps be included in the output? Default is \code{FALSE}
#' @param use_true_velocity When estimating failure, should true or biological (default) velocity be used?
#' @param inter_set_fatigue Should profile inter-set fatigue parameters be utilized? Default is \code{TRUE}
#' @param keep_program_df Should \code{LEV_sets} object be returned or \code{data.frame}
#'
#' @return Object \code{LEV_sets}, or \code{data.frame} depending if \code{keep_program_df} equal to TRUE
#' @export
#' @examples
#' data("strength_training_program")
#'
#' set.seed(10)
#' program_sets <- create_profiles(athletes = 1, L0_visit_change = 1, L0_fatigue = -2.5) %>%
#'   create_visits(1:12) %>%
#'   create_prescription_1RM(buffer = 0.9) %>%
#'   create_program_sets(
#'     program_df = strength_training_program,
#'     visit = "visit",
#'     load = "perc_1RM",
#'     reps = "target_reps",
#'     load_type = "prescription 1RM"
#'   ) %>%
#'   create_summary()
#'
#' plot(program_sets, type = "pooled")
create_program_sets_fast <- function(LEV_profile = create_profiles(),
                                visit,
                                load,
                                reps,
                                load_type = "absolute",
                                max_reps = 100,
                                failed_reps = FALSE,
                                use_true_velocity = FALSE,
                                inter_set_fatigue = TRUE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  # +++++++++++++++++++++++++++++++++++++++++++

  # Check the loading type
  if ((length(load_type) != 1) | !(load_type %in% c("absolute", "visit 1RM", "prescription 1RM"))) {
    stop("Wrong loading type. Please use 'absolute', 'visit 1RM', or 'prescription 1RM'", call. = FALSE)
  }

  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(LEV_profile, stop_running = TRUE)

  # Convert to data frame
  LEV_visits <- as.data.frame(LEV_profile)

  # Check if there are no visits
  if(!("visit" %in% colnames(LEV_visits)))
  {
    stop("There are no visits. Please use 'create_visits()' to create visits", call. = FALSE)
  }

  # Create program DF
  program_df <- data.frame(
    visit = visit,
    load = load,
    target_reps = reps
  ) %>%
    dplyr::group_by(visit) %>%
    mutate(load_index = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Merge together
  LEV_program <- LEV_visits %>%
    dplyr::inner_join(program_df, by = c("visit"))

  # Create visit loads
  if (load_type == "absolute") {
    # Do nothing
  } else if (load_type == "visit 1RM") {
    # visit 1RM
    LEV_program <- LEV_program %>%
      dplyr::mutate(load = get_load_rounded(load * visit_1RM, load_increment))

    # Check if there are NAs due to missing visit_1RM metric
    if (any(is.na(LEV_program$load))) {
      stop("There are missing values in the load due to missing visit 1RM. Please use 'create_visit_1RM()' before 'create_sets()'",
           call. = FALSE
      )
    }
  } else {
    # prescription 1RM
    LEV_program <- LEV_program %>%
      dplyr::mutate(load = get_load_rounded(load * prescription_1RM, load_increment))

    # Check if there are NAs due to missing prescription_1RM metric
    if (any(is.na(LEV_program$load))) {
      stop("There are missing values in the load due to missing prescription 1RM. Please use 'create_prescription_1RM()' before 'create_sets()'",
           call. = FALSE
      )
    }
  }

  program_sets <- get_sets_df(
    LEV_program,
    max_reps = max_reps,
    use_true_velocity = use_true_velocity,
    inter_set_fatigue = inter_set_fatigue
  )

  program_sets_selected <- program_sets[c("athlete", "visit", "load_index", setdiff(names(program_sets), names(LEV_program)))]

  # Filter out failed reps
  if (failed_reps == FALSE) {
    program_sets_selected <- program_sets_selected %>%
      dplyr::filter(
        failed_rep == FALSE
      )
  }

  LEV_program_merged <- LEV_program %>%
    dplyr::left_join(program_sets_selected, by = c("athlete", "visit", "load_index")) %>%
    dplyr::arrange(athlete, visit, load_index, rep)

  # Return sets object
  new_sets(
    athlete = LEV_program_merged$athlete,
    visit = LEV_program_merged$visit,
    V0 = LEV_program_merged$V0,
    L0 = LEV_program_merged$L0,
    v1RM = LEV_program_merged$v1RM,
    `1RM` = LEV_program_merged$`1RM`,
    visit_1RM = LEV_program_merged$visit_1RM,
    prescription_1RM = LEV_program_merged$prescription_1RM,
    load_increment = LEV_program_merged$load_increment,
    set = LEV_program_merged$set,
    load_index = LEV_program_merged$load_index,
    load = LEV_program_merged$load,
    set_V0 = LEV_program_merged$set_V0,
    set_L0 = LEV_program_merged$set_L0,
    set_1RM = LEV_program_merged$set_1RM,
    nRM = LEV_program_merged$nRM,
    target_reps = LEV_program_merged$target_reps,
    reps_done = LEV_program_merged$reps_done,
    set_to_failure = LEV_program_merged$set_to_failure,
    rep = LEV_program_merged$rep,
    rep_V0 = LEV_program_merged$rep_V0,
    rep_L0 = LEV_program_merged$rep_L0,
    rep_1RM = LEV_program_merged$rep_1RM,
    failed_rep = LEV_program_merged$failed_rep,
    true_rep_velocity = LEV_program_merged$true_rep_velocity,
    biological_rep_velocity = LEV_program_merged$biological_rep_velocity,
    measured_rep_velocity = LEV_program_merged$measured_rep_velocity,
    RIR = LEV_program_merged$RIR,
    `%MNR` = LEV_program_merged$`%MNR`,
    best_measured_rep_velocity = LEV_program_merged$best_measured_rep_velocity,
    VL = LEV_program_merged$VL,
    `%VL` = LEV_program_merged$`%VL`,
    VR = LEV_program_merged$VR,
    est_RIR = LEV_program_merged$est_RIR,
    `est_%MNR` = LEV_program_merged$`est_%MNR`,
    est_nRM = LEV_program_merged$est_nRM
  )

}
