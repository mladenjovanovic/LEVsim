#' Create Program Sets
#'
#' Function creates sets, using provided \code{LEV_profile}, \code{loads}, and \code{program_df}
#' @param LEV_profile \code{LEV_profile} object, returned by \code{\link{create_visits}} function
#' @param program_df Data frame with strength training program
#' @param visit String indicating the name of the column in \code{program_df} where visit
#'      is located
#' @param load String indicating the name of the column in \code{program_df} where load or perc 1RM
#'      is located
#' @param reps String indicating the name of the column in \code{program_df} where number of target
#'     repetitions is located
#' @param load_type Type of load calculation. Can be either 'absolute' (default), 'profile 1RM', 'L0',
#'     'visit 1RM', or 'prescription 1RM'
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param failed_reps Should failed-reps be included in the output? Default is \code{FALSE}
#' @param failed_sets Should failed-sets be included in the output? Default is \code{FALSE}
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
#' program_sets <- create_profiles(
#'       athlete = 1,
#'       L0_visit_change_additive = 1,
#'       L0_fatigue_additive = 5) %>%
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
create_program_sets <- function(LEV_profile = create_profiles(),
                                program_df,
                                visit,
                                load,
                                reps,
                                load_type = "absolute",
                                max_reps = 100,
                                failed_reps = FALSE,
                                failed_sets = FALSE,
                                use_true_velocity = FALSE,
                                inter_set_fatigue = TRUE,
                                keep_program_df = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  failed_rep <- NULL
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
    individual_sets <- purrr::map_df(profile$visit, function(.visit) {
      visit_1RM <- .visit$visit_1RM
      prescription_1RM <- .visit$prescription_1RM
      load_increment <- .visit$load_increment

      # Filter out visit from the training program
      program_df_visit <- program_df[program_df[visit] == .visit$visit, ]

      program_load <- program_df_visit[[load]]
      program_reps <- program_df_visit[[reps]]

      if (purrr::is_empty(program_load)) {
        return(NULL)
      }

      # Create visit loads
      if (load_type == "absolute") {
        visit_load <- program_load
        load_perc <- NA
        load_perc_adj <- NA
      } else if (load_type == "profile 1RM") {
        visit_load <- get_load_rounded(program_load * visit$`1RM`, load_increment)
        load_perc <- program_load
        load_perc_adj <- visit_load / visit$`1RM`
      } else if (load_type == "L0") {
        visit_load <- get_load_rounded(program_load * visit$L0, load_increment)
        load_perc <- program_load
        load_perc_adj <- visit_load / visit$L0
      } else if (load_type == "visit 1RM") {
        # visit 1RM
        visit_load <- get_load_rounded(program_load * visit_1RM, load_increment)
        load_perc <- program_load
        load_perc_adj <- visit_load / visit_1RM

        # Check if there are NAs due to missing visit_1RM metric
        if (any(is.na(visit_load))) {
          stop("There are missing values in the load due to missing visit 1RM. Please use 'create_visit_1RM()' before 'create_sets()'",
               call. = FALSE
          )
        }
      } else if (load_type == "prescription 1RM") {
        # prescription 1RM
        visit_load <- get_load_rounded(program_load * prescription_1RM, load_increment)
        load_perc <- program_load
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
        .visit,
        load = visit_load,
        reps = program_reps,
        max_reps = max_reps,
        use_true_velocity = use_true_velocity,
        inter_set_fatigue = inter_set_fatigue
      )

      visit_sets$load_type <- load_type
      visit_sets$load_perc <- load_perc[visit_sets$load_index]
      visit_sets$load_perc_adj <- load_perc_adj[visit_sets$load_index]

      # Filter out unnecessary columns
      visit_sets <- new_sets(
        athlete = visit_sets$athlete,
        visit = visit_sets$visit,
        V0 = visit_sets$V0,
        L0 = visit_sets$L0,
        v1RM = visit_sets$v1RM,
        `1RM` = visit_sets$`1RM`,
        visit_1RM = visit_sets$visit_1RM,
        prescription_1RM = visit_sets$prescription_1RM,
        load_increment = visit_sets$load_increment,
        set = visit_sets$set,
        load_type = visit_sets$load_type,
        load_index = visit_sets$load_index,
        load_perc = visit_sets$load_perc,
        load_perc_adj = visit_sets$load_perc_adj,
        load = visit_sets$load,
        set_V0 = visit_sets$set_V0,
        set_L0 = visit_sets$set_L0,
        set_1RM = visit_sets$set_1RM,
        nRM = visit_sets$nRM,
        target_reps = visit_sets$target_reps,
        reps_done = visit_sets$reps_done,
        set_to_failure = visit_sets$set_to_failure,
        rep = visit_sets$rep,
        rep_V0 = visit_sets$rep_V0,
        rep_L0 = visit_sets$rep_L0,
        rep_1RM = visit_sets$rep_1RM,
        failed_rep = visit_sets$failed_rep,
        true_rep_velocity = visit_sets$true_rep_velocity,
        manifested_rep_velocity = visit_sets$manifested_rep_velocity,
        measured_rep_velocity = visit_sets$measured_rep_velocity,
        RIR = visit_sets$RIR,
        `%MNR` = visit_sets$`%MNR`,
        best_measured_rep_velocity = visit_sets$best_measured_rep_velocity,
        VL = visit_sets$VL,
        `%VL` = visit_sets$`%VL`,
        VR = visit_sets$VR,
        est_RIR = visit_sets$est_RIR,
        `est_%MNR` = visit_sets$`est_%MNR`,
        est_nRM = visit_sets$est_nRM
      )

      visit_sets <- as.data.frame(visit_sets)

      # Create index for merging
      program_df_visit$load_index <- factor(seq_along(program_load))

      if (keep_program_df == TRUE) {
        suffix <- c("", ".LEV")
      } else {
        suffix <- c(".program", "")
      }

      # Filter out failed reps
      if (failed_reps == FALSE) {
        visit_sets <- filter_missing_reps(visit_sets, failed_sets = failed_sets)
      }

      visit_sets <- dplyr::left_join(program_df_visit, visit_sets, by = "load_index", suffix = suffix)

      visit_sets
    })

    return(individual_sets)
  })

  if (purrr::is_empty(sets_df)) {
    return()
  }

  if (keep_program_df == FALSE) {
    # Save as LV_sets object
    sets_df <- new_sets(
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
      VL = sets_df$VL,
      `%VL` = sets_df$`%VL`,
      VR = sets_df$VR,
      est_RIR = sets_df$est_RIR,
      `est_%MNR` = sets_df$`est_%MNR`,
      est_nRM = sets_df$est_nRM
    )
  }

  sets_df
}
