#' Create Summary
#'
#' Function creates summary of the sets, using provided \code{LEV_sets}
#' @param LEV_sets \code{LEV_sets} object, returned by \code{\link{create_sets}} function
#' @return \code{LEV_summary} object
#' @export
#' @examples
#' set.seed(1667)
#' sets <- create_athletes(2) %>%
#'   create_visits(1) %>%
#'   create_visit_1RM() %>%
#'   create_sets(load = c(0.7, 0.8, 0.9), load_type = "visit 1RM") %>%
#'   create_summary()
#'
#' sets
#' plot(sets)
create_summary <- function(LEV_sets) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  athlete <- NULL
  visit <- NULL
  V0 <- NULL
  L0 <- NULL
  v1RM <- NULL
  `1RM` <- NULL
  visit_1RM <- NULL
  prescription_1RM <- NULL
  set <- NULL
  load_index <- NULL
  load <- NULL
  RTF <- NULL
  nRM <- NULL
  RIR <- NULL
  reps <- NULL
  `%MNR` <- NULL
  true_rep_velocity <- NULL
  manifested_rep_velocity <- NULL
  measured_rep_velocity <- NULL
  best_measured_rep_velocity <- NULL
  VL <- NULL
  `%VL` <- NULL
  VR <- NULL
  `est_%MNR` <- NULL
  est_RIR <- NULL
  reps_done <- NULL
  set_to_failure <- NULL
  target_reps <- NULL
  est_nRM <- NULL
  load_type <- NULL
  load_perc <- NULL
  load_perc_adj <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  is_sets <- validate_LEV_sets(LEV_sets)

  sets <- as.data.frame(LEV_sets)

  df <- sets %>%
    dplyr::group_by(athlete, visit, set, load_index) %>%
    dplyr::summarise(
      V0 = V0[[1]],
      L0 = L0[[1]],
      v1RM = v1RM[[1]],
      `1RM` = `1RM`[[1]],
      visit_1RM = visit_1RM[[1]],
      prescription_1RM = prescription_1RM[[1]],
      set_to_failure = set_to_failure[[1]],
      target_reps = target_reps[[1]],
      load_type = load_type[[1]],
      load_perc = load_perc[[1]],
      load_perc_adj = load_perc_adj[[1]],
      load = load[[1]],
      nRM = nRM[[1]],
      reps_done = reps_done[[1]],
      RIR = RIR[reps_done],
      `%MNR` = `%MNR`[reps_done],
      est_RIR = est_RIR[reps_done],
      est_nRM = est_nRM[[1]],
      `est_%MNR` = `est_%MNR`[reps_done],
      first_true_rep_velocity = true_rep_velocity[[1]],
      last_true_rep_velocity = true_rep_velocity[reps_done],
      first_manifested_rep_velocity = manifested_rep_velocity[[1]],
      last_manifested_rep_velocity = manifested_rep_velocity[reps_done],
      first_measured_rep_velocity = measured_rep_velocity[[1]],
      last_measured_rep_velocity = measured_rep_velocity[reps_done],
      best_measured_rep_velocity = best_measured_rep_velocity[reps_done],
      VL = VL[reps_done],
      `%VL` = `%VL`[reps_done],
      VR = VR[reps_done]
    )

  # Make sure that the load_indexes are kept
  only_load_index <- sets %>%
    dplyr::group_by(athlete, visit, set, load_index) %>%
    dplyr::summarise(
      athlete = athlete[1],
      visit = visit[1],
      set = set[1],
      load_index = load_index[1]
    ) %>%
    dplyr::ungroup()

  df <- only_load_index %>%
    dplyr::left_join(df, by = c("athlete", "visit", "set", "load_index"))

  # Save as LEV_summary object
  new_summary(
    athlete = df$athlete,
    visit = df$visit,
    V0 = df$V0,
    L0 = df$L0,
    v1RM = df$v1RM,
    `1RM` = df$`1RM`,
    visit_1RM = df$visit_1RM,
    prescription_1RM = df$prescription_1RM,
    set = df$set,
    load_type = df$load_type,
    load_index = df$load_index,
    load_perc = df$load_perc,
    load_perc_adj = df$load_perc_adj,
    set_to_failure = df$set_to_failure,
    target_reps = df$target_reps,
    load = df$load,
    nRM = df$nRM,
    reps_done = df$reps_done,
    RIR = df$RIR,
    `%MNR` = df$`%MNR`,
    est_RIR = df$est_RIR,
    est_nRM = df$est_nRM,
    `est_%MNR` = df$`est_%MNR`,
    first_true_rep_velocity = df$first_true_rep_velocity,
    last_true_rep_velocity = df$last_true_rep_velocity,
    first_manifested_rep_velocity = df$first_manifested_rep_velocity,
    last_manifested_rep_velocity = df$last_manifested_rep_velocity,
    first_measured_rep_velocity = df$first_measured_rep_velocity,
    last_measured_rep_velocity = df$last_measured_rep_velocity,
    best_measured_rep_velocity = df$best_measured_rep_velocity,
    VL = df$VL,
    `%VL` = df$`%VL`,
    VR = df$VR
  )
}
