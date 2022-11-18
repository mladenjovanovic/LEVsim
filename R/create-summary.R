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
  RIR <- NULL
  reps <- NULL
  `%MNR` <- NULL
  true_rep_velocity <- NULL
  biological_rep_velocity <- NULL
  measured_rep_velocity <- NULL
  best_measured_rep_velocity <- NULL
  VL <- NULL
  `%VL` <- NULL
  VR <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  is_sets <- validate_LEV_sets(LEV_sets)

  sets <- as.data.frame(LEV_sets)

  df <- sets %>%
    dplyr::group_by(athlete, visit, set) %>%
    dplyr::summarise(
      load_index = load_index[[1]],
      V0 = V0[[1]],
      L0 = L0[[1]],
      v1RM = v1RM[[1]],
      `1RM` = `1RM`[[1]],
      visit_1RM = visit_1RM[[1]],
      prescription_1RM = prescription_1RM[[1]],
      load = load[[1]],
      RTF = RTF[[1]],
      reps = max(rep),
      RIR = RIR[[reps]],
      `%MNR` = `%MNR`[[reps]],
      first_true_rep_velocity = true_rep_velocity[[1]],
      last_true_rep_velocity = true_rep_velocity[[reps]],
      first_biological_rep_velocity = biological_rep_velocity[[1]],
      last_biological_rep_velocity = biological_rep_velocity[[reps]],
      first_measured_rep_velocity = measured_rep_velocity[[1]],
      last_measured_rep_velocity = measured_rep_velocity[[reps]],
      best_measured_rep_velocity = best_measured_rep_velocity[[1]],
      VL = VL[[reps]],
      `%VL` = `%VL`[[reps]],
      VR = VR[[reps]]
    )

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
    load_index = df$load_index,
    load = df$load,
    RTF = df$RTF,
    reps = df$reps,
    RIR = df$RIR,
    `%MNR` = df$`%MNR`,
    first_true_rep_velocity = df$first_true_rep_velocity,
    last_true_rep_velocity = df$last_true_rep_velocity,
    first_biological_rep_velocity = df$first_biological_rep_velocity,
    last_biological_rep_velocity = df$last_biological_rep_velocity,
    first_measured_rep_velocity = df$first_measured_rep_velocity,
    last_measured_rep_velocity = df$last_measured_rep_velocity,
    best_measured_rep_velocity = df$best_measured_rep_velocity,
    VL = df$VL,
    `%VL` = df$`%VL`,
    VR = df$VR
  )
}
