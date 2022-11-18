# New Sets Summary Constructor
#
# This function creates \code{LEV_summary} object
new_summary <- function(athlete,
                        visit,
                        V0,
                        L0,
                        v1RM,
                        `1RM`,
                        visit_1RM,
                        prescription_1RM,
                        set,
                        load_index,
                        load,
                        RTF,
                        reps,
                        RIR,
                        `%MNR`,
                        first_true_rep_velocity,
                        last_true_rep_velocity,
                        first_biological_rep_velocity,
                        last_biological_rep_velocity,
                        first_measured_rep_velocity,
                        last_measured_rep_velocity,
                        best_measured_rep_velocity,
                        VL,
                        `%VL`,
                        VR) {
  summary <- dplyr::tibble(
    athlete = athlete,
    visit = visit,
    V0 = V0,
    L0 = L0,
    v1RM = v1RM,
    `1RM` = `1RM`,
    visit_1RM = visit_1RM,
    prescription_1RM = prescription_1RM,
    set = set,
    load_index = load_index,
    load = load,
    RTF = RTF,
    reps = reps,
    RIR = RIR,
    `%MNR` = `%MNR`,
    first_true_rep_velocity = first_true_rep_velocity,
    last_true_rep_velocity = last_true_rep_velocity,
    first_biological_rep_velocity = first_biological_rep_velocity,
    last_biological_rep_velocity = last_biological_rep_velocity,
    first_measured_rep_velocity = first_measured_rep_velocity,
    last_measured_rep_velocity = last_measured_rep_velocity,
    best_measured_rep_velocity = best_measured_rep_velocity,
    VL = VL,
    `%VL` = `%VL`,
    VR = VR
  )

  class(summary) <- "LEV_summary"
  return(summary)
}
