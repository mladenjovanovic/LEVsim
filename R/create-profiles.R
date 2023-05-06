#' Create Load-Exertion-Velocity Profiles
#'
#' Function creates Load-Exertion-Velocity profiles, which is needed for other functions in the package.
#'     Multiple profiles can be created
#'
#' @param athlete Numeric or character vector indicating different athletes. Can also be names
#' @param V0 Highest velocity when load equals zero
#' @param V0_rep_drop_additive Number indicating fixed amount of \code{V0} dropping
#'     per repetition within set. This parameter models the
#'     exertion-velocity characteristic
#' @param V0_rep_drop_multiplicative Number indicating proportion of \code{V0} dropping
#'     per repetition within set. This parameter models the
#'     exertion-velocity characteristic
#' @param V0_fatigue_additive Number indicating additive systematic change in \code{V0} across multiple
#'     sets, simulating effect of fatigue
#' @param V0_fatigue_multiplicative Number indicating proportional systematic change in \code{V0} across multiple
#'     sets, simulating effect of fatigue
#' @param V0_visit_change_additive Number indicating additive systematic change in \code{V0} per
#'     each visit
#' @param V0_visit_change_multiplicative Number indicating proportional systematic change in \code{V0} per
#'     each visit
#' @param V0_visit_random_additive Number indicating SD of additive random change in \code{V0} for
#'     each visit
#' @param V0_visit_random_multiplicative Number indicating SD of proportional random change in \code{V0} for
#'     each visit
#' @param L0 Highest load when velocity equals zero
#' @param L0_rep_drop_additive Number indicating fixed amount of \code{L0} dropping
#'     per repetition within set. This parameter models the
#'     exertion-velocity characteristic
#' @param L0_rep_drop_multiplicative Number indicating proportion of \code{L0} dropping
#'     per repetition within set. This parameter models the
#'     exertion-velocity characteristic
#' @param L0_fatigue_additive Number indicating additive systematic change in \code{L0} across multiple
#'     sets, simulating effect of fatigue
#' @param L0_fatigue_multiplicative Number indicating proportional systematic change in \code{L0} across multiple
#'     sets, simulating effect of fatigue
#' @param L0_visit_change_additive Number indicating additive systematic change in \code{L0} per
#'     each visit
#' @param L0_visit_change_multiplicative Number indicating proportional systematic change in \code{L0} per
#'     each visit
#' @param L0_visit_random_additive Number indicating SD of additive random change in \code{L0} for
#'     each visit
#' @param L0_visit_random_multiplicative Number indicating SD of proportional random change in \code{L0} for
#'     each visit
#' @param v1RM Velocity at 1RM, or Minimum Velocity Threshold
#' @param v1RM_random_additive Number indicating SD of additive random change in \code{v1RM} for
#'     each visit
#' @param v1RM_random_multiplicative Number indicating SD of proportional random change in \code{v1RM} for
#'     each visit
#' @param est_RIR_systematic_additive Number indicating systematic additive effect of the random
#'     biological variation in estimating \code{RIR}
#' @param est_RIR_systematic_multiplicative Number indicating systematic proportional effect of the random
#'     biological variation in estimating \code{RIR}
#' @param est_RIR_random_additive Number indicating SD of the random additive biological variation in estimating
#'     \code{RIR}
#' @param est_RIR_random_multiplicative Number indicating SD of the random proportional biological variation
#'     in estimating \code{RIR}
#' @param biological_variation_additive Number indicating SD of the random additive biological variation
#'     in velocity when performing repetitions
#' @param biological_variation_multiplicative Number indicating SD of the random proportional biological variation
#'     in velocity when performing repetitions
#' @param instrumentation_noise_additive Number indicating SD of the random additive instrumentation noise when
#'     measuring repetition velocity
#' @param instrumentation_noise_multiplicative Number indicating SD of the random proportional instrumentation noise
#'     when measuring repetition velocity
#' @param load_increment What should be the minimal jump in load? Default is 2.5, common load jump
#'     in kg in resistance training
#'
#' @return Function returns \code{LV_profile} object
#' @export
#' @seealso [create_athletes()] is another option to create LEV profiles
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
create_profiles <- function(athlete = NA,
                            V0 = 1.8,
                            V0_rep_drop_additive = 0,
                            V0_rep_drop_multiplicative = 0,
                            V0_fatigue_additive = 0,
                            V0_fatigue_multiplicative = 0,
                            V0_visit_change_additive = 0,
                            V0_visit_change_multiplicative = 0,
                            V0_visit_random_additive = 0,
                            V0_visit_random_multiplicative = 0,
                            L0 = 180,
                            L0_rep_drop_additive = 0,
                            L0_rep_drop_multiplicative = 0.045,
                            L0_fatigue_additive = 0,
                            L0_fatigue_multiplicative = 0,
                            L0_visit_change_additive = 0,
                            L0_visit_change_multiplicative = 0,
                            L0_visit_random_additive = 0,
                            L0_visit_random_multiplicative = 0,
                            v1RM = 0.2,
                            v1RM_random_additive = 0,
                            v1RM_random_multiplicative = 0,
                            est_RIR_systematic_additive = 0,
                            est_RIR_systematic_multiplicative = 0,
                            est_RIR_random_additive = 0,
                            est_RIR_random_multiplicative = 0,
                            biological_variation_additive = 0,
                            biological_variation_multiplicative = 0,
                            instrumentation_noise_additive = 0,
                            instrumentation_noise_multiplicative = 0,
                            load_increment = 2.5) {

  # Create LEV profiles using constructor
  new_LEV_profile(
    athlete = athlete,
    V0 = V0,
    V0_rep_drop_additive = V0_rep_drop_additive,
    V0_rep_drop_multiplicative = V0_rep_drop_multiplicative,
    V0_fatigue_additive = V0_fatigue_additive,
    V0_fatigue_multiplicative = V0_fatigue_multiplicative,
    V0_visit_change_additive = V0_visit_change_additive,
    V0_visit_change_multiplicative = V0_visit_change_multiplicative,
    V0_visit_random_additive = V0_visit_random_additive,
    V0_visit_random_multiplicative = V0_visit_random_multiplicative,
    L0 = L0,
    L0_rep_drop_additive = L0_rep_drop_additive,
    L0_rep_drop_multiplicative = L0_rep_drop_multiplicative,
    L0_fatigue_additive = L0_fatigue_additive,
    L0_fatigue_multiplicative = L0_fatigue_multiplicative,
    L0_visit_change_additive = L0_visit_change_additive,
    L0_visit_change_multiplicative = L0_visit_change_multiplicative,
    L0_visit_random_additive = L0_visit_random_additive,
    L0_visit_random_multiplicative = L0_visit_random_multiplicative,
    v1RM = v1RM,
    v1RM_random_additive = v1RM_random_additive,
    v1RM_random_multiplicative = v1RM_random_multiplicative,
    est_RIR_systematic_additive = est_RIR_systematic_additive,
    est_RIR_systematic_multiplicative = est_RIR_systematic_multiplicative,
    est_RIR_random_additive = est_RIR_random_additive,
    est_RIR_random_multiplicative = est_RIR_random_multiplicative,
    biological_variation_additive = biological_variation_additive,
    biological_variation_multiplicative = biological_variation_multiplicative,
    instrumentation_noise_additive = instrumentation_noise_additive,
    instrumentation_noise_multiplicative = instrumentation_noise_multiplicative,
    load_increment = load_increment
  )
}
