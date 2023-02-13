#' Create Load-Exertion-Velocity Profiles
#'
#' Function creates Load-Exertion-Velocity profiles, which is needed for other functions in the package.
#'     Multiple profiles can be created
#'
#' @param athletes Numeric or character vector indicating different athletes. Can also be names
#' @param V0 Highest velocity when load equals zero
#' @param V0_rep_drop Number indicating proportion of \code{V0} dropping
#'     when multiple repetitions are performed. This parameter models the
#'     exertion-velocity characteristics
#' @param V0_fatigue Number indicating systematic change in \code{V0} per
#'     each set
#' @param V0_fatigue_multiplicative Should multiplicative fatigue be used? Default
#'     is \code{FALSE}
#' @param V0_visit_change Number indicating systematic change in \code{V0} per
#'     each visit to the laboratory/gym
#' @param V0_visit_change_multiplicative Should multiplicative change be used? Default
#'     is \code{FALSE}
#' @param V0_visit_random Number indicating SD of random change in \code{V0} for
#'     each visit to the laboratory/gym
#' @param V0_visit_random_multiplicative Should multiplicative random error be used? Default
#'     is \code{FALSE}
#' @param L0 Highest load when velocity equals zero
#' @param L0_rep_drop Number indicating proportion of \code{L0} dropping
#'     when multiple repetitions are performed. This parameter models the
#'     exertion-velocity characteristics
#' @param L0_fatigue Number indicating systematic change in \code{L0} per
#'     each set
#' @param L0_fatigue_multiplicative Should multiplicative fatigue be used? Default
#'     is \code{FALSE}
#' @param L0_visit_change Number indicating systematic change in \code{L0} per
#'     each visit to the laboratory/gym
#' @param L0_visit_change_multiplicative Should multiplicative change be used? Default
#'     is \code{FALSE}
#' @param L0_visit_random Number indicating SD of random change in \code{L0} for
#'     each visit to the laboratory/gym
#' @param L0_visit_random_multiplicative Should multiplicative random error be used? Default
#'     is \code{FALSE}
#' @param v1RM Velocity at 1RM, or Minimum Velocity Threshold
#' @param v1RM_random Number indicating SD of random change in \code{v1RM} for
#'     each visit to the laboratory/gym
#' @param v1RM_random_multiplicative Should multiplicative random error be used? Default
#'     is \code{FALSE}
#' @param est_RIR_random Number indicating SD of the random biological variation in estimating
#'     \code{RIR}
#' @param est_RIR_random_multiplicative Should multiplicative random error be used? Default
#'     is \code{TRUE}
#' @param biological_variation Number indicating SD of the random biological variation in velocity
#'     when performing repetitions
#' @param biological_variation_multiplicative Should multiplicative random error be used? Default
#'     is \code{TRUE}
#' @param instrumentation_noise Number indicating SD of the random instrumentation noise when
#'     measuring repetition velocity
#' @param instrumentation_noise_multiplicative Should multiplicative random error be used? Default
#'     is \code{FALSE}
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
#' sets <- create_profiles(athletes = c("Mladen", "Ivan"), L0 = c(200, 180)) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(100, 120, 140), load_type = "absolute")
#'
#' plot(sets)
#' plot(sets, facet = NULL, x_var = "load")
#' plot(sets, visits = 1, x_var = "RIR")
create_profiles <- function(athletes = NA,
                            V0 = 1.8,
                            V0_rep_drop = 0,
                            V0_fatigue = 0,
                            V0_fatigue_multiplicative = FALSE,
                            V0_visit_change = 0,
                            V0_visit_change_multiplicative = FALSE,
                            V0_visit_random = 0,
                            V0_visit_random_multiplicative = FALSE,
                            L0 = 180,
                            L0_rep_drop = 0.04,
                            L0_fatigue = 0,
                            L0_fatigue_multiplicative = FALSE,
                            L0_visit_change = 0,
                            L0_visit_change_multiplicative = FALSE,
                            L0_visit_random = 0,
                            L0_visit_random_multiplicative = FALSE,
                            v1RM = 0.2,
                            v1RM_random = 0,
                            v1RM_random_multiplicative = FALSE,
                            est_RIR_random = 0,
                            est_RIR_random_multiplicative = TRUE,
                            biological_variation = 0,
                            biological_variation_multiplicative = TRUE,
                            instrumentation_noise = 0,
                            instrumentation_noise_multiplicative = FALSE,
                            load_increment = 2.5) {

  # Create LEV profiles using constructor
  new_LEV_profile(
    athletes = athletes,
    V0 = V0,
    V0_rep_drop = V0_rep_drop,
    V0_fatigue = V0_fatigue,
    V0_fatigue_multiplicative = V0_fatigue_multiplicative,
    V0_visit_change = V0_visit_change,
    V0_visit_change_multiplicative = V0_visit_change_multiplicative,
    V0_visit_random = V0_visit_random,
    V0_visit_random_multiplicative = V0_visit_random_multiplicative,
    L0 = L0,
    L0_rep_drop = L0_rep_drop,
    L0_fatigue = L0_fatigue,
    L0_fatigue_multiplicative = L0_fatigue_multiplicative,
    L0_visit_change = L0_visit_change,
    L0_visit_change_multiplicative = L0_visit_change_multiplicative,
    L0_visit_random = L0_visit_random,
    L0_visit_random_multiplicative = L0_visit_random_multiplicative,
    v1RM = v1RM,
    v1RM_random = v1RM_random,
    v1RM_random_multiplicative = v1RM_random_multiplicative,
    est_RIR_random = est_RIR_random,
    est_RIR_random_multiplicative = est_RIR_random_multiplicative,
    biological_variation = biological_variation,
    biological_variation_multiplicative = biological_variation_multiplicative,
    instrumentation_noise = instrumentation_noise,
    instrumentation_noise_multiplicative = instrumentation_noise_multiplicative,
    load_increment = load_increment
  )
}
