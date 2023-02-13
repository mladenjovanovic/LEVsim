#' Create Random Athletes
#'
#' This function create random \code{n} athletes with random \code{LEV_profile} parameters. These
#'     parameters are generated using uniform random number between \code{min} and \code{max} parameters.
#'     LEV profiles can be created either using \code{create_athletes} or \code{\link{create_profiles}}
#'      function. For extensive description of parameters see \code{\link{create_profiles}} function.
#'
#' @seealso [create_profiles()] function generates LEV profiles, but provide more control of the parameters
#'
#' @param n Number of athletes
#' @param V0_min Numeric
#' @param V0_max Numeric
#' @param V0_rep_drop_min Numeric
#' @param V0_rep_drop_max Numeric
#' @param V0_visit_change_min Numeric
#' @param V0_visit_change_max Numeric
#' @param V0_visit_change_multiplicative Logical
#' @param V0_visit_random_min Numeric
#' @param V0_visit_random_max Numeric
#' @param V0_visit_random_multiplicative Logical
#' @param L0_min Numeric
#' @param L0_max Numeric
#' @param L0_rep_drop_min Numeric
#' @param L0_rep_drop_max Numeric
#' @param L0_visit_change_min Numeric
#' @param L0_visit_change_max Numeric
#' @param L0_visit_change_multiplicative Logical
#' @param L0_visit_random_min Numeric
#' @param L0_visit_random_max Numeric
#' @param L0_visit_random_multiplicative Logical
#' @param v1RM_min Numeric
#' @param v1RM_max Numeric
#' @param v1RM_random_min Numeric
#' @param v1RM_random_max Numeric
#' @param v1RM_random_multiplicative Logical
#' @param est_RIR_random_min Numeric
#' @param est_RIR_random_max Numeric
#' @param est_RIR_random_multiplicative Logical
#' @param biological_variation_min Numeric
#' @param biological_variation_max Numeric
#' @param biological_variation_multiplicative Logical
#' @param instrumentation_noise_min Numeric
#' @param instrumentation_noise_max Numeric
#' @param instrumentation_noise_multiplicative Logical
#' @param load_increment Numeric
#'
#' @return Function returns \code{LV_profile} object
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
create_athletes <- function(n = 20,
                            V0_min = 1.5,
                            V0_max = 2.5,
                            V0_rep_drop_min = 0,
                            V0_rep_drop_max = 0.1,
                            V0_visit_change_min = -0.05,
                            V0_visit_change_max = 0.05,
                            V0_visit_change_multiplicative = FALSE,
                            V0_visit_random_min = 0.01,
                            V0_visit_random_max = 0.05,
                            V0_visit_random_multiplicative = FALSE,
                            L0_min = 180,
                            L0_max = 280,
                            L0_rep_drop_min = 0.03,
                            L0_rep_drop_max = 0.1,
                            L0_visit_change_min = -5,
                            L0_visit_change_max = 5,
                            L0_visit_change_multiplicative = FALSE,
                            L0_visit_random_min = 0.5,
                            L0_visit_random_max = 5,
                            L0_visit_random_multiplicative = FALSE,
                            v1RM_min = 0.2,
                            v1RM_max = 0.4,
                            v1RM_random_min = 0,
                            v1RM_random_max = 0.05,
                            v1RM_random_multiplicative = FALSE,
                            est_RIR_random_min = 0,
                            est_RIR_random_max = 0.5,
                            est_RIR_random_multiplicative = TRUE,
                            biological_variation_min = 0.02,
                            biological_variation_max = 0.075,
                            biological_variation_multiplicative = TRUE,
                            instrumentation_noise_min = 0,
                            instrumentation_noise_max = 0,
                            instrumentation_noise_multiplicative = FALSE,
                            load_increment = 2.5) {

  # Call create profiles
  create_profiles(
    athletes = sprintf(paste0("Athlete %0", floor(log10(n)) + 1, "d"), seq(1, n)),
    V0 = stats::runif(n, V0_min, V0_max),
    V0_rep_drop = stats::runif(n, V0_rep_drop_min, V0_rep_drop_max),
    V0_visit_change = stats::runif(n, V0_visit_change_min, V0_visit_change_max),
    V0_visit_change_multiplicative = V0_visit_change_multiplicative,
    V0_visit_random = stats::runif(n, V0_visit_random_min, V0_visit_random_max),
    V0_visit_random_multiplicative = V0_visit_random_multiplicative,
    L0 = stats::runif(n, L0_min, L0_max),
    L0_rep_drop = stats::runif(n, L0_rep_drop_min, L0_rep_drop_max),
    L0_visit_change = stats::runif(n, L0_visit_change_min, L0_visit_change_max),
    L0_visit_change_multiplicative = L0_visit_change_multiplicative,
    L0_visit_random = stats::runif(n, L0_visit_random_min, L0_visit_random_max),
    L0_visit_random_multiplicative = L0_visit_random_multiplicative,
    v1RM = stats::runif(n, v1RM_min, v1RM_max),
    v1RM_random = stats::runif(n, v1RM_random_min, v1RM_random_max),
    v1RM_random_multiplicative = v1RM_random_multiplicative,
    est_RIR_random = stats::runif(n, est_RIR_random_min, est_RIR_random_max),
    est_RIR_random_multiplicative = est_RIR_random_multiplicative,
    biological_variation = stats::runif(n, biological_variation_min, biological_variation_max),
    biological_variation_multiplicative = biological_variation_multiplicative,
    instrumentation_noise = stats::runif(n, instrumentation_noise_min, instrumentation_noise_max),
    instrumentation_noise_multiplicative = instrumentation_noise_multiplicative,
    load_increment = load_increment
  )
}
