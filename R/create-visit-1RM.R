#' Create visit 1RM
#'
#' This function creates visit 1RM within provided \code{LEV_profile}.
#'     It simply tests the progressive load until one cannot make a single rep.
#'     This is done by using \code{load_perc} against theoretical visit profile 1RM
#'
#' @param LEV_profile Object returned from \code{\link{create_visits}}
#' @param load_perc Percentage to be used to calculate loads using theoretical visit profile 1RM
#' @param use_true_velocity When estimating failure, should true or biological (default) velocity be used?
#' @export
#' @examples
#' set.seed(1667)
#' sets <- create_profiles(1, L0_visit_change = 2.5) %>%
#'   create_visits(1:10) %>%
#'   create_visit_1RM() %>%
#'   create_sets(load = 0.8, load_type = "visit 1RM")
#'
#' plot(sets, x_var = "visit", y_var = "load", reps = 1, facet = NULL)
#'
#' plot(sets, x_var = "visit", y_var = "visit_1RM", facet = NULL)
create_visit_1RM <- function(LEV_profile, load_perc = seq(0.6, 1.2, by = 0.025), use_true_velocity = FALSE) {

  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(LEV_profile, stop_running = TRUE)

  # Cycle through each athlete in the LEV_profile
  res <- purrr::map(LEV_profile, function(profile) {
    # Cycle through visits
    profile$visit <- purrr::map(profile$visit, function(visit) {
      load <- get_load_rounded(visit$`1RM` * load_perc, visit$load_increment)
      oneRM_trials <- get_sets(visit, load, max_reps = 1, use_true_velocity = use_true_velocity)

      last_try <- min(which(oneRM_trials$failed_rep)) - 1
      visit$visit_1RM <- load[[last_try]]

      visit
    })

    profile
  })

  class(res) <- "LEV_profile"
  return(res)
}
